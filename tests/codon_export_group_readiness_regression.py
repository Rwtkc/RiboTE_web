import argparse
from playwright.sync_api import sync_playwright


def click_when_enabled(locator, page, timeout: int = 180000) -> None:
    locator.wait_for(state="visible", timeout=timeout)
    page.wait_for_function(
        "(element) => !!element && !element.disabled && element.getAttribute('aria-disabled') !== 'true'",
        arg=locator.element_handle(),
        timeout=timeout
    )
    locator.click()


def prep(page, base_url: str) -> None:
    page.goto(base_url, wait_until="networkidle", timeout=60000)
    page.get_by_role("link", name="Load Data").click()
    page.wait_for_timeout(300)
    page.get_by_role("button", name="Load Demo").click()
    page.wait_for_timeout(2500)

    page.get_by_role("link", name="Data Preprocess").click()
    page.wait_for_timeout(300)
    click_when_enabled(page.get_by_role("button", name="Run Preprocess"), page)
    page.wait_for_selector("text=GENES RETAINED", timeout=180000)

    page.get_by_role("link", name="Translation Efficiency").click()
    page.wait_for_timeout(300)
    click_when_enabled(page.get_by_role("button", name="Run TE Analysis"), page)
    page.wait_for_selector("text=GENES ASSESSED", timeout=180000)

    page.get_by_role("link", name="Codon").click()
    page.wait_for_timeout(1000)
    click_when_enabled(page.locator("#codon-controls_host button").filter(has_text="Choose Codons"), page)
    click_when_enabled(page.locator(".ribote-codon-picker__grid button").filter(has_text="AAA").first, page)
    click_when_enabled(page.locator(".ribote-sample-modal__close"), page)
    page.wait_for_timeout(500)


def read_export_state(page) -> dict:
    return page.evaluate(
        """() => {
            const exportShell = document.querySelector('#codon-export_host .ribote-export-shell');
            const exportConfig = window.__rnaMetaBridge?.latestControlEvents?.['codon-export_host']?.message?.config || null;

            return {
                activeGroup: document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active')?.textContent?.trim() || null,
                activeView: document.querySelector('#codon-results_host .ribote-canvas-tab.is-active')?.textContent?.trim() || null,
                exportExists: Boolean(exportShell),
                exportReadyAttr: exportShell?.dataset?.riboteExportReady || null,
                exportCurrentView: exportConfig?.currentView || null,
                exportDisabled: exportConfig?.figureDisabled ?? null
            };
        }"""
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True)
    args = parser.parse_args()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1200})

        prep(page, args.url)

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON BIAS"), page)
        page.wait_for_timeout(1000)
        click_when_enabled(page.get_by_role("button", name="Run Codon Bias"), page)
        page.wait_for_function(
            "() => (document.querySelector('#codon-results_host')?.textContent || '').includes('Codon Bias and Adaptation Summary')",
            timeout=180000
        )

        after_bias = read_export_state(page)
        print("after bias run:", after_bias)
        assert after_bias["activeGroup"] == "Codon Bias", after_bias
        assert after_bias["exportExists"] is True, after_bias
        assert after_bias["exportReadyAttr"] == "true", after_bias
        assert after_bias["exportCurrentView"] == "cbi_tai_by_group", after_bias

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="INPUT AND USAGE"), page)
        page.wait_for_function(
            """() => {
                const exportShell = document.querySelector('#codon-export_host .ribote-export-shell');
                return (
                    (document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active')?.textContent?.trim() || '') === 'Input and Usage' &&
                    (document.querySelector('#codon-results_host .ribote-canvas-tab.is-active')?.textContent?.trim() || '') === 'Input Summary' &&
                    !exportShell
                );
            }""",
            timeout=2500
        )

        after_switch = read_export_state(page)
        print("after switching to input without usage results:", after_switch)
        assert after_switch["activeGroup"] == "Input and Usage", after_switch
        assert after_switch["activeView"] == "Input Summary", after_switch
        assert after_switch["exportExists"] is False, after_switch

        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
