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


def select_demo_codon(page, codon: str = "AAA") -> None:
    page.get_by_role("link", name="Codon").click()
    page.wait_for_timeout(1000)

    click_when_enabled(page.locator("#codon-controls_host button").filter(has_text="Choose Codons"), page)
    click_when_enabled(page.locator(".ribote-codon-picker__grid button").filter(has_text=codon).first, page)
    click_when_enabled(page.locator(".ribote-sample-modal__close"), page)
    page.wait_for_timeout(500)


def read_selected_codons(page):
    return page.evaluate(
        """() => {
            const shinyInputs = window.Shiny?.shinyapp?.$inputValues || {};
            return shinyInputs['codon-codon_select:shiny.json'] || shinyInputs['codon-codon_select'] || [];
        }"""
    )


def install_summary_observer(page) -> None:
    page.evaluate(
        """() => {
            const host = document.querySelector('#codon-analysis_summary');
            const controlsHost = document.querySelector('#codon-controls_host');
            window.__riboteCodonSummaryMutationCount = 0;
            window.__riboteCodonSummaryChildListMutationCount = 0;
            window.__riboteCodonSummaryChildCounts = [host ? host.childElementCount : -1];
            window.__riboteCodonControlsChildListMutationCount = 0;
            window.__riboteCodonControlsChildCounts = [controlsHost ? controlsHost.childElementCount : -1];

            if (window.__riboteCodonSummaryObserver) {
                window.__riboteCodonSummaryObserver.disconnect();
                window.__riboteCodonSummaryObserver = null;
            }

            if (window.__riboteCodonControlsObserver) {
                window.__riboteCodonControlsObserver.disconnect();
                window.__riboteCodonControlsObserver = null;
            }

            if (!host || !controlsHost) {
                return;
            }

            window.__riboteCodonSummaryObserver = new MutationObserver((mutations) => {
                window.__riboteCodonSummaryMutationCount += mutations.length;
                window.__riboteCodonSummaryChildListMutationCount += mutations.filter((mutation) => mutation.type === 'childList').length;
                window.__riboteCodonSummaryChildCounts.push(host.childElementCount);
            });

            window.__riboteCodonSummaryObserver.observe(host, {
                childList: True,
                subtree: True,
                characterData: True
            });

            window.__riboteCodonControlsObserver = new MutationObserver((mutations) => {
                window.__riboteCodonControlsChildListMutationCount += mutations.filter((mutation) => mutation.type === 'childList').length;
                window.__riboteCodonControlsChildCounts.push(controlsHost.childElementCount);
            });

            window.__riboteCodonControlsObserver.observe(controlsHost, {
                childList: True,
                subtree: True
            });
        }""".replace("True", "true")
    )


def reset_summary_mutation_count(page) -> None:
    page.evaluate(
        """() => {
            const host = document.querySelector('#codon-analysis_summary');
            const controlsHost = document.querySelector('#codon-controls_host');
            window.__riboteCodonSummaryMutationCount = 0;
            window.__riboteCodonSummaryChildListMutationCount = 0;
            window.__riboteCodonSummaryChildCounts = [host ? host.childElementCount : -1];
            window.__riboteCodonControlsChildListMutationCount = 0;
            window.__riboteCodonControlsChildCounts = [controlsHost ? controlsHost.childElementCount : -1];
        }"""
    )


def read_summary_mutation_count(page) -> int:
    return int(page.evaluate("window.__riboteCodonSummaryMutationCount || 0"))


def read_summary_childlist_mutation_count(page) -> int:
    return int(page.evaluate("window.__riboteCodonSummaryChildListMutationCount || 0"))


def read_summary_child_counts(page):
    return page.evaluate("window.__riboteCodonSummaryChildCounts || []")


def read_controls_childlist_mutation_count(page) -> int:
    return int(page.evaluate("window.__riboteCodonControlsChildListMutationCount || 0"))


def read_controls_child_counts(page):
    return page.evaluate("window.__riboteCodonControlsChildCounts || []")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True)
    args = parser.parse_args()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1200})

        prep(page, args.url)
        select_demo_codon(page, codon="AAA")

        before_switch = read_selected_codons(page)
        print("selected codons before group switch:", before_switch)
        assert before_switch == ["AAA"], before_switch

        click_when_enabled(page.get_by_role("button", name="Run Input and Usage"), page)
        page.wait_for_selector("#codon-results_host .ribote-codon-table", timeout=180000)

        install_summary_observer(page)
        reset_summary_mutation_count(page)
        click_when_enabled(
            page.locator("#codon-results_host .ribote-canvas-tab").filter(has_text="Selected Codon Usage by TE Group"),
            page
        )
        page.wait_for_timeout(1000)

        summary_mutations = read_summary_mutation_count(page)
        print("summary mutations after same-group subtab switch:", summary_mutations)
        assert summary_mutations == 0, summary_mutations

        reset_summary_mutation_count(page)
        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON BIAS"), page)
        page.wait_for_timeout(1000)
        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="INPUT AND USAGE"), page)
        page.wait_for_timeout(1000)

        summary_childlist_mutations = read_summary_childlist_mutation_count(page)
        summary_child_counts = read_summary_child_counts(page)
        controls_childlist_mutations = read_controls_childlist_mutation_count(page)
        controls_child_counts = read_controls_child_counts(page)
        print("summary childList mutations after group switch:", summary_childlist_mutations)
        print("summary child counts after group switch:", summary_child_counts)
        print("controls childList mutations after group switch:", controls_childlist_mutations)
        print("controls child counts after group switch:", controls_child_counts)
        assert summary_childlist_mutations == 0, summary_childlist_mutations
        assert all(count == 1 for count in summary_child_counts), summary_child_counts
        assert controls_childlist_mutations == 0, controls_childlist_mutations
        assert all(count == 1 for count in controls_child_counts), controls_child_counts

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON BIAS"), page)
        page.wait_for_timeout(1000)
        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="INPUT AND USAGE"), page)
        page.wait_for_timeout(1000)

        after_switch = read_selected_codons(page)
        print("selected codons after group switch:", after_switch)
        assert after_switch == ["AAA"], after_switch

        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
