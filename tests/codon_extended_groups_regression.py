import argparse
import json
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


def read_state(page) -> dict:
    return page.evaluate(
        """() => {
            const resultConfig = window.__rnaMetaBridge?.latestControlEvents?.['codon-results_host']?.message?.config || null;
            const exportConfig = window.__rnaMetaBridge?.latestControlEvents?.['codon-export_host']?.message?.config || null;
            const exportShell = document.querySelector('#codon-export_host .ribote-export-shell');

            return {
                activeGroup: document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active')?.textContent?.trim() || null,
                activeViewLabel: document.querySelector('#codon-results_host .ribote-canvas-tab.is-active')?.textContent?.trim() || null,
                resultActiveView: resultConfig?.activeView || null,
                exportShell: Boolean(exportShell),
                exportReady: exportConfig?.ready ?? null,
                exportCurrentView: exportConfig?.currentView || null,
                hostText: document.querySelector('#codon-results_host')?.textContent || ''
            };
        }"""
    )


def wait_for_view(page, expected_group: str, expected_view_id: str, expected_view_label: str, expected_text: str, timeout: int = 30000) -> dict:
    page.wait_for_function(
        """([expectedGroup, expectedViewId, expectedViewLabel, expectedText]) => {
            const resultConfig = window.__rnaMetaBridge?.latestControlEvents?.['codon-results_host']?.message?.config || null;
            const exportConfig = window.__rnaMetaBridge?.latestControlEvents?.['codon-export_host']?.message?.config || null;
            const activeGroup = document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active')?.textContent?.trim() || '';
            const activeViewLabel = document.querySelector('#codon-results_host .ribote-canvas-tab.is-active')?.textContent?.trim() || '';
            const hostText = document.querySelector('#codon-results_host')?.textContent || '';
            const exportShell = document.querySelector('#codon-export_host .ribote-export-shell');

            return activeGroup === expectedGroup &&
                activeViewLabel === expectedViewLabel &&
                resultConfig?.activeView === expectedViewId &&
                exportConfig?.currentView === expectedViewId &&
                exportConfig?.ready === true &&
                Boolean(exportShell) &&
                hostText.includes(expectedText);
        }""",
        arg=[expected_group, expected_view_id, expected_view_label, expected_text],
        timeout=timeout
    )
    return read_state(page)


def assert_stable(page, expected_group: str, expected_view_id: str, expected_view_label: str, expected_text: str, rounds: int = 6) -> None:
    for index in range(rounds):
        state = read_state(page)
        print(f"stable[{index}] => {json.dumps(state, ensure_ascii=True)}")
        assert state["activeGroup"] == expected_group, state
        assert state["activeViewLabel"] == expected_view_label, state
        assert state["resultActiveView"] == expected_view_id, state
        assert state["exportCurrentView"] == expected_view_id, state
        assert state["exportReady"] is True, state
        assert state["exportShell"] is True, state
        assert expected_text in state["hostText"], state
        page.wait_for_timeout(250)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True)
    args = parser.parse_args()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1200})

        prep(page, args.url)

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="TE SHIFT AND ENRICHMENT"), page)
        click_when_enabled(page.get_by_role("button", name="Run TE Shift and Enrichment"), page)
        wait_for_view(
            page,
            expected_group="TE Shift and Enrichment",
            expected_view_id="selected_codon_burden",
            expected_view_label="Selected Codon Burden vs TE Change",
            expected_text="Selected Codon Burden Summary",
            timeout=240000
        )
        click_when_enabled(
            page.locator("#codon-results_host .ribote-canvas-tab").filter(has_text="Selected Codon Load vs TE Effect Score"),
            page
        )
        wait_for_view(
            page,
            expected_group="TE Shift and Enrichment",
            expected_view_id="selected_load_effect",
            expected_view_label="Selected Codon Load vs TE Effect Score",
            expected_text="Selected Codon Load vs TE Effect Score",
            timeout=30000
        )

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="PATTERN VIEWS"), page)
        click_when_enabled(page.get_by_role("button", name="Run Pattern Views"), page)
        wait_for_view(
            page,
            expected_group="Pattern Views",
            expected_view_id="codon_clustering",
            expected_view_label="Codon Co-usage Clustering",
            expected_text="Codon Co-usage Clustering Summary",
            timeout=240000
        )
        click_when_enabled(
            page.locator("#codon-results_host .ribote-canvas-tab").filter(has_text="Codon Usage Z-score Heatmap"),
            page
        )
        wait_for_view(
            page,
            expected_group="Pattern Views",
            expected_view_id="codon_usage_heatmap",
            expected_view_label="Codon Usage Z-score Heatmap",
            expected_text="Codon Usage Heatmap Summary",
            timeout=30000
        )

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON RUNS"), page)
        click_when_enabled(page.get_by_role("button", name="Run Codon Runs"), page)
        wait_for_view(
            page,
            expected_group="Codon Runs",
            expected_view_id="codon_run_zscore",
            expected_view_label="Codon Run Load vs Usage Z-score",
            expected_text="Codon Run Load vs Usage Z-score",
            timeout=360000
        )
        click_when_enabled(
            page.locator("#codon-results_host .ribote-canvas-tab").filter(has_text="Codon Run Enrichment by TE Group"),
            page
        )
        wait_for_view(
            page,
            expected_group="Codon Runs",
            expected_view_id="codon_run_enrichment",
            expected_view_label="Codon Run Enrichment by TE Group",
            expected_text="Codon Run Enrichment by TE Group",
            timeout=30000
        )

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="TE SHIFT AND ENRICHMENT"), page)
        wait_for_view(
            page,
            expected_group="TE Shift and Enrichment",
            expected_view_id="selected_load_effect",
            expected_view_label="Selected Codon Load vs TE Effect Score",
            expected_text="Selected Codon Load vs TE Effect Score",
            timeout=30000
        )
        assert_stable(
            page,
            expected_group="TE Shift and Enrichment",
            expected_view_id="selected_load_effect",
            expected_view_label="Selected Codon Load vs TE Effect Score",
            expected_text="Selected Codon Load vs TE Effect Score"
        )

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="PATTERN VIEWS"), page)
        wait_for_view(
            page,
            expected_group="Pattern Views",
            expected_view_id="codon_usage_heatmap",
            expected_view_label="Codon Usage Z-score Heatmap",
            expected_text="Codon Usage Heatmap Summary",
            timeout=30000
        )
        assert_stable(
            page,
            expected_group="Pattern Views",
            expected_view_id="codon_usage_heatmap",
            expected_view_label="Codon Usage Z-score Heatmap",
            expected_text="Codon Usage Heatmap Summary"
        )

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON RUNS"), page)
        wait_for_view(
            page,
            expected_group="Codon Runs",
            expected_view_id="codon_run_enrichment",
            expected_view_label="Codon Run Enrichment by TE Group",
            expected_text="Codon Run Enrichment by TE Group",
            timeout=30000
        )
        assert_stable(
            page,
            expected_group="Codon Runs",
            expected_view_id="codon_run_enrichment",
            expected_view_label="Codon Run Enrichment by TE Group",
            expected_text="Codon Run Enrichment by TE Group"
        )

        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
