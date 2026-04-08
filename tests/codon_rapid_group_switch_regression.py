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

    click_when_enabled(page.get_by_role("button", name="Run Input and Usage"), page)
    page.wait_for_selector("#codon-results_host .ribote-codon-table", timeout=180000)

    click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON BIAS"), page)
    page.wait_for_timeout(1000)
    click_when_enabled(page.get_by_role("button", name="Run Codon Bias"), page)
    page.wait_for_function(
        "() => (document.querySelector('#codon-results_host')?.textContent || '').includes('Codon Bias and Adaptation Summary')",
        timeout=180000
    )


def read_codon_state(page) -> dict:
    return page.evaluate(
        """() => {
            const activeGroupButton = document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active');
            const activeViewButton = document.querySelector('#codon-results_host .ribote-canvas-tab.is-active');
            const shinyInputs = window.Shiny?.shinyapp?.$inputValues || {};
            const latest = window.__rnaMetaBridge?.latestControlEvents?.['codon-results_host']?.message?.config || null;

            return {
                datasetGroup: document.querySelector('#codon-sidebar_group_host')?.dataset?.riboteCodonActiveGroup || null,
                summaryGroup: document.querySelector('#codon-analysis_summary_shell')?.dataset?.riboteCodonActiveGroup || null,
                activeGroupButton: activeGroupButton ? activeGroupButton.textContent.trim() : null,
                activeViewButton: activeViewButton ? activeViewButton.textContent.trim() : null,
                shinyResultView: shinyInputs['codon-result_view']?.view || shinyInputs['codon-result_view'] || null,
                latestActiveView: latest?.activeView || null,
                latestToken: latest?.activeViewToken || null,
                exportView: window.__rnaMetaBridge?.latestControlEvents?.['codon-export_host']?.message?.config?.currentView || null
            };
        }"""
    )


def wait_for_expected_state(page, expected_group: str, expected_view: str, expected_shiny_view: str, timeout: int = 12000) -> None:
    page.wait_for_function(
        """([expectedGroup, expectedView, expectedShinyView]) => {
            const activeGroupButton = document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active');
            const activeViewButton = document.querySelector('#codon-results_host .ribote-canvas-tab.is-active');
            const shinyInputs = window.Shiny?.shinyapp?.$inputValues || {};
            const latest = window.__rnaMetaBridge?.latestControlEvents?.['codon-results_host']?.message?.config || null;

            const shinyValue = shinyInputs['codon-result_view'];
            const shinyView = (shinyValue && typeof shinyValue === 'object' && 'view' in shinyValue) ? shinyValue.view : (shinyValue || '');

            return (
                (document.querySelector('#codon-sidebar_group_host')?.dataset?.riboteCodonActiveGroup || '') === expectedGroup &&
                (document.querySelector('#codon-analysis_summary_shell')?.dataset?.riboteCodonActiveGroup || '') === expectedGroup &&
                (activeGroupButton ? activeGroupButton.textContent.trim() : '') === expectedGroup &&
                (activeViewButton ? activeViewButton.textContent.trim() : '') === expectedView &&
                shinyView === expectedShinyView &&
                (latest?.activeView || '') === expectedShinyView &&
                (window.__rnaMetaBridge?.latestControlEvents?.['codon-export_host']?.message?.config?.currentView || '') === expectedShinyView
            );
        }""",
        arg=[expected_group, expected_view, expected_shiny_view],
        timeout=timeout
    )


def assert_state_stable(page, expected_group: str, expected_view: str, expected_shiny_view: str, rounds: int = 12) -> None:
    for index in range(rounds):
        state = read_codon_state(page)
        print(f"state[{index}] => {state}")
        assert state["datasetGroup"] == expected_group, state
        assert state["summaryGroup"] == expected_group, state
        assert state["activeGroupButton"] == expected_group, state
        assert state["activeViewButton"] == expected_view, state
        assert state["shinyResultView"] == expected_shiny_view, state
        assert state["latestActiveView"] == expected_shiny_view, state
        assert state["exportView"] == expected_shiny_view, state
        page.wait_for_timeout(250)


def rapid_toggle_groups(page, loops: int, end_group: str) -> None:
    input_button = page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="INPUT AND USAGE")
    bias_button = page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON BIAS")

    for _ in range(loops):
        bias_button.click()
        page.wait_for_timeout(60)
        input_button.click()
        page.wait_for_timeout(60)

    if end_group == "Codon Bias":
        bias_button.click()
    else:
        input_button.click()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True)
    args = parser.parse_args()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1200})

        prep(page, args.url)

        rapid_toggle_groups(page, loops=6, end_group="Input and Usage")
        wait_for_expected_state(
            page,
            expected_group="Input and Usage",
            expected_view="Input Summary",
            expected_shiny_view="input_summary"
        )
        assert_state_stable(
            page,
            expected_group="Input and Usage",
            expected_view="Input Summary",
            expected_shiny_view="input_summary"
        )

        rapid_toggle_groups(page, loops=6, end_group="Codon Bias")
        wait_for_expected_state(
            page,
            expected_group="Codon Bias",
            expected_view="Codon Bias and Adaptation by TE Group",
            expected_shiny_view="cbi_tai_by_group"
        )
        assert_state_stable(
            page,
            expected_group="Codon Bias",
            expected_view="Codon Bias and Adaptation by TE Group",
            expected_shiny_view="cbi_tai_by_group"
        )

        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
