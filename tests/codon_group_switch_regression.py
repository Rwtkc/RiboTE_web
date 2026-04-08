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


def read_codon_state(page) -> dict:
    return page.evaluate(
        """() => {
            const activeGroupButton = document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active');
            const activeViewButton = document.querySelector('#codon-results_host .ribote-canvas-tab.is-active');
            const shinyInputs = window.Shiny?.shinyapp?.$inputValues || {};

            return {
                datasetGroup: document.querySelector('#codon-sidebar_group_host')?.dataset?.riboteCodonActiveGroup || null,
                activeGroupButton: activeGroupButton ? activeGroupButton.textContent.trim() : null,
                activeViewButton: activeViewButton ? activeViewButton.textContent.trim() : null,
                shinyResultView: shinyInputs['codon-result_view']?.view || shinyInputs['codon-result_view'] || null,
                hostText: (document.querySelector('#codon-results_host')?.textContent || '')
            };
        }"""
    )


def wait_for_codon_state(page, expected_group: str, expected_view: str, expected_text: str, timeout: int = 30000) -> dict:
    page.wait_for_function(
        """([expectedGroup, expectedView, expectedText]) => {
            const activeGroupButton = document.querySelector('#codon-results_host .ribote-canvas-group-tab.is-active');
            const activeViewButton = document.querySelector('#codon-results_host .ribote-canvas-tab.is-active');
            const host = document.querySelector('#codon-results_host');
            const shinyInputs = window.Shiny?.shinyapp?.$inputValues || {};

            const activeGroup = activeGroupButton ? activeGroupButton.textContent.trim() : '';
            const activeView = activeViewButton ? activeViewButton.textContent.trim() : '';
            const hostText = host ? (host.textContent || '') : '';
            const datasetGroup = document.querySelector('#codon-sidebar_group_host')?.dataset?.riboteCodonActiveGroup || '';
            const shinyValue = shinyInputs['codon-result_view'];
            const shinyView = (shinyValue && typeof shinyValue === 'object' && 'view' in shinyValue) ? shinyValue.view : (shinyValue || '');

            return activeGroup === expectedGroup &&
                activeView === expectedView &&
                datasetGroup === expectedGroup &&
                shinyView.length > 0 &&
                hostText.includes(expectedText);
        }""",
        arg=[expected_group, expected_view, expected_text],
        timeout=timeout
    )
    return read_codon_state(page)


def assert_codon_state_stable(page, expected_group: str, expected_view: str, expected_text: str, rounds: int = 8) -> None:
    for index in range(rounds):
        state = read_codon_state(page)
        print(f"state[{index}] => {state}")
        assert state["datasetGroup"] == expected_group, state
        assert state["activeGroupButton"] == expected_group, state
        assert state["activeViewButton"] == expected_view, state
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
        select_demo_codon(page, codon="AAA")

        click_when_enabled(page.get_by_role("button", name="Run Input and Usage"), page)
        page.wait_for_selector("#codon-results_host .ribote-codon-table", timeout=180000)
        click_when_enabled(
            page.locator("#codon-results_host .ribote-canvas-tab").filter(has_text="Selected Codon Usage vs RNA Abundance"),
            page
        )
        state = wait_for_codon_state(
            page,
            expected_group="Input and Usage",
            expected_view="Selected Codon Usage vs RNA Abundance",
            expected_text="Selected Codon vs RNA Summary"
        )
        print("after input subgroup:", state)

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON BIAS"), page)
        wait_for_codon_state(
            page,
            expected_group="Codon Bias",
            expected_view="Codon Bias and Adaptation by TE Group",
            expected_text="Run Codon Bias"
        )
        click_when_enabled(page.get_by_role("button", name="Run Codon Bias"), page)
        page.wait_for_function(
            "() => (document.querySelector('#codon-results_host')?.textContent || '').includes('Codon Bias and Adaptation Summary')",
            timeout=180000
        )
        click_when_enabled(
            page.locator("#codon-results_host .ribote-canvas-tab").filter(has_text="CBI Associations"),
            page
        )
        state = wait_for_codon_state(
            page,
            expected_group="Codon Bias",
            expected_view="CBI Associations",
            expected_text="CBI Association Summary"
        )
        print("after bias subgroup:", state)

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="INPUT AND USAGE"), page)
        wait_for_codon_state(
            page,
            expected_group="Input and Usage",
            expected_view="Selected Codon Usage vs RNA Abundance",
            expected_text="Selected Codon vs RNA Summary"
        )
        assert_codon_state_stable(
            page,
            expected_group="Input and Usage",
            expected_view="Selected Codon Usage vs RNA Abundance",
            expected_text="Selected Codon vs RNA Summary"
        )

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="CODON BIAS"), page)
        wait_for_codon_state(
            page,
            expected_group="Codon Bias",
            expected_view="CBI Associations",
            expected_text="CBI Association Summary"
        )
        assert_codon_state_stable(
            page,
            expected_group="Codon Bias",
            expected_view="CBI Associations",
            expected_text="CBI Association Summary"
        )

        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
