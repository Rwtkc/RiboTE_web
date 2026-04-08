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


def read_button_lock_state(page) -> dict:
    return page.evaluate(
        """() => {
            const ids = [
                "codon-run_input_usage",
                "codon-run_te_shift_enrichment",
                "codon-run_pattern_views",
                "codon-run_codon_runs"
            ];

            const buttons = {};
            for (const id of ids) {
                const node = document.getElementById(id);
                buttons[id] = node ? {
                    disabled: Boolean(node.disabled),
                    ariaDisabled: node.getAttribute("aria-disabled"),
                    hasDisabledAttr: node.hasAttribute("disabled")
                } : null;
            }

            return {
                locked: Boolean(window.__rnametaAnalysisLocked),
                owner: window.__rnametaAnalysisOwner || null,
                buttons
            };
        }"""
    )


def assert_all_buttons_disabled(state: dict) -> None:
    assert state["locked"] is True, state
    for button_id, button_state in state["buttons"].items():
        assert button_state is not None, (button_id, state)
        assert button_state["disabled"] is True, (button_id, state)
        assert button_state["ariaDisabled"] == "true", (button_id, state)
        assert button_state["hasDisabledAttr"] is True, (button_id, state)


def wait_for_lock_and_assert_disabled(page, rounds: int = 12, interval_ms: int = 300) -> None:
    page.wait_for_function("() => Boolean(window.__rnametaAnalysisLocked)", timeout=10000)
    for index in range(rounds):
        state = read_button_lock_state(page)
        print(f"lock[{index}] => {state}")
        assert_all_buttons_disabled(state)
        page.wait_for_timeout(interval_ms)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True)
    args = parser.parse_args()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1200})

        prep(page, args.url)

        click_when_enabled(page.get_by_role("button", name="Run Input and Usage"), page)
        wait_for_lock_and_assert_disabled(page)
        page.wait_for_selector("#codon-results_host .ribote-codon-table", timeout=180000)

        click_when_enabled(page.locator("#codon-results_host .ribote-canvas-group-tab").filter(has_text="TE SHIFT AND ENRICHMENT"), page)
        click_when_enabled(page.get_by_role("button", name="Run TE Shift and Enrichment"), page)
        wait_for_lock_and_assert_disabled(page)

        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
