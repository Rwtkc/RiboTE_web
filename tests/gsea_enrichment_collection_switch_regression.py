import argparse
from playwright.sync_api import sync_playwright


def click_when_enabled(locator, page, timeout: int = 60000) -> None:
    locator.wait_for(state="visible", timeout=timeout)
    page.wait_for_function(
        "(element) => !!element && !element.disabled && element.getAttribute('aria-disabled') !== 'true'",
        arg=locator.element_handle(),
        timeout=timeout
    )
    locator.click()


def wait_for_summary_contains(page, host_id: str, expected: str, timeout: int = 180000) -> str:
    page.wait_for_function(
        """([hostSelector, expected]) => {
            const host = document.querySelector(hostSelector);
            if (!host) {
                return false;
            }

            const node = host.querySelector('.ribote-gsea-table-copy');
            return !!node && (node.textContent || '').includes(expected);
        }""",
        arg=[f"#{host_id}", expected],
        timeout=timeout
    )
    matching = page.locator(f"#{host_id} .ribote-gsea-table-copy")
    matching.wait_for(state="visible", timeout=timeout)
    return matching.inner_text()


def prep(page, base_url: str) -> None:
    page.goto(base_url, wait_until="networkidle", timeout=60000)
    page.get_by_role("link", name="Load Data").click()
    page.wait_for_timeout(300)
    page.get_by_role("button", name="Load Demo").click()
    page.wait_for_timeout(2000)

    page.get_by_role("link", name="Data Preprocess").click()
    page.wait_for_timeout(300)
    click_when_enabled(page.get_by_role("button", name="Run Preprocess"), page)
    page.wait_for_selector("text=GENES RETAINED", timeout=30000)

    page.get_by_role("link", name="Translation Efficiency").click()
    page.wait_for_timeout(300)
    click_when_enabled(page.get_by_role("button", name="Run TE Analysis"), page)
    page.wait_for_selector("text=GENES ASSESSED", timeout=180000)


def gsea_collection_combo(page):
    return page.locator("#gsea-controls_host").get_by_role("combobox").first


def enrichment_collection_combo(page):
    return page.locator("#enrichment-controls_host").get_by_role("combobox").first


def set_collection(page, name: str, combo_locator) -> None:
    combo = combo_locator(page)
    combo.scroll_into_view_if_needed()

    option = page.get_by_role("option", name=name)
    for _ in range(2):
        combo.click(force=True)
        try:
            option.wait_for(state="visible", timeout=5000)
            option.click()
            page.wait_for_timeout(300)
            return
        except Exception:
            page.keyboard.press("Escape")
            page.wait_for_timeout(300)

    raise AssertionError(f"Failed to open collection menu option: {name}")


def run_gsea(page) -> str:
    click_when_enabled(page.get_by_role("button", name="Run GSEA"), page)
    page.wait_for_selector("#gsea-results_host .ribote-gsea-table-copy", timeout=180000)
    return page.locator("#gsea-results_host .ribote-gsea-table-copy").inner_text()


def run_enrichment(page) -> str:
    click_when_enabled(page.get_by_role("button", name="Run Enrichment"), page)
    page.wait_for_selector("#enrichment-results_host .ribote-gsea-table-copy", timeout=180000)
    return page.locator("#enrichment-results_host .ribote-gsea-table-copy").inner_text()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True)
    args = parser.parse_args()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1200})

        prep(page, args.url)

        page.get_by_role("link", name="GSEA").click()
        page.wait_for_timeout(1000)
        for select_name, expected in [
            ("Hallmark", "Hallmark"),
            ("GO Biological Process", "GO Biological Process"),
            ("Hallmark", "Hallmark")
        ]:
            set_collection(page, select_name, gsea_collection_combo)
            run_gsea(page)
            summary = wait_for_summary_contains(page, "gsea-results_host", expected)
            print("GSEA", select_name, "=>", summary)
            assert expected in summary, f"GSEA summary mismatch for {select_name}: {summary}"

        page.get_by_role("link", name="Enrichment").click()
        page.wait_for_timeout(1000)
        for select_name, expected in [
            ("GO Biological Process", "GO Biological Process"),
            ("GO Molecular Function", "GO Molecular Function"),
            ("GO Biological Process", "GO Biological Process")
        ]:
            set_collection(page, select_name, enrichment_collection_combo)
            run_enrichment(page)
            summary = wait_for_summary_contains(page, "enrichment-results_host", expected)
            print("ENRICHMENT", select_name, "=>", summary)
            assert expected in summary, f"Enrichment summary mismatch for {select_name}: {summary}"

        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
