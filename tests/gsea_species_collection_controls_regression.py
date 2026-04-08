import argparse
import json
from pathlib import Path

from playwright.sync_api import sync_playwright


REPO_ROOT = Path(__file__).resolve().parents[1]
RICE_MATRIX = REPO_ROOT / "TEShinyData" / "96_99Y.txt"
RICE_SPECIES = "Oryza sativa (IRGSP 1.0)"

SAMPLE_TYPE_MANIFEST = [
    {"sample_name": "RNA_1196Y_1", "sample_type": "RNA-seq"},
    {"sample_name": "RNA_1196Y_2", "sample_type": "RNA-seq"},
    {"sample_name": "RNA_1199Y_1", "sample_type": "RNA-seq"},
    {"sample_name": "RNA_1199Y_2", "sample_type": "RNA-seq"},
    {"sample_name": "1196Y_1", "sample_type": "Ribo-seq"},
    {"sample_name": "1196Y_2", "sample_type": "Ribo-seq"},
    {"sample_name": "1199Y_1", "sample_type": "Ribo-seq"},
    {"sample_name": "1199Y_2", "sample_type": "Ribo-seq"},
]

PAIR_MANIFEST = [
    {"rna_sample": "RNA_1196Y_1", "ribo_sample": "1196Y_1", "group_role": "Control"},
    {"rna_sample": "RNA_1196Y_2", "ribo_sample": "1196Y_2", "group_role": "Control"},
    {"rna_sample": "RNA_1199Y_1", "ribo_sample": "1199Y_1", "group_role": "Treatment"},
    {"rna_sample": "RNA_1199Y_2", "ribo_sample": "1199Y_2", "group_role": "Treatment"},
]


def select_species(page) -> None:
    combo = page.locator("#load_data-load_data_controls").get_by_role("combobox").first
    combo.click()
    page.get_by_role("option", name=RICE_SPECIES).click()
    page.wait_for_function(
        """([selector, expected]) => {
            const node = document.querySelector(selector);
            return !!node && (node.textContent || '').includes(expected);
        }""",
        arg=["#load_data-load_data_controls", RICE_SPECIES],
        timeout=10000,
    )


def set_saved_rice_context(page) -> None:
    page.wait_for_function("() => window.Shiny && typeof window.Shiny.setInputValue === 'function'", timeout=30000)
    select_species(page)
    page.wait_for_timeout(300)
    page.evaluate(
        """(species) => {
            window.Shiny.setInputValue('load_data-species', species, {priority: 'event'});
        }""",
        RICE_SPECIES,
    )
    page.wait_for_timeout(500)
    page.locator("input[type=file]").set_input_files(str(RICE_MATRIX))
    page.wait_for_timeout(500)
    page.evaluate(
        """([sampleTypeManifest, pairManifest]) => {
            window.Shiny.setInputValue('load_data-sample_type_manifest', JSON.stringify(sampleTypeManifest), {priority: 'event'});
            window.Shiny.setInputValue('load_data-pair_manifest', JSON.stringify(pairManifest), {priority: 'event'});
        }""",
        [SAMPLE_TYPE_MANIFEST, PAIR_MANIFEST],
    )
    page.wait_for_timeout(500)
    page.evaluate(
        """(species) => {
            window.Shiny.setInputValue('load_data-species', species, {priority: 'event'});
        }""",
        RICE_SPECIES,
    )
    page.wait_for_timeout(500)
    page.evaluate("() => window.Shiny.setInputValue('load_data-save_context', String(Date.now()), {priority: 'event'})")
    page.wait_for_selector("text=Sample pairing confirmed", timeout=30000)
    page.wait_for_selector("text=Oryza sativa (IRGSP 1.0)", timeout=10000)


def visible_gsea_collection_options(page):
    page.get_by_role("link", name="GSEA").click()
    page.wait_for_selector("#gsea-controls_host", timeout=30000)
    page.wait_for_function(
        """() => {
            const combo = document.querySelector('#gsea-controls_host [role="combobox"]');
            return !!combo && (combo.textContent || '').includes('GO Biological Process');
        }""",
        timeout=10000,
    )
    combo = page.locator("#gsea-controls_host").get_by_role("combobox").first
    selected = combo.inner_text().strip()
    combo.click(force=True)
    page.wait_for_selector("[role=option]", timeout=10000)
    options = [item.strip() for item in page.get_by_role("option").all_inner_texts()]
    page.keyboard.press("Escape")
    return selected, options


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True)
    args = parser.parse_args()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1200})
        page.goto(args.url, wait_until="networkidle", timeout=60000)
        page.get_by_role("link", name="Load Data").click()
        set_saved_rice_context(page)

        selected, options = visible_gsea_collection_options(page)
        print("GSEA rice selected collection:", selected)
        print("GSEA rice collection options:", json.dumps(options, ensure_ascii=False))

        assert selected == "GO Biological Process", selected
        assert "Hallmark" not in options, options
        assert "Reactome" not in options, options
        assert options == ["GO Biological Process", "GO Molecular Function", "GO Cellular Component", "KEGG"], options
        browser.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
