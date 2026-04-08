import { useEffect, useMemo, useRef, useState } from "react";
import { drawEnrichmentOverviewChart, normalizeEnrichmentPlotRows } from "./enrichmentChart";

function useD3Chart(drawChart, deps) {
  const ref = useRef(null);
  const renderedWidthRef = useRef(0);
  const [isRendering, setIsRendering] = useState(false);
  const [renderError, setRenderError] = useState(null);

  useEffect(() => {
    if (!ref.current) {
      return undefined;
    }

    let cleanup;
    let frameId = null;
    const chartRoot = ref.current;

    const renderChart = (reason = "data") => {
      if (!chartRoot) {
        setIsRendering(false);
        return;
      }

      try {
        if (typeof cleanup === "function") {
          cleanup();
        }

        renderedWidthRef.current = Math.round(chartRoot.clientWidth || 0);
        cleanup = drawChart(chartRoot, {
          animate: reason !== "resize",
          reason
        });
        setRenderError(null);
      } catch (error) {
        console.error("Enrichment chart rendering failed.", error);
        setRenderError(error instanceof Error ? error.message : "Unknown Enrichment chart rendering error.");
        if (chartRoot) {
          chartRoot.innerHTML = "";
        }
      } finally {
        setIsRendering(false);
      }
    };

    const scheduleRender = (reason = "data") => {
      setIsRendering(reason !== "resize");

      if (frameId !== null) {
        cancelAnimationFrame(frameId);
      }

      frameId = requestAnimationFrame(() => {
        frameId = null;
        renderChart(reason);
      });
    };

    scheduleRender("initial");

    let observer;
    const handleWindowResize = () => {
      const nextWidth = Math.round(chartRoot.clientWidth || 0);
      if (nextWidth && nextWidth !== renderedWidthRef.current) {
        scheduleRender("resize");
      }
    };

    if (typeof ResizeObserver !== "undefined") {
      observer = new ResizeObserver((entries) => {
        const nextWidth = Math.round(entries[0]?.contentRect?.width || chartRoot.clientWidth || 0);
        if (nextWidth && nextWidth !== renderedWidthRef.current) {
          scheduleRender("resize");
        }
      });
      observer.observe(chartRoot);
    } else {
      window.addEventListener("resize", handleWindowResize);
    }

    return () => {
      if (frameId !== null) {
        cancelAnimationFrame(frameId);
      }

      if (observer) {
        observer.disconnect();
      } else {
        window.removeEventListener("resize", handleWindowResize);
      }

      if (typeof cleanup === "function") {
        cleanup();
      }
    };
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps);

  return { ref, isRendering, renderError };
}

function formatMetric(value, digits = 4) {
  if (!Number.isFinite(Number(value))) {
    return "NA";
  }

  return Number(value).toFixed(digits);
}

function EmptyCard({ title, message }) {
  return (
    <div className="ribote-d3-card">
      <h4 className="ribote-d3-card__title">{title}</h4>
      <p className="ribote-gsea-empty">{message}</p>
    </div>
  );
}

function normalizeRows(rows) {
  if (!Array.isArray(rows)) {
    return [];
  }

  return rows.map((row, index) => ({
    id: String(row?.pathwayId || `term-${index + 1}`),
    group: String(row?.group || ""),
    pathway: String(row?.pathway || ""),
    rawPathway: String(row?.rawPathway || ""),
    url: String(row?.url || ""),
    padj: Number(row?.padj),
    pvalue: Number(row?.pvalue),
    fold: Number(row?.fold),
    overlap: Number(row?.overlap),
    pathwaySize: Number(row?.pathwaySize),
    querySize: Number(row?.querySize)
  }));
}

export default function EnrichmentResults({ config }) {
  const pageSize = 10;
  const rows = useMemo(() => normalizeRows(config?.table?.rows), [config?.table?.rows]);
  const plotRows = useMemo(() => normalizeEnrichmentPlotRows(config?.plot?.rows), [config?.plot?.rows]);
  const [currentPage, setCurrentPage] = useState(1);
  const totalPages = Math.max(1, Math.ceil(rows.length / pageSize));
  const shouldPaginate = rows.length > pageSize;
  const pageStart = (currentPage - 1) * pageSize;
  const paginatedRows = rows.slice(pageStart, pageStart + pageSize);
  const pageNumbers = useMemo(() => Array.from({ length: totalPages }, (_, index) => index + 1), [totalPages]);
  const significantCount = Number(config?.table?.significantCount ?? 0);
  const totalTested = Number(config?.table?.totalTested ?? 0);
  const displayedCount = Number(config?.table?.displayedCount ?? rows.length);
  const tableSummaryCopy = useMemo(() => {
    const collectionLabel = config?.collectionLabel || "Collection";

    if (significantCount > displayedCount && displayedCount > 0) {
      return `${collectionLabel} | displaying ${displayedCount} of ${significantCount} significant terms (${totalTested} tested)`;
    }

    return `${collectionLabel} | ${significantCount} significant terms (${totalTested} tested)`;
  }, [config?.collectionLabel, displayedCount, significantCount, totalTested]);

  const { ref: plotRef, isRendering, renderError } = useD3Chart(
    (element, renderState) => drawEnrichmentOverviewChart(
      element,
      plotRows,
      {
        title: `${config?.collectionLabel || "Collection"} Overview`,
        subtitle: "Signed gene ratio by Up/Down enriched pathways",
        backgroundLabel: config?.plot?.backgroundLabel || ""
      },
      renderState
    ),
    [
      config?.collectionLabel,
      config?.plot?.backgroundLabel,
      plotRows.map((row) => `${row.group}:${row.id}:${row.overlap}:${row.querySize}:${row.pathwaySize}:${row.padj}`).join("|")
    ]
  );

  useEffect(() => {
    setCurrentPage(1);
  }, [rows.length, config?.collectionLabel, displayedCount, significantCount, totalTested]);

  useEffect(() => {
    if (currentPage > totalPages) {
      setCurrentPage(totalPages);
    }
  }, [currentPage, totalPages]);

  if (!config?.note && rows.length === 0) {
    return null;
  }

  return (
    <div className="ribote-enrichment-results">
      {config?.note ? (
        <div className="ribote-result-card ribote-result-card--warning">
          <p className="ribote-result-card__copy">{config.note}</p>
        </div>
      ) : null}

      <div className="ribote-enrichment-grid">
        <div className="ribote-enrichment-panel ribote-enrichment-panel--plot">
          {plotRows.length ? (
            <div className="ribote-d3-card">
              {renderError ? (
                <div className="ribote-result-card ribote-result-card--warning">
                  <p className="ribote-result-card__copy">Enrichment overview rendering failed: {renderError}</p>
                </div>
              ) : null}
              <div className="ribote-enrichment-host">
                <div ref={plotRef} className="ribote-d3-host" />
                {isRendering ? (
                  <div className="ribote-gsea-loading">Rendering enrichment overview...</div>
                ) : null}
              </div>
            </div>
          ) : (
            <EmptyCard title="Enrichment Overview" message="Run Enrichment to summarize Up and Down pathway hits." />
          )}
        </div>

        <div className="ribote-enrichment-panel ribote-enrichment-panel--table">
          {rows.length ? (
            <div className="ribote-d3-card">
              <div className="ribote-gsea-table-header">
                <div>
                  <h4 className="ribote-d3-card__title">Enrichment Table</h4>
                  <p className="ribote-gsea-table-copy">{tableSummaryCopy}</p>
                </div>
              </div>

              <div className="ribote-gsea-table-wrap">
                <table className="ribote-enrichment-table">
                  <thead>
                    <tr>
                      <th>No.</th>
                      <th>Group</th>
                      <th>Pathway</th>
                      <th>FDR</th>
                      <th>Fold</th>
                      <th>Overlap</th>
                      <th>Pathway Size</th>
                    </tr>
                  </thead>
                  <tbody>
                    {paginatedRows.map((row, index) => (
                      <tr key={`${row.group}-${row.id}`}>
                        <td>{pageStart + index + 1}</td>
                        <td>{row.group}</td>
                        <td>
                          <div className="ribote-gsea-pathway-name">
                            {row.url ? (
                              <a
                                href={row.url}
                                target="_blank"
                                rel="noreferrer"
                                className="ribote-enrichment-link"
                              >
                                {row.pathway}
                              </a>
                            ) : (
                              row.pathway
                            )}
                          </div>
                          {row.rawPathway ? (
                            <div className="ribote-gsea-pathway-id">{row.rawPathway}</div>
                          ) : null}
                        </td>
                        <td>{formatMetric(row.padj, 4)}</td>
                        <td>{formatMetric(row.fold, 2)}</td>
                        <td>
                          {Number.isFinite(row.overlap) && Number.isFinite(row.querySize)
                            ? `${row.overlap} / ${row.querySize}`
                            : "NA"}
                        </td>
                        <td>{Number.isFinite(row.pathwaySize) ? row.pathwaySize : "NA"}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              {shouldPaginate ? (
                <div className="ribote-table-pagination">
                  <div className="ribote-table-pagination__meta">
                    Showing {pageStart + 1}-{Math.min(pageStart + pageSize, rows.length)} of {rows.length} displayed terms | Page {currentPage} of {totalPages}
                  </div>
                  <div className="ribote-table-pagination__actions">
                    <button
                      type="button"
                      className="ribote-btn ribote-btn--secondary"
                      disabled={currentPage <= 1}
                      onClick={() => setCurrentPage((page) => Math.max(1, page - 1))}
                    >
                      Previous
                    </button>
                    <div className="ribote-table-pagination__pages">
                      {pageNumbers.map((pageNumber) => (
                        <button
                          key={pageNumber}
                          type="button"
                          className={`ribote-btn ribote-btn--secondary ${pageNumber === currentPage ? "is-active" : ""}`}
                          aria-current={pageNumber === currentPage ? "page" : undefined}
                          onClick={() => setCurrentPage(pageNumber)}
                        >
                          {pageNumber}
                        </button>
                      ))}
                    </div>
                    <button
                      type="button"
                      className="ribote-btn ribote-btn--secondary"
                      disabled={currentPage >= totalPages}
                      onClick={() => setCurrentPage((page) => Math.min(totalPages, page + 1))}
                    >
                      Next
                    </button>
                  </div>
                </div>
              ) : null}
            </div>
          ) : (
            <EmptyCard title="Enrichment Table" message="Run Enrichment to populate grouped over-representation terms." />
          )}
        </div>
      </div>
    </div>
  );
}
