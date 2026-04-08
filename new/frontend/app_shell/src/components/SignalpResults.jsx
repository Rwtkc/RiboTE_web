import { useEffect, useMemo, useRef, useState } from "react";
import { drawSignalpOverviewChart, normalizeSignalpPlotRows } from "./signalpChart";

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
        console.error("SignalP chart rendering failed.", error);
        setRenderError(error instanceof Error ? error.message : "Unknown SignalP chart rendering error.");
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

function formatInteger(value) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) {
    return "NA";
  }

  return numeric.toLocaleString();
}

function formatPercent(value) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) {
    return "NA";
  }

  return `${(numeric * 100).toFixed(2)}%`;
}

function formatPValue(value) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) {
    return "NA";
  }

  return numeric < 1e-3 ? numeric.toExponential(2) : numeric.toFixed(4);
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
    id: String(row?.id || `${row?.method || "method"}-${row?.teGroup || "group"}-${index + 1}`),
    method: String(row?.method || ""),
    methodLabel: String(row?.methodLabel || row?.method || ""),
    teGroup: String(row?.teGroup || ""),
    annotatedCount: Number(row?.annotatedCount),
    totalCount: Number(row?.totalCount),
    percent: Number(row?.percent),
    upVsNonPValue: Number(row?.upVsNonPValue),
    downVsNonPValue: Number(row?.downVsNonPValue)
  }));
}

export default function SignalpResults({ config }) {
  const pageSize = 10;
  const rows = useMemo(() => normalizeRows(config?.table?.rows), [config?.table?.rows]);
  const plotRows = useMemo(() => normalizeSignalpPlotRows(config?.plot?.rows), [config?.plot?.rows]);
  const [currentPage, setCurrentPage] = useState(1);
  const totalPages = Math.max(1, Math.ceil(rows.length / pageSize));
  const shouldPaginate = rows.length > pageSize;
  const pageStart = (currentPage - 1) * pageSize;
  const paginatedRows = rows.slice(pageStart, pageStart + pageSize);
  const pageNumbers = useMemo(() => Array.from({ length: totalPages }, (_, index) => index + 1), [totalPages]);
  const tableSummaryCopy = useMemo(() => {
    const label = config?.methodLabel || "SignalP";
    const comparisons = Number(config?.table?.comparisonCount ?? 0);
    return `${label} | ${comparisons} Fisher comparisons`;
  }, [config?.methodLabel, config?.table?.comparisonCount]);

  const { ref: plotRef, isRendering, renderError } = useD3Chart(
    (element, renderState) => drawSignalpOverviewChart(
      element,
      plotRows,
      {
        title: `${config?.methodLabel || "SignalP"} Overview`,
        subtitle: "Share of annotated genes within each Translation Efficiency group"
      },
      renderState
    ),
    [
      config?.methodLabel,
      plotRows.map((row) => `${row.methodLabel}:${row.teGroup}:${row.annotatedCount}:${row.totalCount}:${row.percent}`).join("|")
    ]
  );

  useEffect(() => {
    setCurrentPage(1);
  }, [rows.length, config?.methodLabel]);

  useEffect(() => {
    if (currentPage > totalPages) {
      setCurrentPage(totalPages);
    }
  }, [currentPage, totalPages]);

  if (!config?.note && rows.length === 0) {
    return null;
  }

  return (
    <div className="ribote-signalp-results">
      {config?.note ? (
        <div className="ribote-result-card">
          <p className="ribote-result-card__copy">{config.note}</p>
        </div>
      ) : null}

      <div className="ribote-signalp-grid">
        <div className="ribote-signalp-panel ribote-signalp-panel--plot">
          {plotRows.length ? (
            <div className="ribote-d3-card">
              {renderError ? (
                <div className="ribote-result-card ribote-result-card--warning">
                  <p className="ribote-result-card__copy">SignalP overview rendering failed: {renderError}</p>
                </div>
              ) : null}
              <div className="ribote-signalp-host">
                <div ref={plotRef} className="ribote-d3-host" />
                {isRendering ? (
                  <div className="ribote-signalp-loading">Rendering SignalP overview...</div>
                ) : null}
              </div>
            </div>
          ) : (
            <EmptyCard title="SignalP Overview" message="Run SignalP to compare TE groups with signal and membrane annotations." />
          )}
        </div>

        <div className="ribote-signalp-panel ribote-signalp-panel--table">
          {rows.length ? (
            <div className="ribote-d3-card">
              <div className="ribote-gsea-table-header">
                <div>
                  <h4 className="ribote-d3-card__title">SignalP Table</h4>
                  <p className="ribote-gsea-table-copy">{tableSummaryCopy}</p>
                </div>
              </div>

              <div className="ribote-gsea-table-wrap">
                <table className="ribote-enrichment-table ribote-signalp-table">
                  <thead>
                    <tr>
                      <th>No.</th>
                      <th>Method</th>
                      <th>TE Group</th>
                      <th>Annotated Genes</th>
                      <th>Total Genes</th>
                      <th>Percentage</th>
                      <th>Up vs Non p</th>
                      <th>Down vs Non p</th>
                    </tr>
                  </thead>
                  <tbody>
                    {paginatedRows.map((row, index) => (
                      <tr key={row.id}>
                        <td>{pageStart + index + 1}</td>
                        <td>{row.methodLabel}</td>
                        <td>{row.teGroup}</td>
                        <td>{formatInteger(row.annotatedCount)}</td>
                        <td>{formatInteger(row.totalCount)}</td>
                        <td>{formatPercent(row.percent)}</td>
                        <td>{formatPValue(row.upVsNonPValue)}</td>
                        <td>{formatPValue(row.downVsNonPValue)}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              {shouldPaginate ? (
                <div className="ribote-table-pagination">
                  <div className="ribote-table-pagination__meta">
                    Showing {pageStart + 1}-{Math.min(pageStart + pageSize, rows.length)} of {rows.length} rows | Page {currentPage} of {totalPages}
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
            <EmptyCard title="SignalP Table" message="Run SignalP to generate the annotation comparison summary." />
          )}
        </div>
      </div>
    </div>
  );
}
