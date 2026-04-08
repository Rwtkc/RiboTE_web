import { useEffect, useMemo, useRef, useState } from "react";
import { setShinyInputValue } from "../utils/shinyBridge";
import {
  buildGseaPlotFromCatalog,
  drawGseaEnrichmentPlot,
  normalizeGseaPlot,
  normalizeGseaPlotCatalog
} from "./gseaChart";

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
        console.error("GSEA chart rendering failed.", error);
        setRenderError(error instanceof Error ? error.message : "Unknown GSEA chart rendering error.");
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
    pathwayId: String(row?.pathwayId || `pathway-${index + 1}`),
    pathway: String(row?.pathway || ""),
    rawPathway: String(row?.rawPathway || ""),
    direction: String(row?.direction || ""),
    nes: Number(row?.nes),
    padj: Number(row?.padj),
    pvalue: Number(row?.pvalue),
    size: Number(row?.size),
    leadingEdgeSize: Number(row?.leadingEdgeSize),
    significant: Boolean(row?.significant)
  }));
}

export default function GseaResults({ config }) {
  const pageSize = 10;
  const ids = config?.ids || {};
  const rows = useMemo(() => normalizeRows(config?.table?.rows), [config?.table?.rows]);
  const plotCatalog = useMemo(() => normalizeGseaPlotCatalog(config?.plotCatalog), [config?.plotCatalog]);
  const plot = useMemo(() => {
    const normalized = normalizeGseaPlot(config?.plot);

    if (!normalized.metricPoints.length && plotCatalog.metricPoints.length) {
      return {
        ...normalized,
        metricPoints: plotCatalog.metricPoints
      };
    }

    return normalized;
  }, [config?.plot, plotCatalog]);
  const tableSignature = useMemo(
    () => [
      config?.collectionLabel || "",
      config?.table?.totalTested ?? 0,
      config?.table?.significantCount ?? 0,
      rows.map((row) => row.pathwayId).join("|")
    ].join("::"),
    [config?.collectionLabel, config?.table?.totalTested, config?.table?.significantCount, rows]
  );
  const [selectedPathway, setSelectedPathway] = useState(config?.table?.selectedPathway || "");
  const [currentPage, setCurrentPage] = useState(1);
  const [displayPlot, setDisplayPlot] = useState(plot);
  const [isSwitchingPathway, setIsSwitchingPathway] = useState(false);
  const plotCacheRef = useRef(new Map());
  const tableSignatureRef = useRef(tableSignature);
  const significantCount = Number(config?.table?.significantCount ?? 0);
  const totalTested = Number(config?.table?.totalTested ?? 0);
  const displayedCount = Number(config?.table?.displayedCount ?? rows.length);
  const fdrCutoff = Number(config?.table?.fdrCutoff);
  const isEmptyState = !config?.note && rows.length === 0 && plot.points.length === 0;
  const totalPages = Math.max(1, Math.ceil(rows.length / pageSize));
  const pageStart = (currentPage - 1) * pageSize;
  const paginatedRows = rows.slice(pageStart, pageStart + pageSize);
  const shouldPaginate = rows.length > pageSize;
  const pageNumbers = useMemo(() => Array.from({ length: totalPages }, (_, index) => index + 1), [totalPages]);
  const derivedPlot = useMemo(() => {
    if (plot.points.length) {
      return plot;
    }

    const targetPathwayId = selectedPathway || config?.table?.selectedPathway || rows[0]?.pathwayId || "";
    const targetRow = rows.find((row) => row.pathwayId === targetPathwayId) || rows[0];

    if (!targetRow) {
      return plot;
    }

    const localPlot = buildGseaPlotFromCatalog({
      row: targetRow,
      collectionLabel: config?.collectionLabel || "",
      catalog: plotCatalog
    });

    return localPlot || plot;
  }, [config?.collectionLabel, config?.table?.selectedPathway, plot, plotCatalog, rows, selectedPathway]);
  const tableSummaryCopy = useMemo(() => {
    const collectionLabel = config?.collectionLabel || "Gene Set Collection";
    const fdrLabel = Number.isFinite(fdrCutoff) ? formatMetric(fdrCutoff, 2) : "current cutoff";

    if (significantCount > displayedCount && displayedCount > 0) {
      return `${collectionLabel} | displaying top ${displayedCount} of ${significantCount} pathways that passed FDR <= ${fdrLabel} (${totalTested} tested)`;
    }

    return `${collectionLabel} | ${significantCount} pathways passed FDR <= ${fdrLabel} (${totalTested} tested)`;
  }, [config?.collectionLabel, displayedCount, fdrCutoff, significantCount, totalTested]);

  useEffect(() => {
    setSelectedPathway(config?.table?.selectedPathway || "");
  }, [config?.table?.selectedPathway]);

  useEffect(() => {
    if (tableSignatureRef.current !== tableSignature) {
      tableSignatureRef.current = tableSignature;
      plotCacheRef.current = new Map();
      setCurrentPage(1);
    }
  }, [tableSignature]);

  useEffect(() => {
    if (currentPage > totalPages) {
      setCurrentPage(totalPages);
    }
  }, [currentPage, totalPages]);

  useEffect(() => {
    if (!selectedPathway) {
      return;
    }

    const selectedIndex = rows.findIndex((row) => row.pathwayId === selectedPathway);
    if (selectedIndex < 0) {
      return;
    }

    const nextPage = Math.floor(selectedIndex / pageSize) + 1;
    if (nextPage !== currentPage) {
      setCurrentPage(nextPage);
    }
  }, [selectedPathway, rows]);

  useEffect(() => {
    if (derivedPlot?.pathwayId && derivedPlot.points.length) {
      plotCacheRef.current.set(derivedPlot.pathwayId, derivedPlot);
    }

    setDisplayPlot(derivedPlot);
    setIsSwitchingPathway(false);
  }, [derivedPlot]);

  const { ref: plotRef, isRendering, renderError } = useD3Chart(
    (element, renderState) => drawGseaEnrichmentPlot(element, displayPlot, renderState),
    [displayPlot.pathwayId]
  );
  const hasDisplayPlot = Boolean(displayPlot?.points?.length);

  if (isEmptyState) {
    return null;
  }

  const handleRowClick = (pathwayId) => {
    if (!pathwayId || pathwayId === selectedPathway) {
      return;
    }

    setSelectedPathway(pathwayId);
    const cachedPlot = plotCacheRef.current.get(pathwayId);

    if (cachedPlot?.points?.length) {
      setDisplayPlot(cachedPlot);
      setIsSwitchingPathway(false);
      return;
    }

    const nextRow = rows.find((row) => row.pathwayId === pathwayId);
    const localPlot = buildGseaPlotFromCatalog({
      row: nextRow,
      collectionLabel: config?.collectionLabel || "",
      catalog: plotCatalog
    });

    if (localPlot?.points?.length) {
      plotCacheRef.current.set(pathwayId, localPlot);
      setDisplayPlot(localPlot);
      setIsSwitchingPathway(false);
      return;
    }

    if (ids.selectedPathwayInputId) {
      setIsSwitchingPathway(true);
      setShinyInputValue(ids.selectedPathwayInputId, pathwayId, { priority: "event" });
    }
  };

  return (
    <div className="ribote-gsea-results">
      {config?.note ? (
        <div className="ribote-result-card ribote-result-card--warning">
          <p className="ribote-result-card__copy">{config.note}</p>
        </div>
      ) : null}

      <div className="ribote-gsea-grid">
        <div className="ribote-gsea-panel ribote-gsea-panel--plot">
          {hasDisplayPlot ? (
            <div className="ribote-d3-card">
              {renderError ? (
                <div className="ribote-result-card ribote-result-card--warning">
                  <p className="ribote-result-card__copy">GSEA plot rendering failed: {renderError}</p>
                </div>
              ) : null}
              <div className="ribote-gsea-host">
                <div ref={plotRef} className="ribote-d3-host" />
                {isRendering || isSwitchingPathway ? (
                  <div className="ribote-gsea-loading">
                    {isSwitchingPathway ? "Loading pathway plot..." : "Rendering enrichment plot..."}
                  </div>
                ) : null}
              </div>
            </div>
          ) : (
            <EmptyCard title="Enrichment Plot" message="Select a pathway row to inspect its enrichment curve." />
          )}
        </div>

        <div className="ribote-gsea-panel ribote-gsea-panel--table">
          {rows.length ? (
            <div className="ribote-d3-card">
              <div className="ribote-gsea-table-header">
                <div>
                  <h4 className="ribote-d3-card__title">Pathway Table</h4>
                  <p className="ribote-gsea-table-copy">
                    {tableSummaryCopy}
                  </p>
                </div>
              </div>

              <div className="ribote-gsea-table-wrap">
                <table className="ribote-gsea-table">
                  <thead>
                    <tr>
                      <th>No.</th>
                      <th>Pathway</th>
                      <th>Direction</th>
                      <th>NES</th>
                      <th>FDR</th>
                      <th>Size</th>
                    </tr>
                  </thead>
                  <tbody>
                    {paginatedRows.map((row, index) => (
                      <tr
                        key={row.pathwayId}
                        className={row.pathwayId === selectedPathway ? "is-selected" : ""}
                        onClick={() => handleRowClick(row.pathwayId)}
                      >
                        <td>{pageStart + index + 1}</td>
                        <td>
                          <div className="ribote-gsea-pathway-name">{row.pathway}</div>
                          <div className="ribote-gsea-pathway-id">{row.rawPathway}</div>
                        </td>
                        <td>{row.direction}</td>
                        <td>{formatMetric(row.nes, 3)}</td>
                        <td>{formatMetric(row.padj, 4)}</td>
                        <td>{Number.isFinite(row.size) ? row.size : "NA"}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              {shouldPaginate ? (
                <div className="ribote-table-pagination">
                <div className="ribote-table-pagination__meta">
                    Showing {pageStart + 1}-{Math.min(pageStart + pageSize, rows.length)} of {rows.length} displayed pathways | Page {currentPage} of {totalPages}
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
            <EmptyCard title="Pathway Table" message="Run GSEA to populate significant pathways." />
          )}
        </div>
      </div>
    </div>
  );
}
