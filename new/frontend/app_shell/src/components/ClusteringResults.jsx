import { useEffect, useMemo, useRef, useState } from "react";
import { setShinyInputValue } from "../utils/shinyBridge";
import { drawClusteringHeatmap, normalizeClusteringHeatmap } from "./clusteringChart";

function useD3Chart(drawChart, deps) {
  const ref = useRef(null);
  const renderedWidthRef = useRef(0);
  const [isRendering, setIsRendering] = useState(false);

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

      if (typeof cleanup === "function") {
        cleanup();
      }

      renderedWidthRef.current = Math.round(chartRoot.clientWidth || 0);
      cleanup = drawChart(chartRoot, {
        animate: reason !== "resize",
        reason
      });
      setIsRendering(false);
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

  return { ref, isRendering };
}

function EmptyCard({ title, message }) {
  return (
    <div className="ribote-d3-card">
      <h4 className="ribote-d3-card__title">{title}</h4>
      <p className="ribote-clustering-panel__empty">{message}</p>
    </div>
  );
}

function DetailValue({ label, value }) {
  return (
    <div className="ribote-clustering-detail__item">
      <span className="ribote-clustering-detail__label">{label}</span>
      <span className="ribote-clustering-detail__value">{value || "NA"}</span>
    </div>
  );
}

export default function ClusteringResults({ config }) {
  const ids = config?.ids || {};
  const mainHeatmap = useMemo(() => normalizeClusteringHeatmap(config?.main), [config?.main]);
  const detailHeatmap = useMemo(() => normalizeClusteringHeatmap(config?.detail), [config?.detail]);
  const [selectedCell, setSelectedCell] = useState(null);
  const lastResolvedDetailRef = useRef({
    mainSignature: "",
    detailMode: "",
    heatmap: null,
    summary: "",
    emptyMessage: ""
  });

  const hasIncomingDetail = detailHeatmap.rows.length && detailHeatmap.columns.length;

  if (hasIncomingDetail) {
    lastResolvedDetailRef.current = {
      mainSignature: mainHeatmap.signature,
      detailMode: config?.detailMode || "",
      heatmap: detailHeatmap,
      summary: config?.detailSummary || "",
      emptyMessage: config?.detailEmptyMessage || ""
    };
  }

  const isGeneIdsMode = /gene/i.test(String(config?.detailMode || ""));

  const preservePreviousDetail = (
    !hasIncomingDetail &&
    !isGeneIdsMode &&
    !config?.detailEmptyMessage &&
    lastResolvedDetailRef.current.heatmap &&
    lastResolvedDetailRef.current.mainSignature === mainHeatmap.signature &&
    lastResolvedDetailRef.current.detailMode === (config?.detailMode || "")
  );

  const resolvedDetailHeatmap = preservePreviousDetail ? lastResolvedDetailRef.current.heatmap : detailHeatmap;
  const resolvedDetailSummary = preservePreviousDetail
    ? lastResolvedDetailRef.current.summary
    : (config?.detailSummary || "");
  const resolvedDetailEmptyMessage = preservePreviousDetail
    ? lastResolvedDetailRef.current.emptyMessage
    : (config?.detailEmptyMessage || "");

  useEffect(() => {
    setSelectedCell(null);
  }, [resolvedDetailHeatmap.signature, config?.detailMode]);

  const { ref: mainRef, isRendering: mainRendering } = useD3Chart(
    (element, renderState) => drawClusteringHeatmap(
      element,
      mainHeatmap,
      {
        chartHeight: 780,
        activeCell: selectedCell,
        onCellClick: (cell) => {
          setSelectedCell(cell);
        },
        onBrushSelection: !isGeneIdsMode && mainHeatmap.brushEnabled && ids.selectionInputId
          ? (selection) => {
              setShinyInputValue(ids.selectionInputId, selection, { priority: "event" });
            }
          : undefined
      },
      renderState
    ),
    [ids.selectionInputId, mainHeatmap.brushEnabled, mainHeatmap.signature, config?.detailMode, isGeneIdsMode, selectedCell]
  );

  const { ref: detailRef, isRendering: detailRendering } = useD3Chart(
    (element, renderState) => drawClusteringHeatmap(
      element,
      resolvedDetailHeatmap,
      {
        chartHeight: 780,
        activeCell: selectedCell,
        emptyMessage: resolvedDetailEmptyMessage,
        onCellClick: (cell) => {
          setSelectedCell(cell);
        }
      },
      renderState
    ),
    [resolvedDetailEmptyMessage, resolvedDetailHeatmap.signature, selectedCell]
  );

  return (
    <div className="ribote-clustering-results">
      <div className="ribote-clustering-grid">
        <div className="ribote-clustering-panel ribote-clustering-panel--main">
          <div className="ribote-d3-card">
            <div className="ribote-clustering-host">
              <div ref={mainRef} className="ribote-d3-host" />
              {mainRendering ? <div className="ribote-clustering-loading">Rendering main heatmap...</div> : null}
            </div>
          </div>
        </div>

        <div className="ribote-clustering-panel ribote-clustering-panel--detail">
          {resolvedDetailHeatmap.rows.length && resolvedDetailHeatmap.columns.length ? (
            <div className="ribote-d3-card">
              <div className="ribote-clustering-host">
                <div ref={detailRef} className="ribote-d3-host" />
                {detailRendering ? <div className="ribote-clustering-loading">Rendering detail heatmap...</div> : null}
              </div>
            </div>
          ) : (
            <EmptyCard
              title="Detail Heatmap"
              message={resolvedDetailEmptyMessage || "No detail heatmap is available yet."}
            />
          )}
        </div>
      </div>

      <div className="ribote-clustering-detail">
        <div className="ribote-clustering-detail__header">
          <div>
            <h4>Detail Selection</h4>
            <p>
              {config?.detailModeLabel ? `${config.detailModeLabel}. ` : ""}
              {resolvedDetailSummary || resolvedDetailEmptyMessage || "Brush the main heatmap or switch to Gene IDs."}
            </p>
          </div>
        </div>

        {selectedCell ? (
          <div className="ribote-clustering-detail__grid">
            <DetailValue label="Gene ID" value={selectedCell.gene} />
            <DetailValue label="Sample" value={selectedCell.displaySample} />
            <DetailValue label="Actual" value={selectedCell.actualSample} />
            <DetailValue label="Group" value={selectedCell.group} />
            <DetailValue label="Value" value={Number.isFinite(selectedCell.value) ? selectedCell.value.toFixed(4) : "NA"} />
          </div>
        ) : (
          <p className="ribote-clustering-detail__empty">
            Click a cell in the main or detail heatmap to inspect its value and sample mapping.
          </p>
        )}
      </div>
    </div>
  );
}

