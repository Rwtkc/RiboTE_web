import { useEffect, useMemo, useRef, useState } from "react";
import { drawNetworkGraph, normalizeNetworkEdges, normalizeNetworkNodes } from "./networkChart";

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
        console.error("Network chart rendering failed.", error);
        setRenderError(error instanceof Error ? error.message : "Unknown Network chart rendering error.");
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

export default function NetworkResults({ config }) {
  const graph = config?.graph || {};
  const nodes = useMemo(() => normalizeNetworkNodes(graph.nodes), [graph.nodes]);
  const edges = useMemo(() => normalizeNetworkEdges(graph.edges), [graph.edges]);
  const [showLabels, setShowLabels] = useState(graph.autoHideLabels !== true);

  useEffect(() => {
    setShowLabels(graph.autoHideLabels !== true);
  }, [graph.signature, graph.autoHideLabels]);

  const performanceNote = useMemo(() => {
    const fragments = [];

    if (graph.displayCapped && !config?.note) {
      fragments.push("The on-page graph is a performance-pruned view of the strongest threshold-passed edges.");
    }

    if (graph.autoHideLabels) {
      fragments.push("Labels start hidden for larger graphs.");
    }

    if (graph.dragEnabled === false) {
      fragments.push("Node dragging is disabled in large-graph mode to keep navigation smooth.");
    }

    if (!fragments.length) {
      return "";
    }

    return fragments.join(" ");
  }, [config?.note, graph.autoHideLabels, graph.displayCapped, graph.dragEnabled]);

  const signature = [
    graph.signature || "",
    graph.moduleLabel || "",
    showLabels ? "labels-on" : "labels-off",
    nodes.length,
    edges.length,
    nodes.map((node) => node.id).join("|"),
    edges.slice(0, 24).map((edge) => `${edge.source}:${edge.target}:${edge.weight}`).join("|")
  ].join("::");

  const { ref, isRendering, renderError } = useD3Chart(
    (element, renderState) => drawNetworkGraph(element, {
      ...graph,
      nodes,
      edges,
      showLabels
    }, renderState),
    [signature]
  );

  const handleFitView = () => {
    ref.current?.dispatchEvent(new CustomEvent("ribote:network-fit-view"));
  };

  if (!config?.note && !nodes.length) {
    return null;
  }

  return (
    <div className="ribote-network-results">
      {config?.note ? (
        <div className="ribote-result-card ribote-result-card--warning">
          <p className="ribote-result-card__copy">{config.note}</p>
        </div>
      ) : null}

      {performanceNote ? (
        <div className="ribote-result-card">
          <p className="ribote-result-card__copy">{performanceNote}</p>
        </div>
      ) : null}

      <div className="ribote-network-panel ribote-network-panel--graph">
        <div className="ribote-d3-card">
          {renderError ? (
            <div className="ribote-result-card ribote-result-card--warning">
              <p className="ribote-result-card__copy">Network graph rendering failed: {renderError}</p>
            </div>
          ) : null}
          <div className="ribote-network-host">
            {nodes.length ? (
              <div className="ribote-network-actions">
                <button
                  type="button"
                  className="ribote-btn ribote-btn--secondary ribote-network-toggle"
                  onClick={handleFitView}
                >
                  Fit View
                </button>
                <button
                  type="button"
                  className="ribote-btn ribote-btn--secondary ribote-network-toggle"
                  onClick={() => setShowLabels((current) => !current)}
                >
                  {showLabels ? "Hide Labels" : "Show Labels"}
                </button>
              </div>
            ) : null}
            <div ref={ref} className="ribote-d3-host" />
            {isRendering ? (
              <div className="ribote-network-loading">Rendering network graph...</div>
            ) : null}
          </div>
        </div>
      </div>
    </div>
  );
}
