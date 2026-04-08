import { useEffect, useMemo, useRef } from "react";
import { drawPcaProjectionChart, normalizePcaPoints } from "./pcaChart";

function useD3Chart(drawChart, deps) {
  const ref = useRef(null);
  const renderedWidthRef = useRef(0);

  useEffect(() => {
    if (!ref.current) {
      return undefined;
    }

    let cleanup;
    let frameId = null;
    const chartRoot = ref.current;

    const renderChart = (reason = "data") => {
      if (!chartRoot) {
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
    };

    const scheduleRender = (reason = "data") => {
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

  return ref;
}

export default function PcaResults({ config }) {
  const plot = config?.plot || {};
  const points = useMemo(() => normalizePcaPoints(plot.points), [plot.points]);
  const projectionRef = useD3Chart(
    (element, renderState) => drawPcaProjectionChart(element, points, {
      title: plot.title,
      xLabel: plot.xLabel,
      yLabel: plot.yLabel
    }, renderState),
    [points, plot.title, plot.xLabel, plot.yLabel]
  );

  return (
    <div className="ribote-pca-results">
      <div className="ribote-pca-panel ribote-pca-panel--projection">
        <div className="ribote-d3-card">
          <div ref={projectionRef} className="ribote-d3-host" />
        </div>
      </div>
    </div>
  );
}
