import { useEffect, useMemo, useRef, useState } from "react";
import * as d3 from "d3";
import { setShinyInputValue } from "../utils/shinyBridge";

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

function positionTooltip(tooltip, container, event) {
  const tooltipNode = tooltip.node();
  if (!tooltipNode) {
    return;
  }

  const [pointerX, pointerY] = d3.pointer(event, container);
  const containerRect = container.getBoundingClientRect();
  const tooltipWidth = tooltipNode.offsetWidth || 0;
  const tooltipHeight = tooltipNode.offsetHeight || 0;
  const viewportPadding = 8;
  const desiredLeft = containerRect.left + pointerX + 14;
  const desiredTop = containerRect.top + pointerY - 10;
  const clampedLeft = Math.max(
    viewportPadding,
    Math.min(desiredLeft, window.innerWidth - tooltipWidth - viewportPadding)
  );
  const clampedTop = Math.max(
    viewportPadding,
    Math.min(desiredTop, window.innerHeight - tooltipHeight - viewportPadding)
  );

  tooltip
    .style("left", `${clampedLeft - containerRect.left}px`)
    .style("top", `${clampedTop - containerRect.top}px`);
}

function drawHorizontalBarChart(container, data, options, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  if (!data.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No chart data available.");
    return;
  }

  const width = container.clientWidth || 960;
  const barHeight = 34;
  const maxLabelLength = d3.max(data, (item) => String(item.label || "").length) || 0;
  const estimatedLabelWidth = (maxLabelLength * 9) + 36;
  const marginLeft = Math.max(100, Math.min(260, estimatedLabelWidth));
  const legendItems = Array.from(
    new Map(
      data.map((item) => [item.group || "Sample", item.color || "#859b7a"])
    ),
    ([label, color]) => ({ label, color })
  );
  const offsetX = Number(options?.offsetX) || 0;
  const offsetY = Number(options?.offsetY) || 0;
  const shouldAnimate = renderState.animate !== false;
  const margin = {
    top: (legendItems.length > 1 ? 48 : 18) + offsetY,
    right: 24,
    bottom: 46,
    left: marginLeft + offsetX
  };
  const height = margin.top + margin.bottom + barHeight * data.length;

  const svg = root.append("svg").attr("viewBox", `0 0 ${width} ${height}`).attr("class", "ribote-d3-chart");
  const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
  const innerWidth = width - margin.left - margin.right;
  const innerHeight = height - margin.top - margin.bottom;
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);

  const x = d3.scaleLinear().domain([0, d3.max(data, (item) => item.value) || 0]).nice().range([0, innerWidth]);
  const y = d3
    .scaleBand()
    .domain(data.map((item) => item.label))
    .range([0, innerHeight])
    .padding(0.22);

  const xAxisTickFormat = (value) => {
    const normalized = value / 1000;
    if (normalized === 0) {
      return "0k";
    }

    return `${d3.format(",")(Math.round(normalized))}k`;
  };

  if (options?.title) {
    svg
      .append("text")
      .attr("x", options.titleX ?? 20)
      .attr("y", options.titleY ?? 20)
      .attr("text-anchor", "start")
      .attr("class", options.titleClass || "ribote-d3-chart-title")
      .text(options.title);
  }

  const bars = chart
    .selectAll("rect")
    .data(data)
    .enter()
    .append("rect")
    .attr("x", 0)
    .attr("y", (item) => y(item.label))
    .attr("width", 0)
    .attr("height", y.bandwidth())
    .attr("fill", (item) => item.color || "#859b7a")
    .on("mouseenter", function(event, item) {
      d3.select(this).attr("opacity", 0.86);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${item.label}</div>
          ${options?.showActualLabel !== false ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Actual:</span><span class="ribote-d3-tooltip__value">${item.actualLabel || item.label}</span></div>` : ""}
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">${options?.groupLabel || "Type"}:</span><span class="ribote-d3-tooltip__value">${item.group || "Sample"}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Count:</span><span class="ribote-d3-tooltip__value">${d3.format(",")(Math.round(item.value))}</span></div>`
        );
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function() {
      d3.select(this).attr("opacity", 1);
      tooltip.style("opacity", 0);
    });

  if (shouldAnimate) {
    bars
      .transition()
      .duration(750)
      .ease(d3.easeCubicOut)
      .attr("width", (item) => x(item.value));
  } else {
    bars.attr("width", (item) => x(item.value));
  }

  chart
    .append("g")
    .call(d3.axisLeft(y).tickSize(0))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-label"));

  chart
    .append("g")
    .attr("transform", `translate(0,${innerHeight})`)
    .call(d3.axisBottom(x).ticks(5).tickSizeOuter(0).tickFormat(xAxisTickFormat))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll(".tick line").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick ribote-d3-axis-tick--x"));

  if (legendItems.length > 1) {
    const legendItemWidth = 188;
    const legendTotalWidth = legendItems.length * legendItemWidth;
    const legendStartX = Math.max((width - legendTotalWidth) / 2, margin.left);
    const legend = svg.append("g").attr("transform", `translate(${legendStartX}, 18)`);

    legendItems.forEach((item, index) => {
      const group = legend.append("g").attr("transform", `translate(${index * legendItemWidth}, 0)`);
      group.append("rect").attr("width", 18).attr("height", 18).attr("fill", item.color).attr("class", "ribote-d3-legend-swatch--library");
      group.append("text").attr("x", 28).attr("y", 15).attr("class", "ribote-d3-legend ribote-d3-legend--library").text(item.label);
    });
  }

  if (options?.xLabel) {
    svg
      .append("text")
      .attr("x", margin.left + (innerWidth / 2))
      .attr("y", height - 4)
      .attr("text-anchor", "middle")
      .attr("class", options.captionClass || "ribote-d3-caption")
      .text(options.xLabel);
  }
}

function drawStackedFractionChart(container, data, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  if (!data.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No chart data available.");
    return;
  }

  const samples = Array.from(new Set(data.map((item) => item.sample)));
  const categories = Array.from(new Set(data.map((item) => item.category)));
  const categoryColors = {
    "rRNA": "#c98468",
    "Non-rRNA": "#859b7a"
  };

  const totalsBySample = new Map(
    samples.map((sample) => [
      sample,
      d3.sum(data.filter((item) => item.sample === sample), (item) => item.total_count)
    ])
  );

  const stackedRows = samples.map((sample) => {
    const row = { sample };
    categories.forEach((category) => {
      const value =
        data.find((item) => item.sample === sample && item.category === category)?.total_count || 0;
      const total = totalsBySample.get(sample) || 1;
      row[`${category}__count`] = value;
      row[category] = value / total;
    });
    return row;
  });

  const width = container.clientWidth || 960;
  const height = 360;
  const margin = { top: 34, right: 20, bottom: 72, left: 64 };
  const innerWidth = width - margin.left - margin.right;
  const innerHeight = height - margin.top - margin.bottom;

  const svg = root.append("svg").attr("viewBox", `0 0 ${width} ${height}`).attr("class", "ribote-d3-chart");
  const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const shouldAnimate = renderState.animate !== false;

  const x = d3.scaleBand().domain(samples).range([0, innerWidth]).padding(0.24);
  const y = d3.scaleLinear().domain([0, 1]).range([innerHeight, 0]);
  const stack = d3.stack().keys(categories);
  const stackedSeries = stack(stackedRows);

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 14)
    .attr("text-anchor", "start")
    .attr("class", "ribote-d3-chart-title ribote-d3-chart-title--rrna")
    .text("rRNA Fraction by Sample");

  const segments = chart
    .selectAll(".ribote-d3-stack")
    .data(stackedSeries)
    .enter()
    .append("g")
    .attr("fill", (series) => categoryColors[series.key] || "#b9b39f")
    .selectAll("rect")
    .data((series) => series.map((item) => ({ ...item, category: series.key })))
    .enter()
    .append("rect")
    .attr("x", (item) => x(item.data.sample))
    .attr("y", innerHeight)
    .attr("height", 0)
    .attr("width", x.bandwidth())
    .on("mouseenter", function(event, item) {
      const percentValue = Math.max(0, item[1] - item[0]);
      d3.select(this).attr("opacity", 0.86);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${item.data.sample}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Actual:</span><span class="ribote-d3-tooltip__value">${item.data.sample_actual || item.data.sample}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Category:</span><span class="ribote-d3-tooltip__value">${item.category}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Fraction:</span><span class="ribote-d3-tooltip__value">${d3.format(".1%")(percentValue)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Count:</span><span class="ribote-d3-tooltip__value">${d3.format(",")(Math.round(item.data[`${item.category}__count`] || 0))}</span></div>`
        );
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function() {
      d3.select(this).attr("opacity", 1);
      tooltip.style("opacity", 0);
    });

  if (shouldAnimate) {
    segments
      .transition()
      .duration(750)
      .ease(d3.easeCubicOut)
      .attr("y", (item) => y(item[1]))
      .attr("height", (item) => y(item[0]) - y(item[1]));
  } else {
    segments
      .attr("y", (item) => y(item[1]))
      .attr("height", (item) => y(item[0]) - y(item[1]));
  }

  chart
    .append("g")
    .attr("transform", `translate(0,${innerHeight})`)
    .call(d3.axisBottom(x).tickSizeOuter(0))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll(".tick line").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick").style("text-anchor", "middle"));

  chart
    .append("g")
    .call(d3.axisLeft(y).ticks(5).tickFormat((value) => `${Math.round(value * 100)}%`))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick"));

  const legendItemWidth = 188;
  const legendTotalWidth = categories.length * legendItemWidth;
  const legendStartX = Math.max((width - legendTotalWidth) / 2, margin.left);
  const legend = svg.append("g").attr("transform", `translate(${legendStartX}, ${height - 18})`);

  categories.forEach((category, index) => {
    const group = legend.append("g").attr("transform", `translate(${index * legendItemWidth}, 0)`);
    group.append("rect").attr("width", 18).attr("height", 18).attr("fill", categoryColors[category] || "#b9b39f");
    group.append("text").attr("x", 28).attr("y", 15).attr("class", "ribote-d3-legend ribote-d3-legend--library").text(category);
  });
}

function mapBarPlotData(data) {
  return (data || []).map((item) => ({
    label: item.sample_display || item.sample,
    actualLabel: item.sample,
    value: Number(item.total_count) || 0,
    group: item.sample_type || "Sample",
    color:
      item.sample_type === "Ribo-seq"
        ? "#c98468"
        : item.sample_type === "RNA-seq"
          ? "#859b7a"
          : "#b9b39f"
  }));
}

function mapBiotypeData(data) {
  return (data || []).map((item) => ({
    label: item.gene_biotype,
    value: Number(item.genes_retained) || 0,
    group: "Gene Biotype",
    color: "#859b7a"
  }));
}

function ensureActiveViewStore() {
  return (window.__riboteActiveViewState ??= {});
}

function readPersistedActiveView(key) {
  if (!key) {
    return null;
  }

  return ensureActiveViewStore()[key] || null;
}

export default function PreprocessResults({ config }) {
  const table = config.table || {};
  const columns = table.columns || [];
  const rows = table.rows || [];
  const page = table.page || 1;
  const pageCount = table.pageCount || 1;
  const totalRows = table.totalRows || rows.length;
  const pageInputId = table.pageInputId;
  const searchQuery = table.searchQuery || "";
  const searchInputId = table.searchInputId;
  const activeViewInputId = table.activeViewInputId;
  const persistedActiveViewKey = activeViewInputId ? `ribote-active-view:${activeViewInputId}` : null;
  const configuredActiveView = table.activeView || "data";
  const [activeTab, setActiveTab] = useState(() => readPersistedActiveView(persistedActiveViewKey) || configuredActiveView);
  const pendingActiveViewRef = useRef(null);
  const lastConfiguredActiveViewRef = useRef(configuredActiveView);
  const [searchValue, setSearchValue] = useState("");
  const [pageJumpValue, setPageJumpValue] = useState("");

  const barplotData = useMemo(() => mapBarPlotData(config.charts?.barplot), [config.charts?.barplot]);
  const biotypeData = useMemo(() => mapBiotypeData(config.charts?.biotype), [config.charts?.biotype]);
  const rrnaData = useMemo(
    () => (config.charts?.rrna || []).map((item) => ({
      ...item,
      sample_actual: item.sample,
      sample: item.sample_display || item.sample
    })),
    [config.charts?.rrna]
  );

  const barplotRef = useD3Chart(
    (element) => drawHorizontalBarChart(element, barplotData, {
      title: "Library Size by Sample",
      titleX: 0,
      titleY: 12,
      titleClass: "ribote-d3-chart-title ribote-d3-chart-title--library",
      groupLabel: "Type",
      showActualLabel: true,
      xLabel: "Counts (k)",
      captionClass: "ribote-d3-caption ribote-d3-caption--library"
    }),
    [barplotData, activeTab]
  );
  const biotypeRef = useD3Chart(
    (element) => drawHorizontalBarChart(element, biotypeData, {
      title: "Gene Biotype Composition",
      titleX: 0,
      titleY: 12,
      titleClass: "ribote-d3-chart-title ribote-d3-chart-title--qc",
      groupLabel: "Class",
      showActualLabel: false,
      xLabel: "Retained genes",
      captionClass: "ribote-d3-caption ribote-d3-caption--qc"
    }),
    [biotypeData, activeTab]
  );
  const rrnaRef = useD3Chart((element) => drawStackedFractionChart(element, rrnaData), [rrnaData, activeTab]);

  useEffect(() => {
    setSearchValue(searchQuery);
  }, [searchQuery]);

  useEffect(() => {
    setPageJumpValue(String(page));
  }, [page]);

  useEffect(() => {
    if (!persistedActiveViewKey || !activeTab) {
      return;
    }

    ensureActiveViewStore()[persistedActiveViewKey] = activeTab;
  }, [activeTab, persistedActiveViewKey]);

  useEffect(() => {
    const pendingView = pendingActiveViewRef.current;
    const persistedActiveView = readPersistedActiveView(persistedActiveViewKey);
    const configuredChanged = lastConfiguredActiveViewRef.current !== configuredActiveView;

    lastConfiguredActiveViewRef.current = configuredActiveView;

    if (pendingView) {
      if (configuredActiveView === pendingView) {
        pendingActiveViewRef.current = null;
      } else if (configuredActiveView !== activeTab) {
        return;
      }
    }

    if (!configuredChanged && persistedActiveView && activeTab === persistedActiveView) {
      return;
    }

    setActiveTab((currentTab) => (currentTab === configuredActiveView ? currentTab : configuredActiveView));
  }, [activeTab, configuredActiveView, persistedActiveViewKey]);

  useEffect(() => {
    if (!activeViewInputId || activeTab === configuredActiveView) {
      return;
    }

    setShinyInputValue(activeViewInputId, activeTab, { priority: "event" });
  }, [activeTab, activeViewInputId, configuredActiveView]);

  const submitSearch = () => {
    if (!searchInputId) {
      return;
    }

    setShinyInputValue(searchInputId, searchValue, { priority: "event" });
  };

  const submitPageJump = () => {
    if (!pageInputId) {
      return;
    }

    const requestedPage = Number.parseInt(pageJumpValue, 10);
    if (Number.isNaN(requestedPage)) {
      setPageJumpValue(String(page));
      return;
    }

    const normalizedPage = Math.max(1, Math.min(pageCount, requestedPage));
    setShinyInputValue(pageInputId, String(normalizedPage), { priority: "event" });
  };

  const handleTabChange = (nextTab) => {
    if (nextTab === activeTab) {
      return;
    }

    pendingActiveViewRef.current = nextTab;
    setActiveTab(nextTab);
  };

  return (
    <div className="ribote-preprocess-results">
      <div className="ribote-canvas-tabs ribote-canvas-tabs--results">
        {[
          { id: "data", title: "Data" },
          { id: "barplot", title: "Library Size" },
          { id: "qc", title: "QC" }
        ].map((tab) => (
          <button
            key={tab.id}
            type="button"
            className={`ribote-canvas-tab${tab.id === activeTab ? " is-active" : ""}`}
            onClick={() => handleTabChange(tab.id)}
          >
            {tab.title}
          </button>
        ))}
      </div>

      {activeTab === "data" ? (
        <div className="ribote-preprocess-panel ribote-preprocess-panel--data">
          <div className="ribote-table-toolbar">
            <label className="ribote-table-toolbar__search">
              <span>Search</span>
              <div className="ribote-table-toolbar__search-controls">
                <input
                  type="text"
                  className="ribote-input"
                  value={searchValue}
                  onChange={(event) => setSearchValue(event.target.value)}
                  onKeyDown={(event) => {
                    if (event.key === "Enter") {
                      event.preventDefault();
                      submitSearch();
                    }
                  }}
                  placeholder="Search rows"
                />
                <button type="button" className="ribote-btn ribote-btn--secondary" onClick={submitSearch}>
                  Apply
                </button>
              </div>
            </label>
            <div className="ribote-table-toolbar__page-jump">
              <span>Go to page</span>
              <div className="ribote-table-toolbar__page-jump-controls">
                <input
                  type="number"
                  min="1"
                  max={pageCount}
                  className="ribote-input"
                  value={pageJumpValue}
                  onChange={(event) => setPageJumpValue(event.target.value)}
                  onKeyDown={(event) => {
                    if (event.key === "Enter") {
                      event.preventDefault();
                      submitPageJump();
                    }
                  }}
                />
                <button type="button" className="ribote-btn ribote-btn--secondary" onClick={submitPageJump}>
                  Go
                </button>
              </div>
            </div>
          </div>
          <div className="ribote-table-wrap">
            <table className="ribote-preview-table">
              <thead>
                <tr>
                  {columns.map((column) => (
                    <th key={column}>{column}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {rows.length > 0 ? (
                  rows.map((row, rowIndex) => (
                    <tr key={`row-${rowIndex}`}>
                      {columns.map((column) => (
                        <td key={`${rowIndex}-${column}`}>{String(row[column] ?? "")}</td>
                      ))}
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td colSpan={columns.length || 1} className="ribote-preview-table__empty">
                      No rows matched the current search.
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
          <div className="ribote-table-pagination">
            <span className="ribote-table-pagination__meta">
              Page {page} of {pageCount} | {totalRows} rows
            </span>
            <div className="ribote-table-pagination__actions">
              <button
                type="button"
                className="ribote-btn ribote-btn--secondary"
                disabled={page <= 1}
                onClick={() => setShinyInputValue(pageInputId, String(page - 1), { priority: "event" })}
              >
                Previous
              </button>
              <button
                type="button"
                className="ribote-btn ribote-btn--secondary"
                disabled={page >= pageCount}
                onClick={() => setShinyInputValue(pageInputId, String(page + 1), { priority: "event" })}
              >
                Next
              </button>
            </div>
          </div>
        </div>
      ) : null}

      {activeTab === "barplot" ? (
        <div className="ribote-preprocess-panel ribote-preprocess-panel--barplot">
          <div className="ribote-d3-card">
            <div ref={barplotRef} className="ribote-d3-host" />
          </div>
        </div>
      ) : null}

      {activeTab === "qc" ? (
        <div className="ribote-preprocess-panel ribote-preprocess-panel--stacked">
          <div className="ribote-d3-card">
            <div ref={biotypeRef} className="ribote-d3-host" />
          </div>
          <div className="ribote-d3-card">
            <div ref={rrnaRef} className="ribote-d3-host" />
          </div>
        </div>
      ) : null}
    </div>
  );
}
