import { useEffect, useMemo, useRef, useState } from "react";
import * as d3 from "d3";

let marginalScatterClipSequence = 0;
const TE_DISPLAY_POINT_LIMIT = 5000;
const TE_LARGE_POINT_SET_THRESHOLD = 4000;

export function useD3Chart(drawChart, deps) {
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

function teStatusColor(status) {
  switch (status) {
    case "Up":
      return "#c98468";
    case "Down":
      return "#607d6d";
    default:
      return "#b9b39f";
  }
}

function toFiniteNumber(value, fallback = 0) {
  const numberValue = Number(value);
  return Number.isFinite(numberValue) ? numberValue : fallback;
}

function filterScatterDataForScale(data, scaleType = "linear") {
  const finiteData = (data || []).filter((item) => Number.isFinite(item?.x) && Number.isFinite(item?.y));

  if (scaleType === "log2") {
    return finiteData.filter((item) => item.x > 0 && item.y > 0);
  }

  return finiteData;
}

function deterministicStrideSample(data, limit) {
  if (!Array.isArray(data) || data.length <= limit) {
    return data || [];
  }

  if (!Number.isFinite(limit) || limit <= 0) {
    return [];
  }

  if (limit === 1) {
    return [data[0]];
  }

  const selected = [];
  const usedIndexes = new Set();
  const step = (data.length - 1) / (limit - 1);

  for (let index = 0; index < limit; index += 1) {
    let sourceIndex = Math.round(index * step);

    while (usedIndexes.has(sourceIndex) && sourceIndex < data.length - 1) {
      sourceIndex += 1;
    }

    while (usedIndexes.has(sourceIndex) && sourceIndex > 0) {
      sourceIndex -= 1;
    }

    if (!usedIndexes.has(sourceIndex)) {
      usedIndexes.add(sourceIndex);
      selected.push(data[sourceIndex]);
    }
  }

  return selected;
}

function buildDisplayDataSubset(data, limit = TE_DISPLAY_POINT_LIMIT, displayMeta = null) {
  const fullData = Array.isArray(data) ? data : [];
  const normalizedLimit = Math.max(1, Math.floor(Number(limit) || TE_DISPLAY_POINT_LIMIT));
  const originalCount = Number.isFinite(Number(displayMeta?.originalCount))
    ? Number(displayMeta.originalCount)
    : fullData.length;
  const displayedCount = Number.isFinite(Number(displayMeta?.displayedCount))
    ? Number(displayMeta.displayedCount)
    : fullData.length;

  if (fullData.length <= normalizedLimit && originalCount <= fullData.length) {
    return {
      rows: fullData,
      originalCount,
      displayedCount,
      isSubset: false
    };
  }

  if (fullData.length <= normalizedLimit) {
    return {
      rows: fullData,
      originalCount,
      displayedCount: fullData.length,
      isSubset: true
    };
  }

  const prioritized = fullData.filter((item) => item?.status && item.status !== "Non");
  const background = fullData.filter((item) => !item?.status || item.status === "Non");
  const priorityRows = prioritized.length >= normalizedLimit
    ? deterministicStrideSample(prioritized, normalizedLimit)
    : prioritized;
  const backgroundRows = deterministicStrideSample(background, normalizedLimit - priorityRows.length);
  const rows = priorityRows.concat(backgroundRows);

  return {
    rows,
    originalCount,
    displayedCount: rows.length,
    isSubset: true
  };
}

function getMetaDomain(meta, key) {
  const domain = meta?.[key];
  if (!Array.isArray(domain) || domain.length < 2) {
    return null;
  }

  const minValue = Number(domain[0]);
  const maxValue = Number(domain[1]);
  if (!Number.isFinite(minValue) || !Number.isFinite(maxValue)) {
    return null;
  }

  return [minValue, maxValue];
}

export function buildLegendCountsWithMeta(data, meta) {
  const counts = meta?.statusCounts;
  if (counts && ["Up", "Non", "Down"].some((status) => Number.isFinite(Number(counts[status])))) {
    return {
      Up: Number(counts.Up || 0),
      Non: Number(counts.Non || 0),
      Down: Number(counts.Down || 0)
    };
  }

  return buildLegendCounts(data);
}

function appendDisplaySubsetNotice(root, displaySubset) {
  if (!displaySubset?.isSubset) {
    return;
  }

  root
    .append("div")
    .attr("class", "ribote-gsea-table-copy")
    .text(
      `Only a deterministic display subset is drawn (${displaySubset.displayedCount.toLocaleString()} of ${displaySubset.originalCount.toLocaleString()} points); table values and data exports still use the full result set.`
    );
}

function ensureNumericDomain(values, fallback = [-1, 1]) {
  const numericValues = values.filter((value) => Number.isFinite(value));
  if (!numericValues.length) {
    return fallback;
  }

  const minValue = d3.min(numericValues);
  const maxValue = d3.max(numericValues);

  if (minValue === maxValue) {
    const delta = Math.max(Math.abs(minValue) * 0.15, 1);
    return [minValue - delta, maxValue + delta];
  }

  return [minValue, maxValue];
}

function buildLegendCounts(data) {
  const counts = { Up: 0, Non: 0, Down: 0 };

  (data || []).forEach((item) => {
    const status = item?.status;
    if (Object.prototype.hasOwnProperty.call(counts, status)) {
      counts[status] += 1;
    }
  });

  return counts;
}

export function shallowObjectArrayEqual(left, right) {
  if (left === right) {
    return true;
  }

  if (!Array.isArray(left) || !Array.isArray(right) || left.length !== right.length) {
    return false;
  }

  for (let index = 0; index < left.length; index += 1) {
    const leftRow = left[index];
    const rightRow = right[index];

    if (leftRow === rightRow) {
      continue;
    }

    const leftKeys = Object.keys(leftRow || {});
    const rightKeys = Object.keys(rightRow || {});

    if (leftKeys.length !== rightKeys.length) {
      return false;
    }

    for (const key of leftKeys) {
      if (leftRow?.[key] !== rightRow?.[key]) {
        return false;
      }
    }
  }

  return true;
}

function pearsonCorrelation(data) {
  if (!Array.isArray(data) || data.length < 2) {
    return null;
  }

  const valid = data.filter((item) => Number.isFinite(item.x) && Number.isFinite(item.y));
  if (valid.length < 2) {
    return null;
  }

  const meanX = d3.mean(valid, (item) => item.x) || 0;
  const meanY = d3.mean(valid, (item) => item.y) || 0;
  const numerator = d3.sum(valid, (item) => (item.x - meanX) * (item.y - meanY));
  const denominatorX = Math.sqrt(d3.sum(valid, (item) => (item.x - meanX) ** 2));
  const denominatorY = Math.sqrt(d3.sum(valid, (item) => (item.y - meanY) ** 2));

  if (!denominatorX || !denominatorY) {
    return null;
  }

  return numerator / (denominatorX * denominatorY);
}

function buildLog2TickValues(domain, targetCount = 5) {
  const minValue = Math.max(domain?.[0] || 1, 1e-12);
  const maxValue = Math.max(domain?.[1] || 1, minValue * 2);
  const minExponent = Math.floor(Math.log2(minValue));
  const maxExponent = Math.ceil(Math.log2(maxValue));
  const exponentSpan = Math.max(maxExponent - minExponent, 1);
  const step = Math.max(1, Math.ceil(exponentSpan / Math.max(targetCount - 1, 1)));
  const ticks = [];

  for (let exponent = minExponent; exponent <= maxExponent; exponent += step) {
    ticks.push(2 ** exponent);
  }

  if (!ticks.includes(2 ** maxExponent)) {
    ticks.push(2 ** maxExponent);
  }

  return ticks.filter((value) => value >= minValue && value <= maxValue);
}

export function formatLogTick(value) {
  if (!Number.isFinite(value) || value <= 0) {
    return "";
  }

  if (value >= 10000 || value < 0.1) {
    return d3.format(".4e")(value);
  }

  if (value >= 100) {
    return d3.format(",.0f")(value);
  }

  if (value >= 1) {
    return d3.format("~g")(value);
  }

  return d3.format(".4f")(value);
}

function kernelEpanechnikov(bandwidth) {
  return (value) => {
    const scaled = value / bandwidth;
    return Math.abs(scaled) <= 1 ? (0.75 * (1 - (scaled ** 2))) / bandwidth : 0;
  };
}

function kernelDensityEstimator(kernel, thresholds, values) {
  return thresholds.map((threshold) => ([
    threshold,
    d3.mean(values, (value) => kernel(threshold - value)) || 0
  ]));
}

export function drawScatterChart(container, data, options, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");
  const scaleType = options.scaleType || "linear";
  const fullPlotData = filterScatterDataForScale(data, scaleType);
  const displaySubset = buildDisplayDataSubset(fullPlotData, options.displayPointLimit, options.displayMeta);
  const plotData = displaySubset.rows;

  if (!plotData.length) {
    if ((data || []).length > 0) {
      root
        .append("div")
        .attr("class", "ribote-d3-loading")
        .text(scaleType === "log2"
          ? "No finite positive points are available for this log-scale chart."
          : "No finite points are available for this chart.");
    }
    return;
  }

  const width = container.clientWidth || 960;
  const height = Number(options?.chartHeight) || 420;
  const margin = {
    top: Number(options?.marginTop) || 68,
    right: 28,
    bottom: 64,
    left: 76
  };
  const innerWidth = width - margin.left - margin.right;
  const innerHeight = height - margin.top - margin.bottom;
  const svg = root.append("svg").attr("viewBox", `0 0 ${width} ${height}`).attr("class", "ribote-d3-chart");
  const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const legendItems = options.legendItems || ["Up", "Non", "Down"];
  const legendCounts = options.legendCounts || {};
  const metaXDomain = getMetaDomain(options.displayMeta, "xDomain");
  const metaYDomain = getMetaDomain(options.displayMeta, "yDomain");
  const xDomain = scaleType === "log2"
    ? (metaXDomain || ensureNumericDomain(fullPlotData.map((item) => item.x), [1, 2]))
    : (metaXDomain || ensureNumericDomain(fullPlotData.map((item) => item.x)));
  const yDomain = scaleType === "log2"
    ? (metaYDomain || ensureNumericDomain(fullPlotData.map((item) => item.y), [1, 2]))
    : (metaYDomain || ensureNumericDomain(fullPlotData.map((item) => item.y)));
  const x = scaleType === "log2"
    ? d3.scaleLog().base(2).domain(xDomain).range([0, innerWidth])
    : d3.scaleLinear().domain(xDomain).nice().range([0, innerWidth]);
  const y = scaleType === "log2"
    ? d3.scaleLog().base(2).domain(yDomain).range([innerHeight, 0])
    : d3.scaleLinear().domain(yDomain).nice().range([innerHeight, 0]);
  const isLargePointSet = fullPlotData.length > TE_LARGE_POINT_SET_THRESHOLD;
  const shouldAnimate = renderState.animate !== false;
  const showLegend = options.showLegend !== false;
  const pointColor = options.pointColor || null;
  const axisTickFormat = scaleType === "log2" ? formatLogTick : null;
  const xTickValues = scaleType === "log2" ? buildLog2TickValues(xDomain) : null;
  const yTickValues = scaleType === "log2" ? buildLog2TickValues(yDomain) : null;

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 12)
    .attr("class", `ribote-d3-chart-title ${options.titleClass || ""}`.trim())
    .text(options.title);

  if (scaleType !== "log2" && x(0) >= 0 && x(0) <= innerWidth) {
    chart
      .append("line")
      .attr("x1", x(0))
      .attr("x2", x(0))
      .attr("y1", 0)
      .attr("y2", innerHeight)
      .attr("stroke", "#cfc5b5")
      .attr("stroke-dasharray", "6 6");
  }

  if (scaleType !== "log2" && y(0) >= 0 && y(0) <= innerHeight) {
    chart
      .append("line")
      .attr("x1", 0)
      .attr("x2", innerWidth)
      .attr("y1", y(0))
      .attr("y2", y(0))
      .attr("stroke", "#cfc5b5")
      .attr("stroke-dasharray", "6 6");
  }

  const thresholdLines = options.referenceLines || {};
  (thresholdLines.x || []).forEach((value) => {
    if (!Number.isFinite(value) || x(value) < 0 || x(value) > innerWidth) {
      return;
    }

    chart
      .append("line")
      .attr("x1", x(value))
      .attr("x2", x(value))
      .attr("y1", 0)
      .attr("y2", innerHeight)
      .attr("stroke", "#b9b39f")
      .attr("stroke-width", 1.2)
      .attr("stroke-dasharray", "6 6");
  });

  (thresholdLines.y || []).forEach((value) => {
    if (!Number.isFinite(value) || y(value) < 0 || y(value) > innerHeight) {
      return;
    }

    chart
      .append("line")
      .attr("x1", 0)
      .attr("x2", innerWidth)
      .attr("y1", y(value))
      .attr("y2", y(value))
      .attr("stroke", "#b9b39f")
      .attr("stroke-width", 1.2)
      .attr("stroke-dasharray", "6 6");
  });

  const points = chart
    .selectAll("circle")
    .data(plotData)
    .enter()
    .append("circle")
    .attr("cx", (item) => x(item.x))
    .attr("cy", (item) => y(item.y))
    .attr("r", 0)
    .attr("fill", (item) => pointColor || teStatusColor(item.status))
    .attr("opacity", 0.78)
    .on("mouseenter", function(event, item) {
      d3.select(this).attr("opacity", 1).attr("stroke", "#51483f").attr("stroke-width", 1.2);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${item.gene}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Status:</span><span class="ribote-d3-tooltip__value">${item.status}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">${options.xLabel}:</span><span class="ribote-d3-tooltip__value">${d3.format(".3f")(item.x)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">${options.yLabel}:</span><span class="ribote-d3-tooltip__value">${d3.format(".3f")(item.y)}</span></div>
          ${options.extraRows ? options.extraRows(item) : ""}`
        );
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function() {
      d3.select(this).attr("opacity", 0.78).attr("stroke", "none");
      tooltip.style("opacity", 0);
    });

  if (shouldAnimate && !isLargePointSet) {
    points
      .attr("r", 0)
      .transition()
      .duration(720)
      .ease(d3.easeCubicOut)
      .attr("r", 4.6);
  } else {
    points.attr("r", 4.6);
  }

  chart
    .append("g")
    .attr("transform", `translate(0,${innerHeight})`)
    .call(
      d3.axisBottom(x)
        .ticks(scaleType === "log2" ? xTickValues.length : 6)
        .tickValues(xTickValues)
        .tickFormat(axisTickFormat)
        .tickSizeOuter(0)
    )
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll(".tick line").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick"));

  chart
    .append("g")
    .call(
      d3.axisLeft(y)
        .ticks(scaleType === "log2" ? yTickValues.length : 6)
        .tickValues(yTickValues)
        .tickFormat(axisTickFormat)
        .tickSizeOuter(0)
    )
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick"));

  svg
    .append("text")
    .attr("x", margin.left + (innerWidth / 2))
    .attr("y", height - 8)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-caption ribote-d3-caption--library")
    .text(options.xLabel);

  svg
    .append("text")
    .attr("transform", `translate(18, ${margin.top + (innerHeight / 2)}) rotate(-90)`)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-caption ribote-d3-caption--library")
    .text(options.yLabel);

  if (showLegend) {
    const legendItemWidth = 136;
    const legend = svg.append("g").attr("transform", `translate(${Math.max((width - legendItemWidth * legendItems.length) / 2, margin.left)}, 28)`);

    legendItems.forEach((item, index) => {
      const group = legend.append("g").attr("transform", `translate(${index * legendItemWidth}, 0)`);
      group.append("rect").attr("width", 16).attr("height", 16).attr("rx", 6).attr("fill", teStatusColor(item));
      group
        .append("text")
        .attr("x", 24)
        .attr("y", 13)
        .attr("class", "ribote-d3-legend ribote-d3-legend--library")
        .text(`${item} (${legendCounts[item] ?? 0})`);
    });
  }

  if (options.showCorrelation) {
    const correlation = pearsonCorrelation(fullPlotData);
    if (Number.isFinite(correlation)) {
      svg
        .append("text")
        .attr("x", width - margin.right)
        .attr("y", 18)
        .attr("text-anchor", "end")
        .attr("class", "ribote-d3-legend ribote-d3-legend--library")
        .text(`Pearson r = ${d3.format(".3f")(correlation)}`);
    }
  }

  appendDisplaySubsetNotice(root, displaySubset);
}

export function drawMarginalDensityScatterChart(container, data, options, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");
  const fullPlotData = filterScatterDataForScale(data, "linear");
  const displaySubset = buildDisplayDataSubset(fullPlotData, options.displayPointLimit, options.displayMeta);
  const plotData = displaySubset.rows;

  if (!plotData.length) {
    if ((data || []).length > 0) {
      root
        .append("div")
        .attr("class", "ribote-d3-loading")
        .text("No finite points are available for this chart.");
    }
    return;
  }

  const width = container.clientWidth || 960;
  const height = Number(options?.chartHeight) || 520;
  const margin = { top: 60, right: 60, bottom: 64, left: 76 };
  const topHeight = 96;
  const rightWidth = 96;
  const gap = 0;
  const scatterWidth = width - margin.left - margin.right - rightWidth - gap;
  const scatterHeight = height - margin.top - margin.bottom - topHeight - gap;
  const isLargePointSet = fullPlotData.length > TE_LARGE_POINT_SET_THRESHOLD;
  const shouldAnimate = renderState.animate !== false;
  const legendItems = ["Up", "Non", "Down"];
  const legendCounts = options.legendCounts || {};
  const x = d3.scaleLinear().domain([-4, 4]).range([0, scatterWidth]);
  const y = d3.scaleLinear().domain([-4, 4]).range([scatterHeight, 0]);

  const svg = root.append("svg").attr("viewBox", `0 0 ${width} ${height}`).attr("class", "ribote-d3-chart");
  const scatter = svg.append("g").attr("transform", `translate(${margin.left},${margin.top + topHeight + gap})`);
  const topChart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
  const rightChart = svg.append("g").attr("transform", `translate(${margin.left + scatterWidth + gap},${margin.top + topHeight + gap})`);
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const clipId = `ribote-te-marginal-clip-${marginalScatterClipSequence += 1}`;

  svg
    .append("defs")
    .append("clipPath")
    .attr("id", clipId)
    .append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", scatterWidth)
    .attr("height", scatterHeight);

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 12)
    .attr("class", `ribote-d3-chart-title ${options.titleClass || ""}`.trim())
    .text(options.title);

  const densitySteps = d3.range(-4, 4.001, 0.1);
  const densityBandwidth = 0.45;
  const groupedDensityX = legendItems.map((status) => ({
    status,
    density: kernelDensityEstimator(
      kernelEpanechnikov(densityBandwidth),
      densitySteps,
      fullPlotData.filter((item) => item.status === status).map((item) => item.x)
    )
  }));
  const groupedDensityY = legendItems.map((status) => ({
    status,
    density: kernelDensityEstimator(
      kernelEpanechnikov(densityBandwidth),
      densitySteps,
      fullPlotData.filter((item) => item.status === status).map((item) => item.y)
    )
  }));
  const topMax = d3.max(groupedDensityX.flatMap((entry) => entry.density.map((point) => point[1]))) || 1;
  const rightMax = d3.max(groupedDensityY.flatMap((entry) => entry.density.map((point) => point[1]))) || 1;
  const topScale = d3.scaleLinear().domain([0, topMax]).range([topHeight, 0]);
  const rightScale = d3.scaleLinear().domain([0, rightMax]).range([0, rightWidth]);
  const topArea = d3.area().curve(d3.curveBasis).x((point) => x(point[0])).y0(topHeight).y1((point) => topScale(point[1]));
  const rightArea = d3.area().curve(d3.curveBasis).y((point) => y(point[0])).x0(0).x1((point) => rightScale(point[1]));

  groupedDensityX.forEach(({ status, density }) => {
    const path = topChart
      .append("path")
      .datum(density)
      .attr("fill", teStatusColor(status))
      .attr("opacity", 0.45)
      .attr("stroke", "#111")
      .attr("stroke-width", 0.9);

    if (shouldAnimate && !isLargePointSet) {
      path.attr("d", topArea(density.map(([value]) => [value, 0]))).transition().duration(620).ease(d3.easeCubicOut).attr("d", topArea(density));
    } else {
      path.attr("d", topArea(density));
    }
  });

  groupedDensityY.forEach(({ status, density }) => {
    const path = rightChart
      .append("path")
      .datum(density)
      .attr("fill", teStatusColor(status))
      .attr("opacity", 0.45)
      .attr("stroke", "#111")
      .attr("stroke-width", 0.9);

    if (shouldAnimate && !isLargePointSet) {
      path.attr("d", rightArea(density.map(([value]) => [value, 0]))).transition().duration(620).ease(d3.easeCubicOut).attr("d", rightArea(density));
    } else {
      path.attr("d", rightArea(density));
    }
  });

  const pointsLayer = scatter.append("g").attr("clip-path", `url(#${clipId})`);
  const points = pointsLayer
    .selectAll("circle")
    .data(plotData)
    .enter()
    .append("circle")
    .attr("cx", (item) => x(item.x))
    .attr("cy", (item) => y(item.y))
    .attr("r", 0)
    .attr("fill", (item) => teStatusColor(item.status))
    .attr("opacity", 0.72)
    .on("mouseenter", function(event, item) {
      d3.select(this).attr("opacity", 1).attr("stroke", "#51483f").attr("stroke-width", 1.2);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${item.gene}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Status:</span><span class="ribote-d3-tooltip__value">${item.status}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">${options.xLabel}:</span><span class="ribote-d3-tooltip__value">${d3.format(".3f")(item.x)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">${options.yLabel}:</span><span class="ribote-d3-tooltip__value">${d3.format(".3f")(item.y)}</span></div>`
        );
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function() {
      d3.select(this).attr("opacity", 0.72).attr("stroke", "none");
      tooltip.style("opacity", 0);
    });

  if (shouldAnimate && !isLargePointSet) {
    points
      .transition()
      .duration(720)
      .ease(d3.easeCubicOut)
      .attr("r", 4.2);
  } else {
    points.attr("r", 4.2);
  }

  scatter
    .append("g")
    .attr("transform", `translate(0,${scatterHeight})`)
    .call(d3.axisBottom(x).tickValues([-4, -2, 0, 2, 4]).tickSizeOuter(0))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll(".tick line").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick"));

  scatter
    .append("g")
    .call(d3.axisLeft(y).tickValues([-4, -2, 0, 2, 4]).tickSizeOuter(0))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick"));

  scatter
    .append("rect")
    .attr("width", scatterWidth)
    .attr("height", scatterHeight)
    .attr("fill", "none")
    .attr("stroke", "#000")
    .attr("stroke-width", 1);

  topChart
    .append("g")
    .attr("transform", `translate(0,${topHeight})`)
    .call(d3.axisBottom(x).tickValues([]).tickSize(0))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"));

  rightChart
    .append("g")
    .call(d3.axisLeft(y).tickValues([]).tickSize(0))
    .call((axis) => axis.select(".domain").attr("stroke", "#000"));

  svg
    .append("text")
    .attr("x", margin.left + (scatterWidth / 2))
    .attr("y", height - 8)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-caption ribote-d3-caption--library")
    .text(options.xLabel);

  svg
    .append("text")
    .attr("transform", `translate(18, ${margin.top + topHeight + gap + (scatterHeight / 2)}) rotate(-90)`)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-caption ribote-d3-caption--library")
    .text(options.yLabel);

  const legendItemWidth = 136;
  const legend = svg.append("g").attr("transform", `translate(${Math.max((width - legendItemWidth * legendItems.length) / 2, margin.left)}, 28)`);
  legendItems.forEach((item, index) => {
    const group = legend.append("g").attr("transform", `translate(${index * legendItemWidth}, 0)`);
    group.append("rect").attr("width", 16).attr("height", 16).attr("rx", 6).attr("fill", teStatusColor(item));
    group
      .append("text")
      .attr("x", 24)
      .attr("y", 13)
        .attr("class", "ribote-d3-legend ribote-d3-legend--library")
        .text(`${item} (${legendCounts[item] ?? 0})`);
  });

  appendDisplaySubsetNotice(root, displaySubset);
}

export function mapVolcanoData(data) {
  return (data || []).map((item) => ({
    gene: item.GeneID || "",
    x: toFiniteNumber(item.log2FoldChange, 0),
    y: toFiniteNumber(item.significance, 0),
    pvalue: toFiniteNumber(item.pvalue, 0),
    padj: toFiniteNumber(item.padj, 0),
    status: item.diffTE || "Non"
  }));
}

export function mapScatterData(data, yKey) {
  return (data || []).map((item) => ({
    gene: item.GeneID || "",
    x: toFiniteNumber(item.log2InputFC, 0),
    y: toFiniteNumber(item[yKey], 0),
    rpf: toFiniteNumber(item.log2RPFFC, 0),
    te: toFiniteNumber(item.log2FoldChange, 0),
    status: item.diffTE || "Non"
  }));
}

export function mapExpressionScatterData(data, xKey, yKey, statusKey) {
  return (data || []).map((item) => ({
    gene: item.GeneID || "",
    x: toFiniteNumber(item[xKey], 0),
    y: toFiniteNumber(item[yKey], 0),
    status: item[statusKey] || "Non"
  }));
}

export function ensureActiveViewStore() {
  return (window.__riboteActiveViewState ??= {});
}

export function readPersistedActiveView(key) {
  if (!key) {
    return null;
  }

  return ensureActiveViewStore()[key] || null;
}

