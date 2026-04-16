import * as d3 from "d3";

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

function expandDomain(domain, ratio = 0.12) {
  const span = domain[1] - domain[0];
  if (!Number.isFinite(span) || span <= 0) {
    return [domain[0] - 1, domain[1] + 1];
  }

  const padding = span * ratio;
  return [domain[0] - padding, domain[1] + padding];
}

function sampleGroupColor(group) {
  if (group === "Treatment") {
    return "#d45a2a";
  }

  return "#0f6d78";
}

export function normalizePcaPoints(points) {
  if (!Array.isArray(points)) {
    return [];
  }

  return points
    .map((item) => ({
      displaySample: String(item?.display_sample || item?.displaySample || ""),
      actualSample: String(item?.actual_sample || item?.actualSample || ""),
      actualRna: String(item?.actual_rna || item?.actualRna || ""),
      actualRibo: String(item?.actual_ribo || item?.actualRibo || ""),
      group: String(item?.group || ""),
      x: Number(item?.x),
      y: Number(item?.y)
    }))
    .filter((item) => Number.isFinite(item.x) && Number.isFinite(item.y));
}

export function drawPcaProjectionChart(container, points, options, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  if (!points.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No PCA projection data available.");
    return undefined;
  }

  const width = container.clientWidth || 960;
  const height = 600;
  const margin = { top: 68, right: 28, bottom: 74, left: 82 };
  const innerWidth = width - margin.left - margin.right;
  const innerHeight = height - margin.top - margin.bottom;
  const shouldAnimate = renderState.animate !== false;
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const groups = Array.from(new Set(points.map((item) => item.group).filter(Boolean)));

  const svg = root.append("svg").attr("viewBox", `0 0 ${width} ${height}`).attr("class", "ribote-d3-chart");

  const xDomain = expandDomain(ensureNumericDomain(points.map((item) => item.x)));
  const yDomain = expandDomain(ensureNumericDomain(points.map((item) => item.y)));

  const x = d3.scaleLinear().domain(xDomain).nice().range([0, innerWidth]);
  const y = d3.scaleLinear().domain(yDomain).nice().range([innerHeight, 0]);

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 12)
    .attr("text-anchor", "start")
    .attr("class", "ribote-d3-chart-title ribote-d3-chart-title--qc")
    .text(options.title || "PCA Projection");

  const legendItemWidth = 172;
  const legend = svg
    .append("g")
    .attr("transform", `translate(${Math.max((width - legendItemWidth * groups.length) / 2, margin.left)}, 28)`);

  groups.forEach((group, index) => {
    const legendGroup = legend.append("g").attr("transform", `translate(${index * legendItemWidth}, 0)`);

    legendGroup
      .append("circle")
      .attr("cx", 10)
      .attr("cy", 0)
      .attr("r", 10)
      .attr("fill", sampleGroupColor(group));

    legendGroup
      .append("text")
      .attr("x", 30)
      .attr("y", 6)
      .attr("class", "ribote-d3-legend ribote-d3-legend--library")
      .text(group);
  });

  const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

  chart
    .append("g")
    .attr("class", "ribote-d3-grid")
    .attr("transform", `translate(0,${innerHeight})`)
    .call(d3.axisBottom(x).tickSize(-innerHeight).tickFormat(() => ""))
    .call((axis) => axis.selectAll("line").attr("stroke", "rgba(20, 119, 130, 0.14)"))
    .call((axis) => axis.selectAll("path").attr("stroke", "none"));

  chart
    .append("g")
    .attr("class", "ribote-d3-grid")
    .call(d3.axisLeft(y).tickSize(-innerWidth).tickFormat(() => ""))
    .call((axis) => axis.selectAll("line").attr("stroke", "rgba(20, 119, 130, 0.14)"))
    .call((axis) => axis.selectAll("path").attr("stroke", "none"));

  chart
    .append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", innerWidth)
    .attr("height", innerHeight)
    .attr("fill", "none")
    .attr("stroke", "#000")
    .attr("stroke-width", 1);

  if (x.domain()[0] < 0 && x.domain()[1] > 0) {
    chart
      .append("line")
      .attr("x1", x(0))
      .attr("x2", x(0))
      .attr("y1", 0)
      .attr("y2", innerHeight)
      .attr("stroke", "rgba(20, 119, 130, 0.32)")
      .attr("stroke-dasharray", "6 6");
  }

  if (y.domain()[0] < 0 && y.domain()[1] > 0) {
    chart
      .append("line")
      .attr("x1", 0)
      .attr("x2", innerWidth)
      .attr("y1", y(0))
      .attr("y2", y(0))
      .attr("stroke", "rgba(20, 119, 130, 0.32)")
      .attr("stroke-dasharray", "6 6");
  }

  chart
    .append("g")
    .attr("transform", `translate(0,${innerHeight})`)
    .call(d3.axisBottom(x).ticks(6))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick"))
    .call((axis) => axis.selectAll("path,line").attr("stroke", "#000"));

  chart
    .append("g")
    .call(d3.axisLeft(y).ticks(6))
    .call((axis) => axis.selectAll("text").attr("class", "ribote-d3-axis-tick"))
    .call((axis) => axis.selectAll("path,line").attr("stroke", "#000"));

  chart
    .append("text")
    .attr("x", innerWidth / 2)
    .attr("y", innerHeight + 54)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text(options.xLabel || "Dimension 1");

  chart
    .append("text")
    .attr("x", -innerHeight / 2)
    .attr("y", -56)
    .attr("transform", "rotate(-90)")
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text(options.yLabel || "Dimension 2");

  const pointGroups = chart
    .append("g")
    .selectAll("g")
    .data(points)
    .enter()
    .append("g")
    .attr("transform", (item) => `translate(${x(item.x)},${y(item.y)})`);

  const circles = pointGroups
    .append("circle")
    .attr("r", shouldAnimate ? 0 : 8)
    .attr("fill", (item) => sampleGroupColor(item.group))
    .attr("fill-opacity", 0.9)
    .attr("stroke", "rgba(255,255,255,0.92)")
    .attr("stroke-width", 1.5)
    .on("mouseenter", function(event, item) {
      d3.select(this).attr("stroke-width", 2.5);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${item.displaySample}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Group:</span><span class="ribote-d3-tooltip__value">${item.group}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Actual:</span><span class="ribote-d3-tooltip__value">${item.actualSample}</span></div>
          ${item.actualRna ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">RNA:</span><span class="ribote-d3-tooltip__value">${item.actualRna}</span></div>` : ""}
          ${item.actualRibo ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Ribo:</span><span class="ribote-d3-tooltip__value">${item.actualRibo}</span></div>` : ""}
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">${options.xLabel}:</span><span class="ribote-d3-tooltip__value">${d3.format(".4f")(item.x)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">${options.yLabel}:</span><span class="ribote-d3-tooltip__value">${d3.format(".4f")(item.y)}</span></div>`
        );
      positionTooltip(tooltip, container, event);
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function() {
      d3.select(this).attr("stroke-width", 1.5);
      tooltip.style("opacity", 0);
    });

  if (shouldAnimate) {
    circles
      .transition()
      .duration(650)
      .ease(d3.easeCubicOut)
      .attr("r", 8);
  }

  pointGroups
    .append("text")
    .attr("x", 12)
    .attr("y", 4)
    .attr("class", "ribote-d3-axis-tick")
    .attr("font-size", 12)
    .attr("font-weight", 700)
    .attr("paint-order", "stroke")
    .attr("stroke", "rgba(255,252,246,0.95)")
    .attr("stroke-width", 3)
    .attr("stroke-linejoin", "round")
    .text((item) => item.displaySample);

  return () => {
    tooltip.remove();
  };
}
