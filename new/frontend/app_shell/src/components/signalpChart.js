import * as d3 from "d3";

const SIGNALP_GROUP_COLORS = {
  Up: "#0f6d78",
  Non: "#8fa1a7",
  Down: "#d45a2a"
};

const SIGNALP_GRID_STROKE = "#147782";
const SIGNALP_GRID_OPACITY = 0.14;

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

function formatPercent(value) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) {
    return "NA";
  }

  return `${numeric.toFixed(1)}%`;
}

function formatPValue(value) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) {
    return "NA";
  }

  return numeric < 1e-3 ? numeric.toExponential(2) : numeric.toFixed(4);
}

export function normalizeSignalpPlotRows(rows) {
  if (!Array.isArray(rows)) {
    return [];
  }

  return rows.map((row, index) => ({
    id: String(row?.id || `${row?.method || "method"}-${row?.teGroup || "group"}-${index + 1}`),
    method: String(row?.method || ""),
    methodLabel: String(row?.methodLabel || row?.method || `Method ${index + 1}`),
    teGroup: String(row?.teGroup || ""),
    annotatedCount: Number(row?.annotatedCount),
    totalCount: Number(row?.totalCount),
    percent: Number(row?.percent) * 100,
    upVsNonPValue: Number(row?.upVsNonPValue),
    downVsNonPValue: Number(row?.downVsNonPValue)
  }));
}

export function drawSignalpOverviewChart(container, rows, options = {}, renderState = {}) {
  const normalizedRows = Array.isArray(rows) ? rows : [];
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  if (!normalizedRows.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No SignalP comparison is available.");
    return undefined;
  }

  const width = container.clientWidth || 960;
  const height = Math.max(480, Math.min(620, 430 + Math.max(0, normalizedRows.length - 6) * 18));
  const margin = {
    top: 100,
    right: 34,
    bottom: 76,
    left: 84
  };
  const innerWidth = Math.max(120, width - margin.left - margin.right);
  const innerHeight = Math.max(220, height - margin.top - margin.bottom);
  const title = options.title || "SignalP Comparison";
  const subtitle = options.subtitle || "";
  const methodLabels = Array.from(new Set(normalizedRows.map((row) => row.methodLabel)));
  const teGroups = ["Up", "Non", "Down"];
  const maxPercent = d3.max(normalizedRows, (row) => row.percent) || 0;
  const yDomainMax = maxPercent > 0 ? Math.min(100, maxPercent * 1.18) : 10;
  const shouldAnimate = renderState.animate !== false;
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);

  const svg = root
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("class", "ribote-d3-chart");

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 12)
    .attr("text-anchor", "start")
    .attr("class", "ribote-d3-chart-title ribote-d3-chart-title--library")
    .text(title);

  if (subtitle) {
    svg
      .append("text")
      .attr("x", 0)
      .attr("y", 34)
      .attr("text-anchor", "start")
      .attr("class", "ribote-d3-chart-subtitle")
      .text(subtitle);
  }

  const legendItemSpacing = 126;
  const legendWidth = ((teGroups.length - 1) * legendItemSpacing) + 92;
  const legendX = Math.max(0, (width - legendWidth) / 2);
  const legend = svg.append("g").attr("transform", `translate(${legendX}, 66)`);
  teGroups.forEach((group, index) => {
    const legendItem = legend.append("g").attr("transform", `translate(${index * legendItemSpacing}, 0)`);

    legendItem
      .append("rect")
      .attr("width", 18)
      .attr("height", 18)
      .attr("rx", 4)
      .attr("fill", SIGNALP_GROUP_COLORS[group] || "#147782");

    legendItem
      .append("text")
      .attr("x", 28)
      .attr("y", 14)
      .attr("class", "ribote-d3-legend ribote-d3-legend--library")
      .text(group);
  });

  const chart = svg.append("g").attr("transform", `translate(${margin.left}, ${margin.top})`);
  const x0 = d3.scaleBand().domain(methodLabels).range([0, innerWidth]).padding(0.24);
  const x1 = d3.scaleBand().domain(teGroups).range([0, x0.bandwidth()]).padding(0.16);
  const y = d3.scaleLinear().domain([0, yDomainMax]).nice().range([innerHeight, 0]);

  chart
    .append("g")
    .attr("class", "ribote-d3-grid")
    .call(d3.axisLeft(y).ticks(5).tickSize(-innerWidth).tickFormat(() => ""))
    .call((axis) => axis.select(".domain").remove())
    .call((axis) => axis
      .selectAll("line")
      .attr("stroke", SIGNALP_GRID_STROKE)
      .attr("stroke-opacity", SIGNALP_GRID_OPACITY));

  const xAxis = chart
    .append("g")
    .attr("transform", `translate(0, ${innerHeight})`)
    .call(d3.axisBottom(x0));

  xAxis.select(".domain").attr("stroke", "#000000");
  xAxis.selectAll("text").attr("class", "ribote-d3-axis-tick");
  xAxis.selectAll("line").attr("stroke", "#000000");

  const yAxis = chart.append("g").call(d3.axisLeft(y).ticks(5).tickFormat((value) => `${value}%`));
  yAxis.select(".domain").attr("stroke", "#000000");
  yAxis.selectAll("text").attr("class", "ribote-d3-axis-tick");
  yAxis.selectAll("line").attr("stroke", "#000000");

  chart
    .append("text")
    .attr("x", innerWidth / 2)
    .attr("y", innerHeight + 56)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text("Annotation Method");

  chart
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("x", -innerHeight / 2)
    .attr("y", -56)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text("Annotated Genes Within TE Group (%)");

  const barGroups = chart
    .append("g")
    .selectAll("g")
    .data(methodLabels)
    .join("g")
    .attr("transform", (label) => `translate(${x0(label)}, 0)`);

  const bars = barGroups
    .selectAll("rect")
    .data((label) => normalizedRows.filter((row) => row.methodLabel === label))
    .join("rect")
    .attr("x", (row) => x1(row.teGroup) || 0)
    .attr("y", innerHeight)
    .attr("width", x1.bandwidth())
    .attr("height", 0)
    .attr("rx", 5)
    .attr("fill", (row) => SIGNALP_GROUP_COLORS[row.teGroup] || "#147782")
    .attr("stroke", "rgba(16, 35, 42, 0.16)")
    .attr("stroke-width", 1)
    .on("mouseenter", function handleMouseEnter(event, row) {
      const comparisonText = row.teGroup === "Up"
        ? `Up vs Non p: ${formatPValue(row.upVsNonPValue)}`
        : row.teGroup === "Down"
          ? `Down vs Non p: ${formatPValue(row.downVsNonPValue)}`
          : `Up vs Non p: ${formatPValue(row.upVsNonPValue)} | Down vs Non p: ${formatPValue(row.downVsNonPValue)}`;

      tooltip
        .html(
          `<div class="ribote-d3-tooltip__title">${row.methodLabel} | ${row.teGroup}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Annotated:</span><span class="ribote-d3-tooltip__value">${row.annotatedCount}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Total:</span><span class="ribote-d3-tooltip__value">${row.totalCount}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Percentage:</span><span class="ribote-d3-tooltip__value">${formatPercent(row.percent)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Fisher Test:</span><span class="ribote-d3-tooltip__value">${comparisonText}</span></div>`
        )
        .style("opacity", 1);

      positionTooltip(tooltip, container, event);
      d3.select(this).attr("stroke-width", 1.4);
    })
    .on("mousemove", (event) => {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function handleMouseLeave() {
      tooltip.style("opacity", 0);
      d3.select(this).attr("stroke-width", 1);
    });

  const labels = barGroups
    .selectAll("text")
    .data((label) => normalizedRows.filter((row) => row.methodLabel === label))
    .join("text")
    .attr("x", (row) => (x1(row.teGroup) || 0) + x1.bandwidth() / 2)
    .attr("y", innerHeight - 6)
    .attr("text-anchor", "middle")
    .attr("fill", "rgba(16, 35, 42, 0.78)")
    .style("font-size", "11px")
    .style("font-weight", 700)
    .text((row) => formatPercent(row.percent));

  const finalizeBars = (selection) => {
    selection
      .attr("y", (row) => y(row.percent))
      .attr("height", (row) => Math.max(0, innerHeight - y(row.percent)));
  };

  const finalizeLabels = (selection) => {
    selection.attr("y", (row) => Math.max(12, y(row.percent) - 8));
  };

  if (shouldAnimate) {
    bars.transition().duration(420).ease(d3.easeCubicOut).call(finalizeBars);
    labels.transition().duration(420).ease(d3.easeCubicOut).call(finalizeLabels);
  } else {
    finalizeBars(bars);
    finalizeLabels(labels);
  }

  return () => {
    tooltip.remove();
  };
}
