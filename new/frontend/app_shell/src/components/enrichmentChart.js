import * as d3 from "d3";

const TEXT_COLOR = "#17292f";
const AXIS_COLOR = "#000000";
const FONT_FAMILY = "sans-serif";
const DOWN_FILL = "#bfe4e8";
const UP_FILL = "#f3b18c";

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
  const desiredTop = containerRect.top + pointerY - 12;
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

export function normalizeEnrichmentPlotRows(rows) {
  if (!Array.isArray(rows)) {
    return [];
  }

  return rows
    .map((row, index) => ({
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
    }))
    .filter((row) => row.pathway && Number.isFinite(row.padj));
}

function buildSignedScore(row) {
  const safePadj = Math.max(Number.isFinite(row.padj) ? row.padj : 1, 1e-300);
  const score = Math.max(0, -Math.log10(safePadj));
  return row.group === "Down" ? -score : score;
}

export function drawEnrichmentOverviewChart(container, rows, options = {}, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  if (!rows.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No enrichment plot is available.");
    return undefined;
  }

  const width = container.clientWidth || 980;
  const rowHeight = 40;
  const groupGap = 30;
  const margin = { top: 56, right: 100, bottom: 72, left: 100 };
  const shouldAnimate = renderState.animate !== false;
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);

  const plottedRows = rows.map((row) => ({
    ...row,
    signedScore: buildSignedScore(row)
  }));
  const layoutRows = [];
  let runningY = 0;

  plottedRows.forEach((row, index) => {
    if (index > 0 && row.group !== plottedRows[index - 1].group) {
      runningY += groupGap;
    }

    layoutRows.push({
      ...row,
      yCenter: runningY + rowHeight / 2
    });

    runningY += rowHeight;
  });

  const height = Math.max(400, runningY + margin.top + margin.bottom + 24);
  const innerWidth = Math.max(360, width - margin.left - margin.right);
  const innerHeight = Math.max(220, runningY);

  const maxAbsScore = Math.max(
    d3.max(layoutRows, (row) => Math.abs(row.signedScore)) || 1,
    1
  );

  const x = d3.scaleLinear()
    .domain([-maxAbsScore * 1.12, maxAbsScore * 1.12])
    .range([0, innerWidth]);

  const barHeight = Math.max(14, Math.min(24, rowHeight * 0.76));
  const labelGap = 14;

  const svg = root
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("class", "ribote-d3-chart");

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 12)
    .attr("fill", TEXT_COLOR)
    .attr("font-family", FONT_FAMILY)
    .attr("font-size", 14)
    .attr("font-weight", 800)
    .text(options.title || "Enrichment Overview");

  const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
  const zeroX = x(0);

  const legend = chart.append("g").attr("transform", `translate(${(innerWidth - 160) / 2}, -18)`);

  [
    { label: "Down", fill: DOWN_FILL, textFill: "#0a5963" },
    { label: "Up", fill: UP_FILL, textFill: "#934018" }
  ].forEach((item, index) => {
    const group = legend.append("g").attr("transform", `translate(${index * 88}, 0)`);

    group
      .append("rect")
      .attr("x", 0)
      .attr("y", -9)
      .attr("width", 14)
      .attr("height", 14)
      .attr("rx", 2)
      .attr("fill", item.fill);

    group
      .append("text")
      .attr("x", 22)
      .attr("y", 1)
      .attr("dominant-baseline", "middle")
      .attr("fill", item.textFill)
      .attr("font-family", FONT_FAMILY)
      .attr("font-size", 13)
      .attr("font-weight", 700)
      .text(item.label);
  });

  chart
    .append("g")
    .attr("transform", `translate(0,${innerHeight})`)
    .call(
      d3.axisBottom(x)
        .ticks(6)
        .tickFormat((value) => d3.format(".0f")(Math.abs(value)))
        .tickSizeOuter(0)
    )
    .call((axis) => axis.selectAll("text")
      .attr("fill", TEXT_COLOR)
      .attr("font-family", FONT_FAMILY)
      .attr("font-size", 12)
      .attr("font-weight", 700))
    .call((axis) => axis.selectAll("path,line").attr("stroke", AXIS_COLOR));

  chart
    .append("text")
    .attr("x", innerWidth / 2)
    .attr("y", innerHeight + 52)
    .attr("text-anchor", "middle")
    .attr("fill", TEXT_COLOR)
    .attr("font-family", FONT_FAMILY)
    .attr("font-size", 14)
    .attr("font-weight", 700)
    .text("-log10(FDR)");

  chart
    .append("line")
    .attr("x1", zeroX)
    .attr("x2", zeroX)
    .attr("y1", -10)
    .attr("y2", innerHeight)
    .attr("stroke", "rgba(0, 0, 0, 0.2)")
    .attr("stroke-width", 1.6)
    .attr("stroke-dasharray", "6 7");

  const rowGroups = chart
    .append("g")
    .selectAll("g")
    .data(layoutRows)
    .enter()
    .append("g")
    .attr("transform", (row) => `translate(0,${row.yCenter})`);

  rowGroups
    .append("rect")
    .attr("x", zeroX)
    .attr("y", -barHeight / 2)
    .attr("height", barHeight)
    .attr("fill", (row) => row.group === "Down" ? DOWN_FILL : UP_FILL)
    .attr("opacity", 0.96)
    .attr("width", shouldAnimate ? 0 : (row) => Math.abs(x(row.signedScore) - zeroX))
    .attr("x", shouldAnimate ? zeroX : (row) => Math.min(zeroX, x(row.signedScore)))
    .on("mouseenter", function(event, row) {
      d3.select(this).attr("opacity", 1);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${row.pathway}</div>
          ${row.rawPathway ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">ID:</span><span class="ribote-d3-tooltip__value">${row.rawPathway}</span></div>` : ""}
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Group:</span><span class="ribote-d3-tooltip__value">${row.group}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">-log10(FDR):</span><span class="ribote-d3-tooltip__value">${d3.format(".2f")(Math.abs(row.signedScore))}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">FDR:</span><span class="ribote-d3-tooltip__value">${d3.format(".4g")(row.padj)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Fold:</span><span class="ribote-d3-tooltip__value">${d3.format(".2f")(row.fold)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Overlap:</span><span class="ribote-d3-tooltip__value">${row.overlap} / ${row.querySize}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Pathway Size:</span><span class="ribote-d3-tooltip__value">${row.pathwaySize}</span></div>`
        );
      positionTooltip(tooltip, container, event);
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function() {
      d3.select(this).attr("opacity", 0.96);
      tooltip.style("opacity", 0);
    })
    .transition()
    .duration(420)
    .ease(d3.easeCubicOut)
    .attr("x", (row) => Math.min(zeroX, x(row.signedScore)))
    .attr("width", (row) => Math.abs(x(row.signedScore) - zeroX));

  rowGroups
    .append("text")
    .attr("x", (row) => row.group === "Down" ? zeroX + labelGap : zeroX - labelGap)
    .attr("y", 1)
    .attr("text-anchor", (row) => row.group === "Down" ? "start" : "end")
    .attr("dominant-baseline", "middle")
    .attr("fill", TEXT_COLOR)
    .attr("font-family", FONT_FAMILY)
    .attr("font-size", 13)
    .attr("font-weight", 700)
    .text((row) => row.pathway);

  chart
    .append("text")
    .attr("x", innerWidth)
    .attr("y", innerHeight + 52)
    .attr("text-anchor", "end")
    .attr("fill", "#5a6b73")
    .attr("font-family", FONT_FAMILY)
    .attr("font-size", 11)
    .attr("font-weight", 600)
    .text(options.backgroundLabel ? `Background: ${options.backgroundLabel}` : "");

  return () => {
    tooltip.remove();
  };
}
