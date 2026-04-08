import * as d3 from "d3";

const GSEA_TEXT_COLOR = "#152018";
const GSEA_AXIS_COLOR = "#000000";
const GSEA_FONT_FAMILY = "\"Montserrat\", sans-serif";

function formatNumber(value, digits = 4) {
  if (!Number.isFinite(value)) {
    return "NA";
  }

  return Number(value).toFixed(digits);
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

export function normalizeGseaPlot(plot) {
  return {
    pathwayId: String(plot?.pathwayId || ""),
    pathway: String(plot?.pathway || ""),
    collection: String(plot?.collection || ""),
    nes: Number(plot?.nes),
    padj: Number(plot?.padj),
    pvalue: Number(plot?.pvalue),
    size: Number(plot?.size),
    leadingEdgeSize: Number(plot?.leadingEdgeSize),
    peakX: Number(plot?.peakX),
    peakY: Number(plot?.peakY),
    maxRank: Number(plot?.maxRank),
    points: Array.isArray(plot?.points)
      ? plot.points.map((point) => ({
          x: Number(point?.x),
          y: Number(point?.y)
        })).filter((point) => Number.isFinite(point.x) && Number.isFinite(point.y))
      : [],
    metricPoints: Array.isArray(plot?.metricPoints)
      ? plot.metricPoints.map((point) => ({
          x: Number(point?.x),
          y: Number(point?.y)
        })).filter((point) => Number.isFinite(point.x) && Number.isFinite(point.y))
      : [],
    hits: Array.isArray(plot?.hits)
      ? plot.hits.map((value) => Number(value)).filter((value) => Number.isFinite(value))
      : []
  };
}

export function normalizeGseaPlotCatalog(catalog) {
  const metricPoints = Array.isArray(catalog?.metricPoints)
    ? catalog.metricPoints
      .map((point) => ({
        x: Number(point?.x),
        y: Number(point?.y)
      }))
      .filter((point) => Number.isFinite(point.x) && Number.isFinite(point.y))
    : [];

  const pathways = new Map(
    (Array.isArray(catalog?.pathways) ? catalog.pathways : [])
      .map((entry) => ({
        pathwayId: String(entry?.pathwayId || ""),
        hits: Array.isArray(entry?.hits)
          ? entry.hits.map((value) => Number(value)).filter((value) => Number.isFinite(value))
          : []
      }))
      .filter((entry) => entry.pathwayId)
      .map((entry) => [entry.pathwayId, entry])
  );

  return {
    maxRank: Number(catalog?.maxRank),
    metricPoints,
    pathways
  };
}

export function buildGseaPlotFromCatalog({ row, collectionLabel, catalog }) {
  if (!row || !catalog?.metricPoints?.length || !(catalog?.pathways instanceof Map)) {
    return null;
  }

  const pathwayEntry = catalog.pathways.get(String(row.pathwayId || ""));
  if (!pathwayEntry?.hits?.length) {
    return null;
  }

  const metricPoints = catalog.metricPoints;
  const geneCount = metricPoints.length;
  const hits = pathwayEntry.hits
    .map((value) => Number(value))
    .filter((value) => Number.isFinite(value) && value >= 1 && value <= geneCount);

  if (!hits.length || hits.length >= geneCount) {
    return null;
  }

  const hitSet = new Set(hits);
  const weightedHits = metricPoints.map((point) => Math.abs(point.y));
  const hitWeightTotal = hits.reduce((sum, position) => sum + weightedHits[position - 1], 0);

  if (!Number.isFinite(hitWeightTotal) || hitWeightTotal <= 0) {
    return null;
  }

  const missPenalty = -1 / (geneCount - hits.length);
  let runningScore = 0;
  let peakX = NaN;
  let peakY = NaN;
  let peakAbs = -1;

  const points = metricPoints.map((point, index) => {
    runningScore += hitSet.has(index + 1)
      ? weightedHits[index] / hitWeightTotal
      : missPenalty;

    if (Math.abs(runningScore) > peakAbs) {
      peakAbs = Math.abs(runningScore);
      peakX = point.x;
      peakY = runningScore;
    }

    return {
      x: point.x,
      y: runningScore
    };
  });

  return normalizeGseaPlot({
    pathwayId: row.pathwayId,
    pathway: row.pathway,
    collection: collectionLabel,
    nes: row.nes,
    padj: row.padj,
    pvalue: row.pvalue,
    size: row.size,
    leadingEdgeSize: row.leadingEdgeSize,
    peakX,
    peakY,
    maxRank: Number.isFinite(catalog.maxRank) ? catalog.maxRank : geneCount,
    points,
    metricPoints,
    hits
  });
}

export function drawGseaEnrichmentPlot(container, plot, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  if (!plot?.points?.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No enrichment plot is available.");
    return undefined;
  }

  const width = container.clientWidth || 920;
  const height = 520;
  const margin = { top: 54, right: 22, bottom: 58, left: 68 };
  const innerWidth = Math.max(120, width - margin.left - margin.right);
  const innerHeight = Math.max(200, height - margin.top - margin.bottom);
  const rugHeight = 22;
  const curveHeight = Math.max(150, Math.round(innerHeight * 0.58));
  const metricHeight = Math.max(90, innerHeight - curveHeight - rugHeight - 28);
  const metricTop = curveHeight + rugHeight + 28;
  const shouldAnimate = renderState.animate !== false;
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);

  const x = d3.scaleLinear()
    .domain(d3.extent(plot.points, (point) => point.x))
    .range([0, innerWidth]);

  const yExtent = d3.extent(plot.points, (point) => point.y);
  const yPadding = Math.max(0.05, (Math.max(Math.abs(yExtent[0] || 0), Math.abs(yExtent[1] || 0)) || 0.1) * 0.08);
  const y = d3.scaleLinear()
    .domain([yExtent[0] - yPadding, yExtent[1] + yPadding])
    .range([curveHeight, 0]);

  const metricPoints = plot.metricPoints.length ? plot.metricPoints : plot.points.map((point) => ({ x: point.x, y: 0 }));
  const metricExtent = d3.extent(metricPoints, (point) => point.y);
  const metricAbsMax = Math.max(Math.abs(metricExtent[0] || 0), Math.abs(metricExtent[1] || 0), 0.1);
  const metricY = d3.scaleLinear()
    .domain([-metricAbsMax, metricAbsMax])
    .range([metricHeight, 0]);

  const svg = root
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("class", "ribote-d3-chart");

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 14)
    .attr("class", "ribote-d3-chart-title ribote-d3-chart-title--library")
    .attr("fill", GSEA_TEXT_COLOR)
    .attr("font-family", GSEA_FONT_FAMILY)
    .attr("font-size", 17)
    .attr("font-weight", 800)
    .text(`${plot.pathway} (${plot.collection})`);

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 34)
    .attr("class", "ribote-d3-chart-subtitle")
    .attr("fill", GSEA_TEXT_COLOR)
    .attr("font-family", GSEA_FONT_FAMILY)
    .attr("font-size", 13)
    .attr("font-weight", 700)
    .text(`NES ${formatNumber(plot.nes, 3)} | FDR ${formatNumber(plot.padj, 4)} | Size ${Number.isFinite(plot.size) ? plot.size : "NA"}`);

  const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
  const baselineY = y(0);
  const curveGroup = chart.append("g").attr("class", "ribote-gsea-curve");

  curveGroup
    .append("line")
    .attr("x1", 0)
    .attr("x2", innerWidth)
    .attr("y1", baselineY)
    .attr("y2", baselineY)
    .attr("stroke", GSEA_AXIS_COLOR)
    .attr("stroke-width", 1)
    .attr("stroke-dasharray", "4 4");

  const lineGenerator = d3.line()
    .x((point) => x(point.x))
    .y((point) => y(point.y))
    .curve(d3.curveMonotoneX);

  const areaGenerator = d3.area()
    .x((point) => x(point.x))
    .y0(baselineY)
    .y1((point) => y(point.y))
    .curve(d3.curveMonotoneX);

  const areaPath = curveGroup
    .append("path")
    .datum(plot.points)
    .attr("fill", "rgba(75, 116, 182, 0.15)")
    .attr("d", areaGenerator);

  const linePath = curveGroup
    .append("path")
    .datum(plot.points)
    .attr("fill", "none")
    .attr("stroke", "#325d9f")
    .attr("stroke-width", 2.35)
    .attr("stroke-linejoin", "round")
    .attr("stroke-linecap", "round")
    .attr("d", lineGenerator);

  if (shouldAnimate) {
    const totalLength = linePath.node()?.getTotalLength?.() || 0;
    if (totalLength > 0) {
      linePath
        .attr("stroke-dasharray", `${totalLength} ${totalLength}`)
        .attr("stroke-dashoffset", totalLength)
        .transition()
        .duration(520)
        .ease(d3.easeCubicOut)
        .attr("stroke-dashoffset", 0);

      areaPath
        .attr("opacity", 0)
        .transition()
        .duration(420)
        .ease(d3.easeCubicOut)
        .attr("opacity", 1);
    }
  }

  const rugTop = curveHeight + 14;
  chart
    .append("g")
    .attr("class", "ribote-gsea-rug")
    .selectAll("line")
    .data(plot.hits)
    .join("line")
    .attr("x1", (value) => x(value))
    .attr("x2", (value) => x(value))
    .attr("y1", rugTop)
    .attr("y2", rugTop + rugHeight)
    .attr("stroke", "#c77469")
    .attr("stroke-width", 1)
    .attr("opacity", 0.78);

  const metricGroup = chart.append("g").attr("transform", `translate(0,${metricTop})`);

  metricGroup
    .append("line")
    .attr("x1", 0)
    .attr("x2", innerWidth)
    .attr("y1", metricY(0))
    .attr("y2", metricY(0))
    .attr("stroke", GSEA_AXIS_COLOR)
    .attr("stroke-width", 1);

  const metricArea = d3.area()
    .x((point) => x(point.x))
    .y0(metricY(0))
    .y1((point) => metricY(point.y))
    .curve(d3.curveMonotoneX);

  metricGroup
    .append("path")
    .datum(metricPoints)
    .attr("fill", "rgba(153, 153, 153, 0.45)")
    .attr("stroke", "none")
    .attr("d", metricArea);

  if (Number.isFinite(plot.peakX) && Number.isFinite(plot.peakY)) {
    chart
      .append("circle")
      .attr("cx", x(plot.peakX))
      .attr("cy", y(plot.peakY))
      .attr("r", 4.2)
      .attr("fill", "#c77469")
      .attr("stroke", "#fffdf8")
      .attr("stroke-width", 1.4);
  }

  chart
    .append("g")
    .attr("transform", `translate(0,${metricTop + metricHeight})`)
    .call(
      d3.axisBottom(x)
        .ticks(6)
        .tickSizeOuter(0)
    )
    .call((axis) => axis.selectAll("text")
      .attr("class", "ribote-d3-axis-text")
      .attr("fill", GSEA_TEXT_COLOR)
      .attr("font-family", GSEA_FONT_FAMILY)
      .attr("font-size", 12)
      .attr("font-weight", 700))
    .call((axis) => axis.selectAll("line").attr("stroke", GSEA_AXIS_COLOR).attr("stroke-width", 1))
    .call((axis) => axis.select(".domain").attr("stroke", GSEA_AXIS_COLOR).attr("stroke-width", 1));

  chart
    .append("g")
    .call(
      d3.axisLeft(y)
        .ticks(6)
        .tickSizeOuter(0)
    )
    .call((axis) => axis.selectAll("text")
      .attr("class", "ribote-d3-axis-text")
      .attr("fill", GSEA_TEXT_COLOR)
      .attr("font-family", GSEA_FONT_FAMILY)
      .attr("font-size", 12)
      .attr("font-weight", 700))
    .call((axis) => axis.selectAll("line").attr("stroke", GSEA_AXIS_COLOR).attr("stroke-width", 1))
    .call((axis) => axis.select(".domain").attr("stroke", GSEA_AXIS_COLOR).attr("stroke-width", 1));

  metricGroup
    .append("g")
    .call(
      d3.axisLeft(metricY)
        .ticks(3)
        .tickSizeOuter(0)
    )
    .call((axis) => axis.selectAll("text")
      .attr("class", "ribote-d3-axis-text")
      .attr("fill", GSEA_TEXT_COLOR)
      .attr("font-family", GSEA_FONT_FAMILY)
      .attr("font-size", 12)
      .attr("font-weight", 700))
    .call((axis) => axis.selectAll("line").attr("stroke", GSEA_AXIS_COLOR).attr("stroke-width", 1))
    .call((axis) => axis.select(".domain").attr("stroke", GSEA_AXIS_COLOR).attr("stroke-width", 1));

  chart
    .append("text")
    .attr("x", innerWidth / 2)
    .attr("y", height - 6)
    .attr("class", "ribote-d3-axis-title")
    .attr("text-anchor", "middle")
    .attr("fill", GSEA_TEXT_COLOR)
    .attr("font-family", GSEA_FONT_FAMILY)
    .attr("font-size", 13)
    .attr("font-weight", 800)
    .text("Rank in TE_log2FC-Ordered Gene List");

  chart
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("x", -(curveHeight / 2))
    .attr("y", -48)
    .attr("class", "ribote-d3-axis-title")
    .attr("text-anchor", "middle")
    .attr("fill", GSEA_TEXT_COLOR)
    .attr("font-family", GSEA_FONT_FAMILY)
    .attr("font-size", 13)
    .attr("font-weight", 800)
    .text("Running Enrichment Score");

  chart
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("x", -(metricTop + metricHeight / 2))
    .attr("y", -48)
    .attr("class", "ribote-d3-axis-title")
    .attr("text-anchor", "middle")
    .attr("fill", GSEA_TEXT_COLOR)
    .attr("font-family", GSEA_FONT_FAMILY)
    .attr("font-size", 13)
    .attr("font-weight", 800)
    .text("Ranked List Metric");

  const overlay = chart
    .append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", innerWidth)
    .attr("height", metricTop + metricHeight)
    .attr("fill", "transparent");

  const focusLine = chart
    .append("line")
    .attr("stroke", GSEA_AXIS_COLOR)
    .attr("stroke-width", 1)
    .attr("stroke-dasharray", "4 4")
    .style("opacity", 0);

  const focusPoint = chart
    .append("circle")
    .attr("r", 4)
    .attr("fill", "#325d9f")
    .attr("stroke", "#fffdf8")
    .attr("stroke-width", 1.2)
    .style("opacity", 0);

  overlay
    .on("mouseenter", () => {
      tooltip.style("opacity", 1);
      focusLine.style("opacity", 1);
      focusPoint.style("opacity", 1);
    })
    .on("mousemove", (event) => {
      const pointerX = d3.pointer(event, overlay.node())[0];
      const hoveredRank = x.invert(pointerX);
      const bisector = d3.bisector((point) => point.x).center;
      const index = bisector(plot.points, hoveredRank);
      const point = plot.points[Math.max(0, Math.min(plot.points.length - 1, index))];

      focusLine
        .attr("x1", x(point.x))
        .attr("x2", x(point.x))
        .attr("y1", 0)
        .attr("y2", metricTop + metricHeight);

      focusPoint
        .attr("cx", x(point.x))
        .attr("cy", y(point.y));

      const metricPoint = metricPoints[Math.max(0, Math.min(metricPoints.length - 1, index))];

      tooltip
        .html(`
          <div><strong>${plot.pathway}</strong></div>
          <div>Rank: ${point.x}</div>
          <div>ES: ${formatNumber(point.y, 4)}</div>
          <div>Ranked Metric: ${formatNumber(metricPoint?.y, 4)}</div>
          <div>NES: ${formatNumber(plot.nes, 3)}</div>
          <div>FDR: ${formatNumber(plot.padj, 4)}</div>
        `)
        .style("opacity", 1);

      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", () => {
      tooltip.style("opacity", 0);
      focusLine.style("opacity", 0);
      focusPoint.style("opacity", 0);
    });

  return () => {
    tooltip.remove();
  };
}
