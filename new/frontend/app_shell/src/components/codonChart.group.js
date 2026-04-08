import * as d3 from "d3";
import {
  CODON_GROUP_COLORS,
  CODON_GROUP_ORDER,
  appendPlotClip,
  buildLinearDomain,
  drawAxisWithGrid,
  drawGroupLegend,
  formatInteger,
  formatNumber,
  formatPValue,
  formatPercent,
  nextClipId,
  positionTooltip
} from "./codonChart.shared.js";

export function drawCodonUsageGroupChart(container, panel, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  const groups = Array.isArray(panel?.groups) ? panel.groups : [];
  if (!groups.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No codon-usage group summary is available.");
    return undefined;
  }

  const width = container.clientWidth || 760;
  const height = 392;
  const margin = { top: 90, right: 30, bottom: 118, left: 72 };
  const innerWidth = Math.max(200, width - margin.left - margin.right);
  const innerHeight = Math.max(160, height - margin.top - margin.bottom);
  const maxPercent = d3.max(groups, (group) => Number(group?.max)) || 0;
  const yMax = maxPercent > 0 ? maxPercent * 1.15 : 1;
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const shouldAnimate = renderState.animate !== false;

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
    .text(`${panel?.codon || "Codon"} Usage by TE Group`);

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 34)
    .attr("text-anchor", "start")
    .attr("class", "ribote-d3-chart-subtitle")
    .text(`Genes measured: ${Number(panel?.genesMeasured || 0).toLocaleString()} | Up vs Non p: ${formatPValue(panel?.upVsNonPValue)} | Down vs Non p: ${formatPValue(panel?.downVsNonPValue)}`);

  const chart = svg.append("g").attr("transform", `translate(${margin.left}, ${margin.top})`);
  const xScale = d3.scaleBand().domain(CODON_GROUP_ORDER).range([0, innerWidth]).padding(0.32);
  const yScale = d3.scaleLinear().domain([0, yMax]).nice().range([innerHeight, 0]);

  drawAxisWithGrid(chart, yScale, innerWidth);

  const xAxis = chart.append("g").attr("transform", `translate(0, ${innerHeight})`).call(d3.axisBottom(xScale));
  xAxis.select(".domain").attr("stroke", "#000000");
  xAxis.selectAll("text").attr("class", "ribote-d3-axis-tick");
  xAxis.selectAll("line").attr("stroke", "#000000");
  xAxis.selectAll("text").attr("dy", "1.1em");

  const yAxis = chart.append("g").call(d3.axisLeft(yScale).ticks(5).tickFormat((value) => `${value}%`));
  yAxis.select(".domain").attr("stroke", "#000000");
  yAxis.selectAll("text").attr("class", "ribote-d3-axis-tick");
  yAxis.selectAll("line").attr("stroke", "#000000");

  chart
    .append("text")
    .attr("x", innerWidth / 2)
    .attr("y", innerHeight + 44)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text("Translation Efficiency Group");

  chart
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("x", -innerHeight / 2)
    .attr("y", -50)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text("Selected Codon Usage in CDS (%)");

  const groupLayer = chart.append("g");
  const panelGroups = groupLayer
    .selectAll("g")
    .data(groups)
    .join("g")
    .attr("transform", (group) => `translate(${xScale(group.teGroup) || 0}, 0)`);

  panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() / 2)
    .attr("x2", xScale.bandwidth() / 2)
    .attr("y1", (group) => yScale(group.min))
    .attr("y2", (group) => yScale(group.max))
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.2);

  panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() * 0.24)
    .attr("x2", xScale.bandwidth() * 0.76)
    .attr("y1", (group) => yScale(group.min))
    .attr("y2", (group) => yScale(group.min))
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.2);

  panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() * 0.24)
    .attr("x2", xScale.bandwidth() * 0.76)
    .attr("y1", (group) => yScale(group.max))
    .attr("y2", (group) => yScale(group.max))
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.2);

  const boxes = panelGroups
    .append("rect")
    .attr("x", xScale.bandwidth() * 0.18)
    .attr("width", xScale.bandwidth() * 0.64)
    .attr("y", innerHeight)
    .attr("height", 0)
    .attr("fill", (group) => CODON_GROUP_COLORS[group.teGroup] || "#859b7a")
    .attr("fill-opacity", 0.84)
    .attr("stroke", "rgba(58, 49, 38, 0.32)")
    .attr("stroke-width", 1.1)
    .on("mouseenter", function handleMouseEnter(event, group) {
      tooltip
        .html(
          `<div class="ribote-d3-tooltip__title">${panel?.codon || "Codon"} | ${group.teGroup}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Genes:</span><span class="ribote-d3-tooltip__value">${Number(group.n || 0).toLocaleString()}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Median:</span><span class="ribote-d3-tooltip__value">${formatPercent(group.median)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Mean:</span><span class="ribote-d3-tooltip__value">${formatPercent(group.mean)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Range:</span><span class="ribote-d3-tooltip__value">${formatPercent(group.min)} to ${formatPercent(group.max)}</span></div>`
        )
        .style("opacity", 1);
      positionTooltip(tooltip, container, event);
      d3.select(this).attr("stroke-width", 1.5);
    })
    .on("mousemove", (event) => {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function handleMouseLeave() {
      tooltip.style("opacity", 0);
      d3.select(this).attr("stroke-width", 1.1);
    });

  const medianLines = panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() * 0.18)
    .attr("x2", xScale.bandwidth() * 0.82)
    .attr("y1", innerHeight)
    .attr("y2", innerHeight)
    .attr("stroke", "#ffffff")
    .attr("stroke-width", 2);

  panelGroups
    .append("circle")
    .attr("cx", xScale.bandwidth() / 2)
    .attr("cy", (group) => yScale(group.mean))
    .attr("r", 3.6)
    .attr("fill", "#ffffff")
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.1);

  panelGroups
    .append("text")
    .attr("x", xScale.bandwidth() / 2)
    .attr("y", (group) => Math.max(14, yScale(group.max) - 12))
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-caption ribote-d3-caption--library")
    .text((group) => `n=${Number(group.n || 0).toLocaleString()}`);

  drawGroupLegend(chart, innerWidth, innerHeight + 82);

  const finalizeBoxes = (selection) => {
    selection
      .attr("y", (group) => yScale(group.q3))
      .attr("height", (group) => Math.max(2, yScale(group.q1) - yScale(group.q3)));
  };

  const finalizeMedians = (selection) => {
    selection
      .attr("y1", (group) => yScale(group.median))
      .attr("y2", (group) => yScale(group.median));
  };

  if (shouldAnimate) {
    boxes.transition().duration(420).ease(d3.easeCubicOut).call(finalizeBoxes);
    medianLines.transition().duration(420).ease(d3.easeCubicOut).call(finalizeMedians);
  } else {
    finalizeBoxes(boxes);
    finalizeMedians(medianLines);
  }

  return () => {
    tooltip.remove();
  };
}

export function drawCodonBiasGroupChart(container, panel, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  const groups = Array.isArray(panel?.groups) ? panel.groups : [];
  if (!groups.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No codon bias group summary is available.");
    return undefined;
  }

  const width = container.clientWidth || 760;
  const height = 392;
  const margin = { top: 90, right: 30, bottom: 118, left: 78 };
  const innerWidth = Math.max(200, width - margin.left - margin.right);
  const innerHeight = Math.max(160, height - margin.top - margin.bottom);
  const [yMin, yMax] = buildLinearDomain(
    groups.flatMap((group) => [group?.min, group?.max, group?.q1, group?.q3, group?.median, group?.mean]),
    { fallbackMin: 0, fallbackMax: 1, lowerPadRatio: 0.08, upperPadRatio: 0.14 }
  );
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const shouldAnimate = renderState.animate !== false;

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
    .text(panel?.metricLabel || "Codon Bias Metric");

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 34)
    .attr("text-anchor", "start")
    .attr("class", "ribote-d3-chart-subtitle")
    .text(`Genes measured: ${Number(panel?.genesMeasured || 0).toLocaleString()} | Up vs Non p: ${formatPValue(panel?.upVsNonPValue)} | Down vs Non p: ${formatPValue(panel?.downVsNonPValue)}`);

  const chart = svg.append("g").attr("transform", `translate(${margin.left}, ${margin.top})`);
  const clipId = nextClipId("ribote-codon-bias-group-clip");
  const xScale = d3.scaleBand().domain(CODON_GROUP_ORDER).range([0, innerWidth]).padding(0.32);
  const yScale = d3.scaleLinear().domain([yMin, yMax]).nice().range([innerHeight, 0]).clamp(true);

  appendPlotClip(svg, clipId, innerWidth, innerHeight);

  drawAxisWithGrid(chart, yScale, innerWidth);

  const xAxis = chart.append("g").attr("transform", `translate(0, ${innerHeight})`).call(d3.axisBottom(xScale));
  xAxis.select(".domain").attr("stroke", "#000000");
  xAxis.selectAll("text").attr("class", "ribote-d3-axis-tick");
  xAxis.selectAll("line").attr("stroke", "#000000");
  xAxis.selectAll("text").attr("dy", "1.1em");

  const yAxis = chart.append("g").call(d3.axisLeft(yScale).ticks(5));
  yAxis.select(".domain").attr("stroke", "#000000");
  yAxis.selectAll("text").attr("class", "ribote-d3-axis-tick");
  yAxis.selectAll("line").attr("stroke", "#000000");

  chart
    .append("text")
    .attr("x", innerWidth / 2)
    .attr("y", innerHeight + 44)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text("Translation Efficiency Group");

  chart
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("x", -innerHeight / 2)
    .attr("y", -56)
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-axis-label")
    .text(panel?.yLabel || panel?.metricLabel || "Metric Value");

  const groupLayer = chart.append("g").attr("clip-path", `url(#${clipId})`);
  const panelGroups = groupLayer
    .selectAll("g")
    .data(groups)
    .join("g")
    .attr("transform", (group) => `translate(${xScale(group.teGroup) || 0}, 0)`);

  panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() / 2)
    .attr("x2", xScale.bandwidth() / 2)
    .attr("y1", (group) => yScale(group.min))
    .attr("y2", (group) => yScale(group.max))
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.2);

  panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() * 0.24)
    .attr("x2", xScale.bandwidth() * 0.76)
    .attr("y1", (group) => yScale(group.min))
    .attr("y2", (group) => yScale(group.min))
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.2);

  panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() * 0.24)
    .attr("x2", xScale.bandwidth() * 0.76)
    .attr("y1", (group) => yScale(group.max))
    .attr("y2", (group) => yScale(group.max))
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.2);

  const boxes = panelGroups
    .append("rect")
    .attr("x", xScale.bandwidth() * 0.18)
    .attr("width", xScale.bandwidth() * 0.64)
    .attr("y", innerHeight)
    .attr("height", 0)
    .attr("fill", (group) => CODON_GROUP_COLORS[group.teGroup] || "#859b7a")
    .attr("fill-opacity", 0.84)
    .attr("stroke", "rgba(58, 49, 38, 0.32)")
    .attr("stroke-width", 1.1)
    .on("mouseenter", function handleMouseEnter(event, group) {
      tooltip
        .html(
          `<div class="ribote-d3-tooltip__title">${panel?.metricLabel || "Metric"} | ${group.teGroup}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Genes:</span><span class="ribote-d3-tooltip__value">${Number(group.n || 0).toLocaleString()}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Median:</span><span class="ribote-d3-tooltip__value">${formatNumber(group.median, 4)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Mean:</span><span class="ribote-d3-tooltip__value">${formatNumber(group.mean, 4)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Range:</span><span class="ribote-d3-tooltip__value">${formatNumber(group.min, 4)} to ${formatNumber(group.max, 4)}</span></div>`
        )
        .style("opacity", 1);
      positionTooltip(tooltip, container, event);
      d3.select(this).attr("stroke-width", 1.5);
    })
    .on("mousemove", (event) => {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function handleMouseLeave() {
      tooltip.style("opacity", 0);
      d3.select(this).attr("stroke-width", 1.1);
    });

  const medianLines = panelGroups
    .append("line")
    .attr("x1", xScale.bandwidth() * 0.18)
    .attr("x2", xScale.bandwidth() * 0.82)
    .attr("y1", innerHeight)
    .attr("y2", innerHeight)
    .attr("stroke", "#ffffff")
    .attr("stroke-width", 2);

  panelGroups
    .append("circle")
    .attr("cx", xScale.bandwidth() / 2)
    .attr("cy", (group) => yScale(group.mean))
    .attr("r", 3.6)
    .attr("fill", "#ffffff")
    .attr("stroke", "#3a3126")
    .attr("stroke-width", 1.1);

  chart
    .append("g")
    .selectAll("text")
    .data(groups)
    .join("text")
    .attr("transform", (group) => `translate(${xScale(group.teGroup) || 0}, 0)`)
    .attr("x", xScale.bandwidth() / 2)
    .attr("y", (group) => Math.max(14, yScale(group.max) - 12))
    .attr("text-anchor", "middle")
    .attr("class", "ribote-d3-caption ribote-d3-caption--library")
    .text((group) => `n=${Number(group.n || 0).toLocaleString()}`);

  drawGroupLegend(chart, innerWidth, innerHeight + 82);

  const finalizeBoxes = (selection) => {
    selection
      .attr("y", (group) => yScale(group.q3))
      .attr("height", (group) => Math.max(2, yScale(group.q1) - yScale(group.q3)));
  };

  const finalizeMedians = (selection) => {
    selection
      .attr("y1", (group) => yScale(group.median))
      .attr("y2", (group) => yScale(group.median));
  };

  if (shouldAnimate) {
    boxes.transition().duration(420).ease(d3.easeCubicOut).call(finalizeBoxes);
    medianLines.transition().duration(420).ease(d3.easeCubicOut).call(finalizeMedians);
  } else {
    finalizeBoxes(boxes);
    finalizeMedians(medianLines);
  }

  return () => {
    tooltip.remove();
  };
}

