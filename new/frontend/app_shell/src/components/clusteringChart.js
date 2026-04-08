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

function sampleGroupColor(group) {
  if (group === "Treatment") {
    return "#cd7468";
  }

  return "#6f88bc";
}

function normalizeColumns(columns) {
  if (!Array.isArray(columns)) {
    return [];
  }

  return columns.map((column, index) => ({
    key: String(column?.displaySample || `sample-${index + 1}`),
    displaySample: String(column?.displaySample || `Sample ${index + 1}`),
    actualSample: String(column?.actualSample || ""),
    actualRna: String(column?.actualRna || ""),
    actualRibo: String(column?.actualRibo || ""),
    group: String(column?.group || "")
  }));
}

export function normalizeClusteringHeatmap(heatmap) {
  const rows = Array.isArray(heatmap?.rowLabels)
    ? heatmap.rowLabels.map((value) => String(value ?? ""))
    : [];
  const columns = normalizeColumns(heatmap?.columns);
  const matrix = Array.isArray(heatmap?.matrix)
    ? heatmap.matrix.map((row) =>
      (Array.isArray(row) ? row : Object.values(row || {})).map((value) => Number(value))
    )
    : [];

  return {
    title: String(heatmap?.title || ""),
    subtitle: String(heatmap?.subtitle || ""),
    signature: String(heatmap?.signature || ""),
    palette: Array.isArray(heatmap?.palette) ? heatmap.palette.map((color) => String(color)) : ["#4b74b6", "#ffffff", "#c23b35"],
    rows,
    columns,
    matrix,
    showRowLabels: Boolean(heatmap?.showRowLabels),
    brushEnabled: Boolean(heatmap?.brushEnabled)
  };
}

function computeLabelStep(labelCount, maxLabels = 40) {
  if (!Number.isFinite(labelCount) || labelCount <= maxLabels) {
    return 1;
  }

  return Math.max(1, Math.ceil(labelCount / maxLabels));
}

function valueDomain(values) {
  const numeric = values.filter((value) => Number.isFinite(value));
  if (!numeric.length) {
    return [-1, 0, 1];
  }

  let minValue = d3.min(numeric);
  let maxValue = d3.max(numeric);

  if (minValue === maxValue) {
    const delta = Math.max(Math.abs(minValue) * 0.2, 1);
    minValue -= delta;
    maxValue += delta;
  }

  if (minValue < 0 && maxValue > 0) {
    return [minValue, 0, maxValue];
  }

  return [minValue, d3.median(numeric), maxValue];
}

function buildColorScale(values, palette) {
  const domain = valueDomain(values);
  const colors = palette.length >= 3 ? palette.slice(0, 3) : ["#4b74b6", "#ffffff", "#c23b35"];

  return d3.scaleLinear()
    .domain(domain)
    .range(colors)
    .clamp(true);
}

function bandSelection(domain, scale, startPx, endPx) {
  const minPx = Math.min(startPx, endPx);
  const maxPx = Math.max(startPx, endPx);
  const selected = [];

  domain.forEach((key, index) => {
    const position = scale(key);
    const bandWidth = scale.bandwidth();

    if (!Number.isFinite(position)) {
      return;
    }

    const bandStart = position;
    const bandEnd = position + bandWidth;

    if (bandEnd >= minPx && bandStart <= maxPx) {
      selected.push(index);
    }
  });

  if (!selected.length) {
    return null;
  }

  return {
    start: selected[0] + 1,
    end: selected[selected.length - 1] + 1
  };
}

function activeCellMatches(activeCell, cell) {
  if (!activeCell) {
    return false;
  }

  return activeCell.gene === cell.gene && activeCell.displaySample === cell.column.displaySample;
}

function drawPlotFrame(target, width, height) {
  target
    .append("line")
    .attr("x1", 0)
    .attr("x2", width)
    .attr("y1", 0)
    .attr("y2", 0)
    .attr("stroke", "#000000")
    .attr("stroke-width", 1);

  target
    .append("line")
    .attr("x1", width)
    .attr("x2", width)
    .attr("y1", 0)
    .attr("y2", height)
    .attr("stroke", "#000000")
    .attr("stroke-width", 1);

  target
    .append("line")
    .attr("x1", 0)
    .attr("x2", width)
    .attr("y1", height)
    .attr("y2", height)
    .attr("stroke", "#000000")
    .attr("stroke-width", 1);

  target
    .append("line")
    .attr("x1", 0)
    .attr("x2", 0)
    .attr("y1", 0)
    .attr("y2", height)
    .attr("stroke", "#000000")
    .attr("stroke-width", 1);
}

export function drawClusteringHeatmap(container, heatmap, options = {}, renderState = {}) {
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  const rows = heatmap?.rows || [];
  const columns = heatmap?.columns || [];
  const matrix = heatmap?.matrix || [];

  if (!rows.length || !columns.length || !matrix.length) {
    root.append("div").attr("class", "ribote-d3-empty").text(options.emptyMessage || "No heatmap data available.");
    return undefined;
  }

  const width = container.clientWidth || 960;
  const showRowLabels = heatmap.showRowLabels === true;
  const longestRowLabel = d3.max(rows, (label) => String(label || "").length) || 0;
  const leftMargin = showRowLabels
    ? Math.min(200, Math.max(94, longestRowLabel * 7.2))
    : 24;
  const chartHeight = Number(options.chartHeight)
    || (showRowLabels ? Math.min(980, Math.max(500, rows.length * 14 + 240)) : 780);
  const margin = {
    top: 78,
    right: 28,
    bottom: 68,
    left: leftMargin
  };
  const annotationHeight = 18;
  const annotationGap = 10;
  const innerWidth = Math.max(120, width - margin.left - margin.right);
  const innerHeight = Math.max(180, chartHeight - margin.top - margin.bottom);
  const heatmapHeight = Math.max(120, innerHeight - annotationHeight - annotationGap);
  const shouldAnimate = renderState.animate !== false;
  const flatValues = matrix.flat().filter((value) => Number.isFinite(value));
  const colorScale = buildColorScale(flatValues, heatmap.palette);
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const columnDomain = columns.map((column) => column.displaySample);
  const rowDomain = rows.slice();
  const x = d3.scaleBand().domain(columnDomain).range([0, innerWidth]).paddingInner(0.01).paddingOuter(0);
  const y = d3.scaleBand().domain(rowDomain).range([0, heatmapHeight]).paddingInner(0).paddingOuter(0);
  const labelStep = computeLabelStep(rows.length, 42);

  const svg = root
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${chartHeight}`)
    .attr("class", "ribote-d3-chart");

  const titleText = heatmap.subtitle
    ? `${heatmap.title || options.title || "Clustered Heatmap"} (${heatmap.subtitle})`
    : (heatmap.title || options.title || "Clustered Heatmap");

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 14)
    .attr("class", "ribote-d3-chart-title ribote-d3-chart-title--library")
    .text(titleText);

  const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
  const annotation = chart.append("g").attr("class", "ribote-clustering-annotation");

  const legendItems = [
    { label: "Treatment", color: sampleGroupColor("Treatment") },
    { label: "Control", color: sampleGroupColor("Control") }
  ];

  const legendItemWidth = 132;
  const legendTotalWidth = legendItems.length * legendItemWidth;
  const legendOffsetX = Math.max(0, (innerWidth - legendTotalWidth) / 2);

  const legend = annotation
    .append("g")
    .attr("class", "ribote-clustering-legend")
    .attr("transform", `translate(${legendOffsetX},-10)`);

  const legendItem = legend
    .selectAll("g")
    .data(legendItems)
    .enter()
    .append("g")
    .attr("class", "ribote-clustering-legend__item")
    .attr("transform", (_, index) => `translate(${index * legendItemWidth},0)`);

  legendItem
    .append("rect")
    .attr("x", 0)
    .attr("y", -11)
    .attr("width", 16)
    .attr("height", 10)
    .attr("rx", 2)
    .attr("fill", (item) => item.color)
    .attr("stroke", "rgba(0, 0, 0, 0.18)")
    .attr("stroke-width", 0.5);

  legendItem
    .append("text")
    .attr("x", 24)
    .attr("y", -3)
    .attr("class", "ribote-d3-caption ribote-d3-caption--library")
    .text((item) => item.label);

  const sampleBand = annotation
    .append("g")
    .attr("class", "ribote-clustering-sample-band");

  sampleBand
    .selectAll("rect")
    .data(columns)
    .enter()
    .append("rect")
    .attr("x", (column) => x(column.displaySample) || 0)
    .attr("y", 0)
    .attr("width", Math.max(1, x.bandwidth()))
    .attr("height", annotationHeight)
    .attr("rx", 2)
    .attr("fill", (column) => sampleGroupColor(column.group))
    .attr("opacity", 0.88);

  const plot = chart.append("g").attr("transform", `translate(0,${annotationHeight + annotationGap})`);

  plot
    .append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", innerWidth)
    .attr("height", heatmapHeight)
    .attr("fill", "#ffffff")
    .attr("stroke", "#000000")
    .attr("stroke-width", 1);

  const cells = [];
  matrix.forEach((row, rowIndex) => {
    row.forEach((value, columnIndex) => {
      const column = columns[columnIndex];
      if (!column) {
        return;
      }

      cells.push({
        gene: rows[rowIndex],
        value,
        rowIndex,
        columnIndex,
        column
      });
    });
  });

  const cellRects = plot
    .append("g")
    .selectAll("rect")
    .data(cells)
    .enter()
    .append("rect")
    .attr("x", (cell) => x(cell.column.displaySample) || 0)
    .attr("y", (cell) => y(cell.gene) || 0)
    .attr("width", Math.max(1, x.bandwidth()))
    .attr("height", Math.max(1, y.bandwidth()))
    .attr("fill", (cell) => colorScale(cell.value))
    .attr("stroke", (cell) => (activeCellMatches(options.activeCell, cell) ? "#1f1f1f" : "none"))
    .attr("stroke-width", (cell) => (activeCellMatches(options.activeCell, cell) ? 1.4 : 0))
    .style("cursor", typeof options.onCellClick === "function" ? "pointer" : "default")
    .on("mouseenter", function(event, cell) {
      d3.select(this).attr("stroke", "#202020").attr("stroke-width", 1.2);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${cell.gene}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Sample:</span><span class="ribote-d3-tooltip__value">${cell.column.displaySample}</span></div>
          ${cell.column.actualSample ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Actual:</span><span class="ribote-d3-tooltip__value">${cell.column.actualSample}</span></div>` : ""}
          ${cell.column.actualRna ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">RNA:</span><span class="ribote-d3-tooltip__value">${cell.column.actualRna}</span></div>` : ""}
          ${cell.column.actualRibo ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Ribo:</span><span class="ribote-d3-tooltip__value">${cell.column.actualRibo}</span></div>` : ""}
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Group:</span><span class="ribote-d3-tooltip__value">${cell.column.group || "Sample"}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Value:</span><span class="ribote-d3-tooltip__value">${d3.format(".4f")(cell.value)}</span></div>`
        );
      positionTooltip(tooltip, container, event);
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function(event, cell) {
      d3.select(this)
        .attr("stroke", activeCellMatches(options.activeCell, cell) ? "#1f1f1f" : "none")
        .attr("stroke-width", activeCellMatches(options.activeCell, cell) ? 1.4 : 0);
      tooltip.style("opacity", 0);
    })
    .on("click", function(event, cell) {
      if (typeof options.onCellClick === "function") {
        options.onCellClick({
          gene: cell.gene,
          displaySample: cell.column.displaySample,
          actualSample: cell.column.actualSample,
          actualRna: cell.column.actualRna,
          actualRibo: cell.column.actualRibo,
          group: cell.column.group,
          value: cell.value
        });
      }
    });

  if (shouldAnimate) {
    cellRects
      .attr("opacity", 0)
      .transition()
      .duration(420)
      .ease(d3.easeCubicOut)
      .attr("opacity", 1);
  }

  const xAxis = chart.append("g").attr("transform", `translate(0,${annotationHeight + annotationGap + heatmapHeight})`);

  xAxis
    .selectAll("text")
    .data(columns)
    .enter()
    .append("text")
    .attr("x", (column) => (x(column.displaySample) || 0) + x.bandwidth() / 2)
    .attr("y", 10)
    .attr("text-anchor", "middle")
    .attr("dominant-baseline", "hanging")
    .attr("class", "ribote-d3-axis-tick")
    .attr("font-size", 12)
    .text((column) => column.displaySample);

  if (showRowLabels) {
    const yAxis = chart.append("g").attr("transform", `translate(-10,${annotationHeight + annotationGap})`);

    yAxis
      .selectAll("text")
      .data(rows.filter((_, index) => index % labelStep === 0))
      .enter()
      .append("text")
      .attr("x", 0)
      .attr("y", (label) => (y(label) || 0) + Math.max(y.bandwidth() / 2, 6))
      .attr("text-anchor", "end")
      .attr("dominant-baseline", "middle")
      .attr("class", "ribote-d3-axis-tick")
      .attr("font-size", 11)
      .text((label) => label);
  }

  if (heatmap.brushEnabled && typeof options.onBrushSelection === "function") {
    const brush = d3.brush()
      .extent([[0, 0], [innerWidth, heatmapHeight]])
      .on("end", ({ selection }) => {
        if (!selection) {
          return;
        }

        const xSelection = bandSelection(columnDomain, x, selection[0][0], selection[1][0]);
        const ySelection = bandSelection(rowDomain, y, selection[0][1], selection[1][1]);

        if (!xSelection || !ySelection) {
          plot.select(".ribote-clustering-brush").call(brush.move, null);
          return;
        }

        options.onBrushSelection({
          rowStart: ySelection.start,
          rowEnd: ySelection.end,
          colStart: xSelection.start,
          colEnd: xSelection.end,
          nonce: Date.now()
        });

        plot.select(".ribote-clustering-brush").call(brush.move, null);
      });

    const brushGroup = plot.append("g").attr("class", "ribote-clustering-brush");
    brushGroup.call(brush);
    brushGroup
      .selectAll(".selection")
      .attr("fill", "rgba(29, 110, 98, 0.6)")
      .attr("stroke", "#165c52")
      .attr("stroke-width", 1.9)
      .attr("shape-rendering", "crispEdges");
    brushGroup.selectAll(".handle").attr("display", "none");
    brushGroup.selectAll(".overlay").style("cursor", "crosshair");
  }

  drawPlotFrame(plot.append("g").attr("class", "ribote-clustering-frame"), innerWidth, heatmapHeight);

  return () => {
    tooltip.remove();
  };
}
