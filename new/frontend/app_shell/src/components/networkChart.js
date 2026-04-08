import * as d3 from "d3";

const networkLayoutCache = new Map();

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

function fallbackModuleColor(moduleName) {
  if (!moduleName) {
    return "#859b7a";
  }

  const probe = document.createElement("span");
  probe.style.color = moduleName;
  const resolved = probe.style.color;

  if (!resolved) {
    return "#859b7a";
  }

  return moduleName;
}

function readCachedNetworkLayout(signature, nodes, width, height) {
  if (!signature) {
    return null;
  }

  const cached = networkLayoutCache.get(signature);
  if (!cached?.positions) {
    return null;
  }

  const positions = cached.positions;
  const widthScale = (cached.width && width) ? width / cached.width : 1;
  const heightScale = (cached.height && height) ? height / cached.height : 1;
  const positionedNodes = [];

  for (const node of nodes) {
    const cachedNode = positions[node.id];
    if (!cachedNode) {
      return null;
    }

    positionedNodes.push({
      ...node,
      x: cachedNode.x * widthScale,
      y: cachedNode.y * heightScale
    });
  }

  return positionedNodes;
}

function writeCachedNetworkLayout(signature, nodes, width, height) {
  if (!signature || !Array.isArray(nodes) || !nodes.length) {
    return;
  }

  networkLayoutCache.set(signature, {
    width,
    height,
    positions: Object.fromEntries(nodes.map((node) => [
      node.id,
      { x: Number(node.x) || 0, y: Number(node.y) || 0 }
    ]))
  });

  if (networkLayoutCache.size > 16) {
    const firstKey = networkLayoutCache.keys().next().value;
    if (firstKey) {
      networkLayoutCache.delete(firstKey);
    }
  }
}

export function normalizeNetworkNodes(nodes) {
  if (!Array.isArray(nodes)) {
    return [];
  }

  return nodes.map((node, index) => ({
    id: String(node?.id || `node-${index + 1}`),
    label: String(node?.label || node?.geneName || node?.geneId || `Node ${index + 1}`),
    geneId: String(node?.geneId || node?.id || ""),
    geneName: String(node?.geneName || ""),
    module: String(node?.module || ""),
    moduleIndex: Number(node?.moduleIndex),
    connectivity: Number(node?.connectivity),
    degree: Number(node?.degree),
    displayDegree: Number(node?.displayDegree)
  }));
}

export function normalizeNetworkEdges(edges) {
  if (!Array.isArray(edges)) {
    return [];
  }

  return edges
    .map((edge) => ({
      source: String(edge?.source || ""),
      target: String(edge?.target || ""),
      weight: Number(edge?.weight)
    }))
    .filter((edge) => edge.source && edge.target && Number.isFinite(edge.weight));
}

export function drawNetworkGraph(container, graph, renderState = {}) {
  const nodes = normalizeNetworkNodes(graph?.nodes);
  const edges = normalizeNetworkEdges(graph?.edges);
  const root = d3.select(container);
  root.selectAll("*").remove();
  root.style("position", "relative");

  if (!nodes.length) {
    root.append("div").attr("class", "ribote-d3-empty").text("No network graph is available.");
    return undefined;
  }

  const width = container.clientWidth || 960;
  const title = graph?.title || "Network Graph";
  const subtitle = graph?.subtitle || "";
  const showLabels = graph?.showLabels !== false;
  const allowDrag = graph?.dragEnabled !== false;
  const tooltip = root.append("div").attr("class", "ribote-d3-tooltip").style("opacity", 0);
  const uniqueModules = Array.from(new Set(nodes.map((node) => node.module).filter(Boolean)));
  const nodeMap = new Map(nodes.map((node) => [node.id, { ...node }]));
  const linkData = edges
    .filter((edge) => nodeMap.has(edge.source) && nodeMap.has(edge.target))
    .map((edge) => ({ ...edge }));
  const denseGraph = nodes.length > 60 || linkData.length > 220;
  const veryDenseGraph = nodes.length > 100 || linkData.length > 360;
  const height = veryDenseGraph
    ? Math.max(860, Math.min(1320, 620 + nodes.length * 5.1))
    : denseGraph
      ? Math.max(700, Math.min(1120, 540 + nodes.length * 4.4))
      : Math.max(520, Math.min(860, 460 + nodes.length * 2));
  const shouldAnimate = renderState.animate !== false && nodes.length <= 40 && linkData.length <= 220;
  const radiusScale = d3
    .scaleSqrt()
    .domain([0, d3.max(nodes, (node) => Math.max(node.degree || 0, 1)) || 1])
    .range([4.2, 9.4]);
  const initialOrbitFactor = veryDenseGraph ? 0.16 : denseGraph ? 0.22 : 0.3;
  const baseLinkDistance = veryDenseGraph ? 164 : denseGraph ? 186 : 212;
  const minLinkDistance = veryDenseGraph ? 44 : denseGraph ? 54 : 68;
  const collisionPadding = veryDenseGraph ? 9 : denseGraph ? 12 : 20;
  const chargeFloor = veryDenseGraph ? -720 : denseGraph ? -860 : -1040;
  const chargeStrength = veryDenseGraph
    ? Math.max(chargeFloor, -150 - nodes.length * 0.95)
    : denseGraph
      ? Math.max(chargeFloor, -180 - nodes.length * 1.15)
      : Math.max(chargeFloor, -240 - nodes.length * 1.8);
  const cachedLayoutNodes = readCachedNetworkLayout(graph?.signature, Array.from(nodeMap.values()), width, height);
  const simulationNodes = cachedLayoutNodes || Array.from(nodeMap.values()).map((node, index) => ({
    ...node,
    x: width / 2 + Math.cos((index / Math.max(nodes.length, 1)) * Math.PI * 2) * Math.min(width, height) * initialOrbitFactor,
    y: height / 2 + Math.sin((index / Math.max(nodes.length, 1)) * Math.PI * 2) * Math.min(width, height) * initialOrbitFactor
  }));
  const nodeById = new Map(simulationNodes.map((node) => [node.id, node]));

  const simulationLinks = linkData.map((edge) => ({
    ...edge,
    source: nodeById.get(edge.source) || edge.source,
    target: nodeById.get(edge.target) || edge.target
  }));

  const simulation = d3
    .forceSimulation(simulationNodes)
    .force(
      "link",
      d3.forceLink(simulationLinks)
        .id((node) => node.id)
        .distance((edge) => {
          const weight = Number(edge.weight);
          if (!Number.isFinite(weight)) {
            return baseLinkDistance * 0.72;
          }

          return Math.max(minLinkDistance, baseLinkDistance - weight * 104);
        })
        .strength((edge) => {
          const weight = Number(edge.weight);
          if (!Number.isFinite(weight)) {
            return 0.18;
          }

          return Math.max(0.1, Math.min(0.72, weight * 0.9));
        })
    )
    .force("charge", d3.forceManyBody().strength(chargeStrength))
    .force("center", d3.forceCenter(width / 2, height / 2 + 16))
    .force("collision", d3.forceCollide().radius((node) => radiusScale(node.degree || 0) + collisionPadding));

  simulation.stop();
  const tickCount = cachedLayoutNodes
    ? 16
    : nodes.length > 90 || linkData.length > 320
      ? 54
      : nodes.length > 50 || linkData.length > 180
        ? 78
        : 110;
  for (let tick = 0; tick < tickCount; tick += 1) {
    simulation.tick();
  }
  writeCachedNetworkLayout(graph?.signature, simulationNodes, width, height);

  const svg = root.append("svg").attr("viewBox", `0 0 ${width} ${height}`).attr("class", "ribote-d3-chart");

  svg
    .append("text")
    .attr("x", 0)
    .attr("y", 18)
    .attr("text-anchor", "start")
    .attr("class", "ribote-d3-chart-title ribote-d3-chart-title--library")
    .text(title);

  if (subtitle) {
    svg
      .append("text")
      .attr("x", 0)
      .attr("y", 40)
      .attr("text-anchor", "start")
      .attr("class", "ribote-d3-chart-subtitle")
      .text(subtitle);
  }

  if (uniqueModules.length) {
    const legend = svg.append("g").attr("transform", `translate(0, ${subtitle ? 62 : 48})`);
    const legendItemWidth = 142;

    uniqueModules.forEach((moduleName, index) => {
      const group = legend.append("g").attr("transform", `translate(${index * legendItemWidth}, 0)`);

      group
        .append("circle")
        .attr("cx", 8)
        .attr("cy", 0)
        .attr("r", 8)
        .attr("fill", fallbackModuleColor(moduleName))
        .attr("stroke", "rgba(0,0,0,0.16)")
        .attr("stroke-width", 1);

      group
        .append("text")
        .attr("x", 24)
        .attr("y", 5)
        .attr("class", "ribote-d3-legend ribote-d3-legend--library")
        .text(moduleName);
    });
  }

  const topOffset = uniqueModules.length ? 88 : (subtitle ? 58 : 40);
  const chart = svg.append("g").attr("transform", `translate(0, ${topOffset})`);
  const visibleHeight = height - topOffset;
  const clipId = `ribote-network-clip-${Math.round(Math.random() * 1e9)}`;

  svg
    .append("defs")
    .append("clipPath")
    .attr("id", clipId)
    .append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", width)
    .attr("height", visibleHeight);

  const zoomViewport = chart
    .append("g")
    .attr("clip-path", `url(#${clipId})`);

  const interactionRect = zoomViewport
    .append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", width)
    .attr("height", visibleHeight)
    .attr("fill", "transparent")
    .style("cursor", "grab");

  const zoomLayer = zoomViewport.append("g");

  const weightExtent = d3.extent(linkData, (edge) => edge.weight);
  const strokeScale = d3
    .scaleLinear()
    .domain([
      Number.isFinite(weightExtent[0]) ? weightExtent[0] : 0,
      Number.isFinite(weightExtent[1]) ? weightExtent[1] : 1
    ])
    .range([0.55, 2.6]);

  const opacityScale = d3
    .scaleLinear()
    .domain([
      Number.isFinite(weightExtent[0]) ? weightExtent[0] : 0,
      Number.isFinite(weightExtent[1]) ? weightExtent[1] : 1
    ])
    .range([0.18, 0.72]);

  const linkGroup = zoomLayer.append("g");
  const nodeGroup = zoomLayer.append("g");
  const labelGroup = zoomLayer.append("g");

  const links = linkGroup
    .selectAll("line")
    .data(simulationLinks)
    .enter()
    .append("line")
    .attr("x1", (edge) => edge.source.x)
    .attr("y1", (edge) => edge.source.y)
    .attr("x2", (edge) => edge.target.x)
    .attr("y2", (edge) => edge.target.y)
    .attr("stroke", "rgba(96, 125, 109, 0.75)")
    .attr("stroke-linecap", "round")
    .attr("stroke-opacity", (edge) => opacityScale(edge.weight))
    .attr("stroke-width", shouldAnimate ? 0 : (edge) => strokeScale(edge.weight));

  if (shouldAnimate) {
    links
      .transition()
      .duration(520)
      .ease(d3.easeCubicOut)
      .attr("stroke-width", (edge) => strokeScale(edge.weight));
  }

  const circles = nodeGroup
    .selectAll("circle")
    .data(simulationNodes)
    .enter()
    .append("circle")
    .attr("cx", (node) => node.x)
    .attr("cy", (node) => node.y)
    .attr("r", shouldAnimate ? 0 : (node) => radiusScale(node.degree || 0))
    .attr("fill", (node) => fallbackModuleColor(node.module))
    .attr("fill-opacity", 0.9)
    .attr("stroke", "rgba(255,255,255,0.92)")
    .attr("stroke-width", 1.6)
    .on("mouseenter", function(event, node) {
      d3.select(this).attr("stroke-width", 2.6);
      tooltip
        .style("opacity", 1)
        .html(
          `<div class="ribote-d3-tooltip__title">${node.label}</div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">GeneID:</span><span class="ribote-d3-tooltip__value">${node.geneId}</span></div>
          ${node.geneName && node.geneName !== "unknown" ? `<div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Gene Name:</span><span class="ribote-d3-tooltip__value">${node.geneName}</span></div>` : ""}
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Module:</span><span class="ribote-d3-tooltip__value">${node.module || "unassigned"}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Connectivity:</span><span class="ribote-d3-tooltip__value">${d3.format(".4f")(node.connectivity || 0)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Degree (thresholded):</span><span class="ribote-d3-tooltip__value">${Math.round(node.degree || 0)}</span></div>
          <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Degree (displayed):</span><span class="ribote-d3-tooltip__value">${Math.round(node.displayDegree || 0)}</span></div>`
        );
      positionTooltip(tooltip, container, event);
    })
    .on("mousemove", function(event) {
      positionTooltip(tooltip, container, event);
    })
    .on("mouseleave", function() {
      d3.select(this).attr("stroke-width", 1.6);
      tooltip.style("opacity", 0);
    });

  if (shouldAnimate) {
    circles
      .transition()
      .duration(650)
      .ease(d3.easeCubicOut)
      .attr("r", (node) => radiusScale(node.degree || 0));
  }

  const labeledNodeIds = new Set(
    [...simulationNodes]
      .sort((left, right) => (right.connectivity || 0) - (left.connectivity || 0))
      .slice(0, nodes.length <= 80 ? nodes.length : 80)
      .map((node) => node.id)
  );

  if (showLabels) {
    labelGroup
      .style("pointer-events", "none")
      .selectAll("text")
      .data(simulationNodes.filter((node) => labeledNodeIds.has(node.id)))
      .enter()
      .append("text")
      .attr("x", (node) => node.x + radiusScale(node.degree || 0) + 4)
      .attr("y", (node) => node.y + 3)
      .attr("class", "ribote-d3-axis-tick")
      .attr("font-size", 7.2)
      .attr("font-weight", 700)
      .attr("paint-order", "stroke")
      .attr("stroke", "rgba(255,252,246,0.94)")
      .attr("stroke-width", 1.35)
      .attr("stroke-linejoin", "round")
      .text((node) => node.label);
  }

  const updateGraphPositions = () => {
    links
      .attr("x1", (edge) => edge.source.x)
      .attr("y1", (edge) => edge.source.y)
      .attr("x2", (edge) => edge.target.x)
      .attr("y2", (edge) => edge.target.y);

    circles
      .attr("cx", (node) => node.x)
      .attr("cy", (node) => node.y);

    labelGroup
      .selectAll("text")
      .attr("x", (node) => node.x + radiusScale(node.degree || 0) + 4)
      .attr("y", (node) => node.y + 3);
  };

  let dragFrameId = null;
  const schedulePositionUpdate = () => {
    if (dragFrameId !== null) {
      return;
    }

    dragFrameId = window.requestAnimationFrame(() => {
      dragFrameId = null;
      updateGraphPositions();
      writeCachedNetworkLayout(graph?.signature, simulationNodes, width, height);
    });
  };

  if (allowDrag) {
    const dragBehavior = d3.drag()
      .container(() => zoomLayer.node())
      .on("start", (event) => {
        event.sourceEvent?.stopPropagation?.();
        interactionRect.style("cursor", "grabbing");
      })
      .on("drag", (event, node) => {
        node.x = event.x;
        node.y = event.y;
        schedulePositionUpdate();
      })
      .on("end", (event) => {
        event.sourceEvent?.stopPropagation?.();
        if (dragFrameId !== null) {
          window.cancelAnimationFrame(dragFrameId);
          dragFrameId = null;
        }
        updateGraphPositions();
        writeCachedNetworkLayout(graph?.signature, simulationNodes, width, height);
        interactionRect.style("cursor", "grab");
      });

    circles
      .style("cursor", "move")
      .call(dragBehavior);
  } else {
    circles.style("cursor", "default");
  }

  const xExtent = d3.extent(simulationNodes, (node) => node.x) || [width / 2, width / 2];
  const yExtent = d3.extent(simulationNodes, (node) => node.y) || [visibleHeight / 2, visibleHeight / 2];
  const radiusExtent = d3.max(simulationNodes, (node) => radiusScale(node.degree || 0)) || 8;
  const bounds = {
    minX: xExtent[0] - radiusExtent - 18,
    maxX: xExtent[1] + radiusExtent + (showLabels ? 72 : 18),
    minY: yExtent[0] - radiusExtent - 18,
    maxY: yExtent[1] + radiusExtent + 18
  };
  const boundsWidth = Math.max(1, bounds.maxX - bounds.minX);
  const boundsHeight = Math.max(1, bounds.maxY - bounds.minY);
  const fitPadding = veryDenseGraph ? 14 : denseGraph ? 18 : 28;
  const fitScale = Math.max(
    0.08,
    Math.min(
      4.5,
      Math.min(
        (width - fitPadding * 2) / boundsWidth,
        (visibleHeight - fitPadding * 2) / boundsHeight
      )
    )
  );
  const fitTranslateX = (width - boundsWidth * fitScale) / 2 - bounds.minX * fitScale;
  const fitTranslateY = (visibleHeight - boundsHeight * fitScale) / 2 - bounds.minY * fitScale;
  const initialTransform = d3.zoomIdentity.translate(fitTranslateX, fitTranslateY).scale(fitScale);
  const minZoomScale = Math.max(0.02, Math.min(0.18, fitScale * 0.45));

  const zoom = d3.zoom()
    .scaleExtent([minZoomScale, 10])
    .translateExtent([
      [-width * 2, -visibleHeight * 2],
      [width * 3, visibleHeight * 3]
    ])
    .on("start", () => {
      interactionRect.style("cursor", "grabbing");
    })
    .on("zoom", (event) => {
      zoomLayer.attr("transform", event.transform);
    })
    .on("end", () => {
      interactionRect.style("cursor", "grab");
    });

  const applyFitView = (animate = false) => {
    const target = animate ? zoomViewport.transition().duration(220).ease(d3.easeCubicOut) : zoomViewport;
    target.call(zoom.transform, initialTransform);
  };

  const handleFitView = () => {
    applyFitView(true);
  };

  container.addEventListener("ribote:network-fit-view", handleFitView);

  zoomViewport
    .style("touch-action", "none")
    .call(zoom)
    .call(zoom.transform, initialTransform);

  return () => {
    if (dragFrameId !== null) {
      window.cancelAnimationFrame(dragFrameId);
    }
    container.removeEventListener("ribote:network-fit-view", handleFitView);
    zoomViewport.on(".zoom", null);
    simulation.stop();
    tooltip.remove();
  };
}
