import html2canvas from "html2canvas";
import { jsPDF } from "jspdf";
import JSZip from "jszip";
import { drawPcaProjectionChart, normalizePcaPoints } from "../components/pcaChart";
import { vectorPdfFromSvgNode } from "./svgVectorPdf";

const EXPORT_BACKGROUND = "#ffffff";
const EXPORT_ROOT_SELECTOR = [
  ".ribote-d3-card",
  ".ribote-d3-host",
  "svg"
].join(", ");

function dispatchActionLockEvent() {
  window.dispatchEvent(new CustomEvent("rnameta:action-lock"));
}

function dispatchActionUnlockEvent() {
  window.dispatchEvent(new CustomEvent("rnameta:action-unlock"));
}

function downloadBlob(blob, filename) {
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  link.remove();
  window.setTimeout(() => URL.revokeObjectURL(url), 1000);
}

function resolveExportElement(hostId, selector = null) {
  const host = document.getElementById(hostId);

  if (!host) {
    return null;
  }

  if (selector) {
    return host.querySelector(selector);
  }

  return host.querySelector(EXPORT_ROOT_SELECTOR) || host.querySelector("svg") || host;
}

function resolveExportSvg(exportElement) {
  if (!exportElement) {
    return null;
  }

  if (exportElement instanceof SVGSVGElement) {
    return exportElement;
  }

  return exportElement.querySelector("svg");
}

function fitExportSize(contentWidth, contentHeight, maxWidthPx, maxHeightPx) {
  const widthScale = maxWidthPx / contentWidth;
  const heightScale = maxHeightPx / contentHeight;
  const scale = Math.max(0.25, Math.min(widthScale, heightScale));

  return {
    scale,
    widthPx: Math.max(1, Math.round(contentWidth * scale)),
    heightPx: Math.max(1, Math.round(contentHeight * scale))
  };
}

function trimCanvasWhitespace(canvas, options = {}) {
  const padding = Math.max(0, Math.round(options.padding ?? 0));
  const background = options.backgroundColor || EXPORT_BACKGROUND;
  const cropXEnabled = options.cropX !== false;
  const cropYEnabled = options.cropY !== false;
  const context = canvas.getContext("2d");

  if (!context) {
    return canvas;
  }

  const imageData = context.getImageData(0, 0, canvas.width, canvas.height);
  const pixels = imageData.data;
  const probe = document.createElement("canvas");
  probe.width = 1;
  probe.height = 1;
  const probeContext = probe.getContext("2d");

  if (!probeContext) {
    return canvas;
  }

  probeContext.fillStyle = background;
  probeContext.fillRect(0, 0, 1, 1);
  const backgroundPixel = probeContext.getImageData(0, 0, 1, 1).data;
  const threshold = 6;
  let minX = canvas.width;
  let minY = canvas.height;
  let maxX = -1;
  let maxY = -1;

  for (let y = 0; y < canvas.height; y += 1) {
    for (let x = 0; x < canvas.width; x += 1) {
      const offset = (y * canvas.width + x) * 4;
      const alpha = pixels[offset + 3];

      if (alpha === 0) {
        continue;
      }

      const differs =
        Math.abs(pixels[offset] - backgroundPixel[0]) > threshold ||
        Math.abs(pixels[offset + 1] - backgroundPixel[1]) > threshold ||
        Math.abs(pixels[offset + 2] - backgroundPixel[2]) > threshold ||
        Math.abs(alpha - backgroundPixel[3]) > threshold;

      if (!differs) {
        continue;
      }

      if (x < minX) minX = x;
      if (y < minY) minY = y;
      if (x > maxX) maxX = x;
      if (y > maxY) maxY = y;
    }
  }

  if (maxX < minX || maxY < minY) {
    return canvas;
  }

  const cropX = cropXEnabled ? Math.max(0, minX - padding) : 0;
  const cropY = cropYEnabled ? Math.max(0, minY - padding) : 0;
  const cropWidth = cropXEnabled
    ? Math.min(canvas.width - cropX, maxX - minX + 1 + padding * 2)
    : canvas.width;
  const cropHeight = cropYEnabled
    ? Math.min(canvas.height - cropY, maxY - minY + 1 + padding * 2)
    : canvas.height;
  const trimmed = document.createElement("canvas");
  trimmed.width = Math.max(1, cropWidth);
  trimmed.height = Math.max(1, cropHeight);
  const trimmedContext = trimmed.getContext("2d");

  if (!trimmedContext) {
    return canvas;
  }

  trimmedContext.fillStyle = background;
  trimmedContext.fillRect(0, 0, trimmed.width, trimmed.height);
  trimmedContext.drawImage(canvas, cropX, cropY, cropWidth, cropHeight, 0, 0, cropWidth, cropHeight);

  return trimmed;
}

function getSvgIntrinsicSize(svgElement) {
  const viewBox = svgElement.viewBox?.baseVal;
  if (viewBox && viewBox.width > 0 && viewBox.height > 0) {
    return { width: viewBox.width, height: viewBox.height };
  }

  const widthAttr = Number(svgElement.getAttribute("width"));
  const heightAttr = Number(svgElement.getAttribute("height"));
  if (widthAttr > 0 && heightAttr > 0) {
    return { width: widthAttr, height: heightAttr };
  }

  const bounds = svgElement.getBoundingClientRect();
  return {
    width: Math.max(1, bounds.width),
    height: Math.max(1, bounds.height)
  };
}

function normalizeExportPadding(padding = null) {
  return {
    top: Math.max(0, Number(padding?.top) || 0),
    right: Math.max(0, Number(padding?.right) || 0),
    bottom: Math.max(0, Number(padding?.bottom) || 0),
    left: Math.max(0, Number(padding?.left) || 0)
  };
}

const SVG_COMPUTED_STYLE_PROPS = [
  "font-family",
  "font-size",
  "font-weight",
  "font-style",
  "letter-spacing",
  "word-spacing",
  "text-anchor",
  "dominant-baseline",
  "alignment-baseline",
  "fill",
  "fill-opacity",
  "stroke",
  "stroke-opacity",
  "stroke-width",
  "stroke-linecap",
  "stroke-linejoin",
  "stroke-dasharray",
  "paint-order",
  "opacity",
  "shape-rendering",
  "color"
];

function inlineSvgComputedStyles(sourceSvg, clonedSvg) {
  if (!(sourceSvg instanceof SVGElement) || !(clonedSvg instanceof SVGElement)) {
    return;
  }

  const sourceNodes = [sourceSvg, ...sourceSvg.querySelectorAll("*")];
  const clonedNodes = [clonedSvg, ...clonedSvg.querySelectorAll("*")];

  sourceNodes.forEach((sourceNode, index) => {
    const clonedNode = clonedNodes[index];

    if (!(sourceNode instanceof Element) || !(clonedNode instanceof Element)) {
      return;
    }

    const computedStyle = window.getComputedStyle(sourceNode);
    SVG_COMPUTED_STYLE_PROPS.forEach((property) => {
      const value = computedStyle.getPropertyValue(property);
      if (value) {
        clonedNode.style.setProperty(property, value);
      }
    });
  });
}

function cloneSvgNodeForExport(svgElement, width, height, exportPadding = null, backgroundColor = EXPORT_BACKGROUND, options = {}) {
  const clone = svgElement.cloneNode(true);
  const intrinsic = getSvgIntrinsicSize(svgElement);
  const padding = normalizeExportPadding(exportPadding);
  const exportWidth = intrinsic.width + padding.left + padding.right;
  const exportHeight = intrinsic.height + padding.top + padding.bottom;

  if (options.preserveComputedStyles === true) {
    inlineSvgComputedStyles(svgElement, clone);
  }

  clone.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  clone.setAttribute("xmlns:xlink", "http://www.w3.org/1999/xlink");
  clone.setAttribute("viewBox", `${-padding.left} ${-padding.top} ${exportWidth} ${exportHeight}`);
  clone.setAttribute("width", `${width}`);
  clone.setAttribute("height", `${height}`);

  const background = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  background.setAttribute("x", `${-padding.left}`);
  background.setAttribute("y", `${-padding.top}`);
  background.setAttribute("width", `${exportWidth}`);
  background.setAttribute("height", `${exportHeight}`);
  background.setAttribute("fill", backgroundColor || EXPORT_BACKGROUND);
  clone.insertBefore(background, clone.firstChild);

  return clone;
}

function cloneSvgForExport(svgElement, width, height, exportPadding = null, backgroundColor = EXPORT_BACKGROUND, options = {}) {
  const serializer = new XMLSerializer();
  return serializer.serializeToString(cloneSvgNodeForExport(svgElement, width, height, exportPadding, backgroundColor, options));
}

async function renderSvgToCanvas({ svgElement, width, height, dpi, exportPadding = null, backgroundColor = EXPORT_BACKGROUND, preserveComputedStyles = false }) {
  const intrinsic = getSvgIntrinsicSize(svgElement);
  const padding = normalizeExportPadding(exportPadding);
  const exportIntrinsicWidth = intrinsic.width + padding.left + padding.right;
  const exportIntrinsicHeight = intrinsic.height + padding.top + padding.bottom;
  const widthScale = intrinsic.width > 0 ? width / intrinsic.width : 1;
  const heightScale = intrinsic.height > 0 ? height / intrinsic.height : 1;
  const maxWidthPx = Math.max(1, Math.round(width + ((padding.left + padding.right) * widthScale)));
  const maxHeightPx = Math.max(1, Math.round(height + ((padding.top + padding.bottom) * heightScale)));
  const { widthPx, heightPx } = fitExportSize(exportIntrinsicWidth, exportIntrinsicHeight, maxWidthPx, maxHeightPx);
  const svgMarkup = cloneSvgForExport(svgElement, widthPx, heightPx, padding, backgroundColor, { preserveComputedStyles });
  const svgBlob = new Blob([svgMarkup], { type: "image/svg+xml;charset=utf-8" });
  const svgUrl = URL.createObjectURL(svgBlob);

  try {
    const image = await new Promise((resolve, reject) => {
      const img = new Image();
      img.onload = () => resolve(img);
      img.onerror = () => reject(new Error("SVG export image load failed."));
      img.src = svgUrl;
    });

    const canvas = document.createElement("canvas");
    canvas.width = widthPx;
    canvas.height = heightPx;

    const context = canvas.getContext("2d");
    if (!context) {
      throw new Error("Canvas 2D context unavailable.");
    }

    context.fillStyle = backgroundColor || EXPORT_BACKGROUND;
    context.fillRect(0, 0, widthPx, heightPx);
    context.drawImage(image, 0, 0, widthPx, heightPx);

    const exportScaleX = exportIntrinsicWidth > 0 ? widthPx / exportIntrinsicWidth : 1;
    const exportScaleY = exportIntrinsicHeight > 0 ? heightPx / exportIntrinsicHeight : 1;

    return {
      canvas,
      widthPx,
      heightPx,
      contentBox: {
        left: padding.left * exportScaleX,
        top: padding.top * exportScaleY,
        width: intrinsic.width * exportScaleX,
        height: intrinsic.height * exportScaleY
      },
      dpi
    };
  } finally {
    URL.revokeObjectURL(svgUrl);
  }
}

function hideTransientUi(clonedDocument) {
  clonedDocument.querySelectorAll("[data-visible='true']").forEach((node) => {
    node.setAttribute("data-visible", "false");
    node.style.opacity = "0";
  });
}

async function renderHostToCanvas({
  hostId,
  selector = null,
  width,
  height,
  dpi,
  forceRaster = false,
  preserveComputedStyles = false,
  exportPadding = null,
  backgroundColor = EXPORT_BACKGROUND
}) {
  const exportElement = resolveExportElement(hostId, selector);

  if (!exportElement) {
    throw new Error("Export host not found.");
  }

  const svgElement = forceRaster ? null : resolveExportSvg(exportElement);
  if (svgElement) {
    return renderSvgToCanvas({ svgElement, width, height, dpi, exportPadding, backgroundColor, preserveComputedStyles });
  }

  const bounds = exportElement.getBoundingClientRect();

  if (!bounds.width || !bounds.height) {
    throw new Error("Export host has no visible size.");
  }

  const maxWidthPx = Math.max(1, Math.round(width));
  const maxHeightPx = Math.max(1, Math.round(height));
  const { scale } = fitExportSize(bounds.width, bounds.height, maxWidthPx, maxHeightPx);

  const canvas = await html2canvas(exportElement, {
    backgroundColor: backgroundColor || EXPORT_BACKGROUND,
    useCORS: true,
    logging: false,
    scale,
    width: Math.ceil(bounds.width),
    height: Math.ceil(bounds.height),
    scrollX: -window.scrollX,
    scrollY: -window.scrollY,
    onclone: hideTransientUi
  });

  const finalCanvas = canvas;

  return {
    canvas: finalCanvas,
    widthPx: finalCanvas.width,
    heightPx: finalCanvas.height,
    dpi
  };
}

async function exportAsPng(options) {
  const pngBlob = await renderPngBlob(options);
  if (pngBlob) {
    downloadBlob(pngBlob, options.filename);
  }
}

async function renderPngBlob(options) {
  const { canvas } = await renderHostToCanvas(options);
  return new Promise((resolve, reject) => {
    canvas.toBlob((pngBlob) => {
      if (!pngBlob) {
        reject(new Error("PNG export blob creation failed."));
        return;
      }

      resolve(pngBlob);
    }, "image/png");
  });
}

async function renderPdfBlob(options) {
  const exportElement = resolveExportElement(options.hostId, options.selector);

  const svgElement = options.forceRaster ? null : resolveExportSvg(exportElement);

  if (svgElement) {
    const intrinsic = getSvgIntrinsicSize(svgElement);
    const { widthPx, heightPx } = fitExportSize(
      intrinsic.width + ((options.exportPadding?.left || 0) + (options.exportPadding?.right || 0)),
      intrinsic.height + ((options.exportPadding?.top || 0) + (options.exportPadding?.bottom || 0)),
      Math.max(1, Math.round(options.width)),
      Math.max(1, Math.round(options.height))
    );

    const svgNode = cloneSvgNodeForExport(
      svgElement,
      widthPx,
      heightPx,
      options.exportPadding,
      options.backgroundColor || EXPORT_BACKGROUND,
      { preserveComputedStyles: options.preserveComputedStyles === true }
    );
    return vectorPdfFromSvgNode({
      svgNode,
      sourceSvgNode: svgElement,
      widthPx,
      heightPx
    });
  }

  const { canvas, widthPx, heightPx, dpi } = await renderHostToCanvas(options);
  const imageData = canvas.toDataURL("image/png");
  const widthIn = widthPx / dpi;
  const heightIn = heightPx / dpi;
  const pdf = new jsPDF({
    orientation: widthIn >= heightIn ? "landscape" : "portrait",
    unit: "in",
    format: [widthIn, heightIn],
    compress: true
  });

  pdf.addImage(imageData, "PNG", 0, 0, widthIn, heightIn, undefined, "FAST");
  return pdf.output("blob");
}

async function exportAsPdf(options) {
  const pdfBlob = await renderPdfBlob(options);
  if (pdfBlob) {
    downloadBlob(pdfBlob, options.filename);
  }
}

function exportTextContent({ filename, mimeType, content }) {
  const blob = new Blob([content ?? ""], { type: mimeType || "text/plain;charset=utf-8" });
  downloadBlob(blob, filename);
}

async function buildPreprocessQcArchive(message) {
  const format = message.format === "pdf" ? "pdf" : "png";
  const extension = format === "pdf" ? "pdf" : "png";
  const selectors = Array.isArray(message.exportEntries) && message.exportEntries.length
    ? message.exportEntries
    : [
      {
        selector: ".ribote-preprocess-panel--stacked .ribote-d3-card:first-child",
        filename: `gene_biotype_composition.${extension}`
      },
      {
        selector: ".ribote-preprocess-panel--stacked .ribote-d3-card:last-child",
        filename: `rrna_fraction_by_sample.${extension}`
      }
    ];

  const zip = new JSZip();

  for (const entry of selectors) {
    const blob = format === "pdf"
      ? await renderPdfBlob({ ...message, selector: entry.selector, exportPadding: entry.exportPadding })
      : await renderPngBlob({ ...message, selector: entry.selector, exportPadding: entry.exportPadding });

    zip.file(entry.filename || `qc_export.${extension}`, blob);
  }

  return zip.generateAsync({ type: "blob" });
}

async function buildFigureArchive(message, fallbackEntries = []) {
  const format = message.format === "pdf" ? "pdf" : "png";
  const extension = format === "pdf" ? "pdf" : "png";
  const selectors = normalizeArchiveEntries(message.exportEntries).length
    ? normalizeArchiveEntries(message.exportEntries)
    : fallbackEntries.map((entry) => ({
      ...entry,
      filename: entry.filename || `figure_export.${extension}`
    }));

  const zip = new JSZip();

  for (const entry of selectors) {
    const blob = format === "pdf"
      ? await renderPdfBlob({ ...message, selector: entry.selector, exportPadding: entry.exportPadding })
      : await renderPngBlob({ ...message, selector: entry.selector, exportPadding: entry.exportPadding });

    zip.file(entry.filename || `figure_export.${extension}`, blob);
  }

  return zip.generateAsync({ type: "blob" });
}

function normalizeArchiveEntries(entries) {
  if (Array.isArray(entries)) {
    return entries;
  }

  if (entries && typeof entries === "object") {
    return Object.values(entries);
  }

  return [];
}

function sanitizeFilenameToken(value, fallback = "figure") {
  const normalized = String(value || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "_")
    .replace(/^_+|_+$/g, "");

  return normalized || fallback;
}

function codonFigureArchiveFilename(message, extension = "zip") {
  const base = String(message?.filename || `codon_current_view.${message?.format === "pdf" ? "pdf" : "png"}`)
    .replace(/\.(png|pdf)$/i, "");
  return `${base}.${extension}`;
}

function collectCodonCardExportEntries(viewRoot, format) {
  const extension = format === "pdf" ? "pdf" : "png";
  const panelCards = Array.from(viewRoot?.querySelectorAll(".ribote-codon-panel-grid > .ribote-d3-card") || []);

  return panelCards.map((card, index) => {
    const exportId = `ribote-codon-card-${Date.now()}-${index}-${Math.round(Math.random() * 1e6)}`;
    const exportTarget = card.querySelector(".ribote-d3-host") || card;
    exportTarget.setAttribute("data-ribote-codon-export-card", exportId);

    const titleNode = card.querySelector(".ribote-d3-chart-title");
    const rawTitle = titleNode?.textContent?.trim() || `codon_panel_${index + 1}`;

    return {
      selector: `[data-ribote-codon-export-card="${exportId}"]`,
      filename: `${sanitizeFilenameToken(rawTitle, `codon_panel_${index + 1}`)}.${extension}`,
      cleanup: () => {
        exportTarget.removeAttribute("data-ribote-codon-export-card");
      }
    };
  });
}

async function buildArchiveBlob(message) {
  const zip = new JSZip();

  const entries = normalizeArchiveEntries(message.entries);

  if (entries.length) {
    entries.forEach((entry) => {
      zip.file(entry.filename || "export.txt", entry.content ?? "");
    });
    return zip.generateAsync({ type: "blob" });
  }

  zip.file(message.entryFilename || "export.txt", message.content ?? "");
  return zip.generateAsync({ type: "blob" });
}

function createOffscreenPcaHost(plot) {
  const host = document.createElement("div");
  host.id = `ribote-pca-export-${Date.now()}-${Math.round(Math.random() * 1e6)}`;
  host.style.position = "fixed";
  host.style.left = "-20000px";
  host.style.top = "0";
  host.style.width = "1200px";
  host.style.padding = "0";
  host.style.margin = "0";
  host.style.background = EXPORT_BACKGROUND;
  host.style.zIndex = "-1";

  const card = document.createElement("div");
  card.className = "ribote-d3-card";
  card.style.background = EXPORT_BACKGROUND;

  const inner = document.createElement("div");
  inner.className = "ribote-d3-host";
  inner.style.width = "1200px";

  card.appendChild(inner);
  host.appendChild(card);
  document.body.appendChild(host);

  drawPcaProjectionChart(
    inner,
    normalizePcaPoints(plot?.points),
    {
      title: plot?.title,
      xLabel: plot?.xLabel,
      yLabel: plot?.yLabel
    },
    { animate: false, reason: "export" }
  );

  return {
    hostId: host.id,
    selector: ".ribote-d3-card",
    cleanup: () => {
      host.remove();
    }
  };
}

async function buildPcaFigureArchive(message) {
  const zip = new JSZip();
  const entries = normalizeArchiveEntries(message.entries);

  for (const entry of entries) {
    const tempHost = createOffscreenPcaHost(entry.plot || {});

    try {
      const blob = message.format === "pdf"
        ? await renderPdfBlob({
          hostId: tempHost.hostId,
          selector: tempHost.selector,
          format: message.format,
          width: message.width,
          height: message.height,
          dpi: message.dpi,
          backgroundColor: message.backgroundColor || EXPORT_BACKGROUND,
          forceRaster: message.forceRaster === true,
          exportPadding: message.exportPadding
        })
        : await renderPngBlob({
          hostId: tempHost.hostId,
          selector: tempHost.selector,
          format: message.format,
          width: message.width,
          height: message.height,
          dpi: message.dpi,
          backgroundColor: message.backgroundColor || EXPORT_BACKGROUND,
          exportPadding: message.exportPadding
        });

      zip.file(entry.filename || "pca_projection.png", blob);
    } finally {
      tempHost.cleanup();
    }
  }

  return zip.generateAsync({ type: "blob" });
}

export function initializeRiboteExportBridge() {
  if (!window.Shiny?.addCustomMessageHandler) {
    return;
  }

  window.Shiny.addCustomMessageHandler("ribote-preprocess-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE Data Preprocess export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-text-export", (message) => {
    dispatchActionLockEvent();
    try {
      exportTextContent(message);
    } finally {
      dispatchActionUnlockEvent();
    }
  });

  window.Shiny.addCustomMessageHandler("ribote-preprocess-qc-export", (message) => {
    dispatchActionLockEvent();
    buildPreprocessQcArchive(message)
      .then((blob) => {
        downloadBlob(blob, message.filename || "data_preprocess_qc_export.zip");
      })
      .catch((error) => {
        console.error("RiboTE Data Preprocess QC export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-translation-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE Translation Efficiency figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-translation-multi-figure-export", (message) => {
    dispatchActionLockEvent();
    buildFigureArchive(message)
      .then((blob) => {
        downloadBlob(blob, message.filename || "translation_efficiency_figure_export.zip");
      })
      .catch((error) => {
        console.error("RiboTE Translation Efficiency multi-figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-archive-export", (message) => {
    dispatchActionLockEvent();
    buildArchiveBlob(message)
      .then((blob) => {
        downloadBlob(blob, message.filename || "ribote_export.zip");
      })
      .catch((error) => {
        console.error("RiboTE archive export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-pca-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE PCA figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-pca-multi-figure-export", (message) => {
    dispatchActionLockEvent();
    buildPcaFigureArchive(message)
      .then((blob) => {
        downloadBlob(blob, message.filename || "pca_figure_export.zip");
      })
      .catch((error) => {
        console.error("RiboTE PCA multi-figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-clustering-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE Clustering figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-clustering-multi-figure-export", (message) => {
    dispatchActionLockEvent();
    buildFigureArchive(message)
      .then((blob) => {
        downloadBlob(blob, message.filename || "clustering_figure_export.zip");
      })
      .catch((error) => {
        console.error("RiboTE Clustering multi-figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-gsea-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE GSEA figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-enrichment-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE Enrichment figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-network-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE Network figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-signalp-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;

    dispatchActionLockEvent();
    runner(message)
      .catch((error) => {
        console.error("RiboTE SignalP figure export failed.", error);
      })
      .finally(() => {
        dispatchActionUnlockEvent();
      });
  });

  window.Shiny.addCustomMessageHandler("ribote-codon-figure-export", (message) => {
    const runner = message.format === "pdf" ? exportAsPdf : exportAsPng;
    const host = document.getElementById(message.hostId);
    const viewRoot = host ? host.querySelector(message.selector || ".ribote-codon-view") : null;
    const panelGrid = viewRoot ? viewRoot.querySelector(".ribote-codon-panel-grid") : null;
    const cardEntries = panelGrid ? collectCodonCardExportEntries(viewRoot, message.format) : [];

    dispatchActionLockEvent();
    (cardEntries.length
      ? buildFigureArchive(
        {
          ...message,
          format: message.format,
          exportEntries: cardEntries.map((entry) => ({
            selector: entry.selector,
            filename: entry.filename
          })),
          preserveComputedStyles: true
        }
      ).then((blob) => {
        downloadBlob(blob, codonFigureArchiveFilename(message, "zip"));
      })
      : runner({
        ...message,
        selector: message.selector,
        preserveComputedStyles: true
      }))
      .catch((error) => {
        console.error("RiboTE Codon figure export failed.", error);
      })
      .finally(() => {
        cardEntries.forEach((entry) => {
          if (typeof entry.cleanup === "function") {
            entry.cleanup();
          }
        });
        dispatchActionUnlockEvent();
      });
  });
}
