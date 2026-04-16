import { jsPDF } from "jspdf";
import { svg2pdf } from "svg2pdf.js";

function copyComputedTextStyle(sourceNode, targetNode) {
  if (!sourceNode || !targetNode || typeof window?.getComputedStyle !== "function") {
    return;
  }

  const computed = window.getComputedStyle(sourceNode);
  const fontFamily = resolveSvgExportFontFamily(computed.fontFamily || getSvgTextValue(sourceNode, "font-family"));
  const fontWeight = normalizeSvgExportFontWeight(computed.fontWeight || getSvgTextValue(sourceNode, "font-weight") || "400");
  const mappedAttributes = {
    "font-family": fontFamily,
    "font-size": computed.fontSize || "16px",
    "font-weight": fontWeight,
    "font-style": "normal",
    "fill": computed.fill || "#17292f",
    "text-anchor": computed.textAnchor || sourceNode.getAttribute("text-anchor") || "start",
    "dominant-baseline": computed.dominantBaseline || sourceNode.getAttribute("dominant-baseline") || "alphabetic",
    "opacity": computed.opacity || "1"
  };

  Object.entries(mappedAttributes).forEach(([name, value]) => {
    if (value != null && value !== "") {
      targetNode.setAttribute(name, String(value));
    }
  });

  const styleMap = parseStyleAttribute(targetNode.getAttribute("style"));
  styleMap["font-family"] = fontFamily;
  styleMap["font-size"] = computed.fontSize || "16px";
  styleMap["font-weight"] = fontWeight;
  styleMap["font-style"] = "normal";
  styleMap.fill = computed.fill || "#17292f";
  targetNode.setAttribute("style", serializeStyleAttribute(styleMap));
}

export function inlineComputedSvgTextStyles(sourceSvgNode, targetSvgNode) {
  if (!sourceSvgNode || !targetSvgNode) {
    return;
  }

  const sourceTextNodes = Array.from(sourceSvgNode.querySelectorAll("text"));
  const targetTextNodes = Array.from(targetSvgNode.querySelectorAll("text"));
  const pairCount = Math.min(sourceTextNodes.length, targetTextNodes.length);

  for (let index = 0; index < pairCount; index += 1) {
    copyComputedTextStyle(sourceTextNodes[index], targetTextNodes[index]);
  }
}

function normalizePdfTextFonts(svgNode) {
  svgNode.querySelectorAll("text").forEach((node) => {
    const fontFamily = resolveSvgExportFontFamily(getSvgTextValue(node, "font-family"));
    const fontWeight = normalizeSvgExportFontWeight(getSvgTextValue(node, "font-weight"));
    node.setAttribute("font-family", fontFamily);
    node.setAttribute("font-weight", fontWeight);
    node.setAttribute("font-style", "normal");

    const styleMap = parseStyleAttribute(node.getAttribute("style"));
    styleMap["font-family"] = fontFamily;
    styleMap["font-weight"] = fontWeight;
    styleMap["font-style"] = "normal";
    node.setAttribute("style", serializeStyleAttribute(styleMap));
  });
}

function getSvgTextValue(node, name) {
  let current = node;

  while (current && current.nodeType === 1) {
    const explicit = current.getAttribute(name);
    if (explicit != null) {
      return explicit;
    }

    const styleMap = parseStyleAttribute(current.getAttribute("style"));
    if (styleMap[name] != null) {
      return styleMap[name];
    }

    current = current.parentElement;
  }

  return null;
}

function parseStyleAttribute(styleText) {
  return String(styleText || "")
    .split(";")
    .map((part) => part.trim())
    .filter(Boolean)
    .reduce((accumulator, entry) => {
      const splitIndex = entry.indexOf(":");
      if (splitIndex === -1) {
        return accumulator;
      }

      const key = entry.slice(0, splitIndex).trim();
      const value = entry.slice(splitIndex + 1).trim();
      accumulator[key] = value;
      return accumulator;
    }, {});
}

function serializeStyleAttribute(styleMap) {
  return Object.entries(styleMap)
    .filter(([, value]) => value != null && String(value).trim() !== "")
    .map(([name, value]) => `${name}: ${value}`)
    .join("; ");
}

function resolveSvgExportFontFamily(value) {
  const family = String(value || "").toLowerCase();
  if (
    !family ||
    family === "inherit" ||
    family.includes("sans-serif") ||
    family.includes("system-ui") ||
    family.includes("var(") ||
    family.includes("--app-") ||
    family.includes("segoe ui") ||
    family.includes("arial") ||
    family.includes("helvetica")
  ) {
    return "sans-serif";
  }

  return value || "sans-serif";
}

function normalizeSvgExportFontWeight(value) {
  const normalized = String(value || "400").trim().toLowerCase();
  if (normalized === "bold" || normalized === "bolder") {
    return "700";
  }

  const numeric = Number.parseInt(normalized, 10);
  if (!Number.isFinite(numeric)) {
    return "400";
  }

  return numeric >= 600 ? "700" : "400";
}

export async function vectorPdfFromSvgNode({ svgNode, sourceSvgNode = null, widthPx, heightPx, filename }) {
  inlineComputedSvgTextStyles(sourceSvgNode, svgNode);
  normalizePdfTextFonts(svgNode);

  const pdf = new jsPDF({
    orientation: widthPx >= heightPx ? "landscape" : "portrait",
    unit: "px",
    format: [widthPx, heightPx],
    compress: true
  });
  pdf.setFont("helvetica", "normal");

  await svg2pdf(svgNode, pdf, { xOffset: 0, yOffset: 0, scale: 1 });
  if (filename) {
    pdf.save(filename);
    return null;
  }

  return pdf.output("blob");
}
