import { jsPDF } from "jspdf";
import * as opentype from "opentype.js";
import { svg2pdf } from "svg2pdf.js";
import montserrat400Woff from "@fontsource/montserrat/files/montserrat-latin-400-normal.woff";
import montserrat500Woff from "@fontsource/montserrat/files/montserrat-latin-500-normal.woff";
import montserrat600Woff from "@fontsource/montserrat/files/montserrat-latin-600-normal.woff";
import montserrat700Woff from "@fontsource/montserrat/files/montserrat-latin-700-normal.woff";
import montserrat800Woff from "@fontsource/montserrat/files/montserrat-latin-800-normal.woff";

const SVG_NS = "http://www.w3.org/2000/svg";
const FONT_ASSETS = {
  montserrat400: montserrat400Woff,
  montserrat500: montserrat500Woff,
  montserrat600: montserrat600Woff,
  montserrat700: montserrat700Woff,
  montserrat800: montserrat800Woff
};
const fontPromises = new Map();

function copyComputedTextStyle(sourceNode, targetNode) {
  if (!sourceNode || !targetNode || typeof window?.getComputedStyle !== "function") {
    return;
  }

  const computed = window.getComputedStyle(sourceNode);
  const mappedAttributes = {
    "font-family": computed.fontFamily || '"Montserrat", sans-serif',
    "font-size": computed.fontSize || "16px",
    "font-weight": computed.fontWeight || "400",
    "fill": computed.fill || "#22301f",
    "text-anchor": computed.textAnchor || sourceNode.getAttribute("text-anchor") || "start",
    "dominant-baseline": computed.dominantBaseline || sourceNode.getAttribute("dominant-baseline") || "alphabetic",
    "opacity": computed.opacity || "1"
  };

  Object.entries(mappedAttributes).forEach(([name, value]) => {
    if (value != null && value !== "") {
      targetNode.setAttribute(name, String(value));
    }
  });
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

function parseSvgLength(value, fontSize) {
  if (value == null || value === "") {
    return 0;
  }

  const text = String(value).trim();
  const numeric = Number.parseFloat(text);
  if (!Number.isFinite(numeric)) {
    return 0;
  }

  if (text.endsWith("em")) {
    return numeric * fontSize;
  }

  return numeric;
}

function normalizeFontWeight(value) {
  const numeric = Number.parseInt(String(value || "400"), 10);
  if (Number.isNaN(numeric)) {
    return 400;
  }

  if (numeric >= 750) {
    return 800;
  }

  if (numeric >= 650) {
    return 700;
  }

  if (numeric >= 550) {
    return 600;
  }

  if (numeric >= 450) {
    return 500;
  }

  return 400;
}

function resolveFontKey(node) {
  return `montserrat${normalizeFontWeight(getSvgTextValue(node, "font-weight"))}`;
}

function resolveBaselineOffset(font, fontSize, baseline) {
  const units = font.unitsPerEm || 1000;
  const ascender = font.ascender || 0;
  const descender = font.descender || 0;
  const normalized = String(baseline || "alphabetic").toLowerCase();

  if (normalized === "middle" || normalized === "central") {
    return ((ascender + descender) / 2 / units) * fontSize;
  }

  return 0;
}

function loadFontByKey(fontKey) {
  if (!fontPromises.has(fontKey)) {
    const assetUrl = FONT_ASSETS[fontKey];
    if (!assetUrl) {
      throw new Error(`Unsupported vector export font key: ${fontKey}`);
    }

    fontPromises.set(fontKey, fetch(assetUrl)
      .then((response) => {
        if (!response.ok) {
          throw new Error("Failed to load vector export font.");
        }
        return response.arrayBuffer();
      })
      .then((buffer) => opentype.parse(buffer)));
  }

  return fontPromises.get(fontKey);
}

async function convertTextToPaths(svgRoot) {
  const textNodes = Array.from(svgRoot.querySelectorAll("text")).filter((node) => String(node.textContent || "").trim());
  if (!textNodes.length) {
    return;
  }

  const uniqueFontKeys = [...new Set(textNodes.map((node) => resolveFontKey(node)))];
  const fonts = Object.fromEntries(await Promise.all(uniqueFontKeys.map(async (fontKey) => [fontKey, await loadFontByKey(fontKey)])));

  textNodes.forEach((node) => {
    const text = String(node.textContent || "");
    const font = fonts[resolveFontKey(node)];
    const fontSize = Number.parseFloat(getSvgTextValue(node, "font-size") || "16");
    const anchor = String(getSvgTextValue(node, "text-anchor") || "start").toLowerCase();
    const baseline = String(getSvgTextValue(node, "dominant-baseline") || "alphabetic").toLowerCase();
    let x = Number.parseFloat(node.getAttribute("x") || "0");
    let y = Number.parseFloat(node.getAttribute("y") || "0");
    x += parseSvgLength(node.getAttribute("dx"), fontSize);
    y += parseSvgLength(node.getAttribute("dy"), fontSize);
    const advance = font.getAdvanceWidth(text, fontSize);

    if (anchor === "middle") {
      x -= advance / 2;
    } else if (anchor === "end") {
      x -= advance;
    }

    y += resolveBaselineOffset(font, fontSize, baseline);

    const pathNode = document.createElementNS(SVG_NS, "path");
    pathNode.setAttribute("d", font.getPath(text, x, y, fontSize).toPathData(2));
    pathNode.setAttribute("fill", getSvgTextValue(node, "fill") || "#22301f");
    pathNode.setAttribute("stroke", "none");

    const transform = node.getAttribute("transform");
    if (transform) {
      pathNode.setAttribute("transform", transform);
    }

    const opacity = getSvgTextValue(node, "opacity");
    if (opacity != null) {
      pathNode.setAttribute("opacity", opacity);
    }

    node.replaceWith(pathNode);
  });
}

export async function vectorPdfFromSvgNode({ svgNode, sourceSvgNode = null, widthPx, heightPx, filename }) {
  inlineComputedSvgTextStyles(sourceSvgNode, svgNode);
  await convertTextToPaths(svgNode);

  const pdf = new jsPDF({
    orientation: widthPx >= heightPx ? "landscape" : "portrait",
    unit: "px",
    format: [widthPx, heightPx],
    compress: true
  });

  await svg2pdf(svgNode, pdf, { xOffset: 0, yOffset: 0, scale: 1 });
  if (filename) {
    pdf.save(filename);
    return null;
  }

  return pdf.output("blob");
}
