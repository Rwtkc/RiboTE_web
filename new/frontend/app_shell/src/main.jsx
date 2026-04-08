import React from "react";
import { createRoot } from "react-dom/client";
import "@fontsource/archivo-black/400.css";
import App from "./App";
import { initializeAnalysisActionBridge } from "./bridge/analysisActionBridge";
import { initializeControlBridge } from "./bridge/controlBridge";
import { initializeButtonStateBridge } from "./bridge/buttonStateBridge";
import { initializeRiboteExportBridge } from "./bridge/exportBridge";
import { initializeProgressDocking } from "./bridge/progressDocking";
import LoadDataControls from "./components/LoadDataControls";
import ModuleControls from "./components/ModuleControls";
import ModuleExport from "./components/ModuleExport";
import ModuleCanvas from "./components/ModuleCanvas";
import PreprocessResults from "./components/PreprocessResults";
import PreprocessExport from "./components/PreprocessExport";
import TranslationEfficiencyResults from "./components/TranslationEfficiencyResults";
import TranslationEfficiencyExport from "./components/TranslationEfficiencyExport";
import PcaResults from "./components/PcaResults";
import PcaExport from "./components/PcaExport";
import ClusteringResults from "./components/ClusteringResults";
import ClusteringExport from "./components/ClusteringExport";
import GseaExport from "./components/GseaExport";
import GseaResults from "./components/GseaResults";
import EnrichmentExport from "./components/EnrichmentExport";
import EnrichmentResults from "./components/EnrichmentResults";
import NetworkExport from "./components/NetworkExport";
import NetworkResults from "./components/NetworkResults";
import SignalpExport from "./components/SignalpExport";
import SignalpResults from "./components/SignalpResults";
import CodonExport from "./components/CodonExport";
import CodonResults from "./components/CodonResults";
import "./styles/tokens.css";
import "./styles/app-shell.css";

function getShellTarget() {
  const root = document.querySelector("[data-shell-config]");

  if (!root) {
    return { root: null, config: {} };
  }

  try {
    return {
      root,
      config: JSON.parse(root.dataset.shellConfig || "{}")
    };
  } catch (error) {
    console.error("Failed to parse shell config.", error);

    return { root, config: {} };
  }
}

const { root, config } = getShellTarget();

if (root) {
  document.body.dataset.shellReady = "true";

  createRoot(root).render(
    <React.StrictMode>
      <App config={config} />
    </React.StrictMode>
  );
}

initializeControlBridge({
  allowedControls: [
    "ribote-load-data-controls",
    "ribote-module-controls",
    "ribote-module-export",
    "ribote-preprocess-export",
    "ribote-module-canvas",
    "ribote-preprocess-results",
    "ribote-translation-export",
    "ribote-translation-results",
    "ribote-pca-export",
    "ribote-pca-results",
    "ribote-clustering-export",
    "ribote-clustering-results",
    "ribote-gsea-export",
    "ribote-gsea-results",
    "ribote-enrichment-export",
    "ribote-enrichment-results",
    "ribote-network-export",
    "ribote-network-results",
    "ribote-signalp-export",
    "ribote-signalp-results",
    "ribote-codon-export",
    "ribote-codon-results"
  ],
  renderControl: (control, config) => {
    if (control === "ribote-load-data-controls") {
      return <LoadDataControls config={config} />;
    }

    if (control === "ribote-module-controls") {
      return <ModuleControls config={config} />;
    }

    if (control === "ribote-module-export") {
      return <ModuleExport config={config} />;
    }

    if (control === "ribote-preprocess-export") {
      return <PreprocessExport config={config} />;
    }

    if (control === "ribote-module-canvas") {
      return <ModuleCanvas config={config} />;
    }

    if (control === "ribote-preprocess-results") {
      return <PreprocessResults config={config} />;
    }

    if (control === "ribote-translation-export") {
      return <TranslationEfficiencyExport config={config} />;
    }

    if (control === "ribote-translation-results") {
      return <TranslationEfficiencyResults config={config} />;
    }

    if (control === "ribote-pca-export") {
      return <PcaExport config={config} />;
    }

    if (control === "ribote-pca-results") {
      return <PcaResults config={config} />;
    }

    if (control === "ribote-clustering-export") {
      return <ClusteringExport config={config} />;
    }

    if (control === "ribote-clustering-results") {
      return <ClusteringResults config={config} />;
    }

    if (control === "ribote-gsea-export") {
      return <GseaExport config={config} />;
    }

    if (control === "ribote-gsea-results") {
      return <GseaResults config={config} />;
    }

    if (control === "ribote-enrichment-export") {
      return <EnrichmentExport config={config} />;
    }

    if (control === "ribote-enrichment-results") {
      return <EnrichmentResults config={config} />;
    }

    if (control === "ribote-network-export") {
      return <NetworkExport config={config} />;
    }

    if (control === "ribote-network-results") {
      return <NetworkResults config={config} />;
    }

    if (control === "ribote-signalp-export") {
      return <SignalpExport config={config} />;
    }

    if (control === "ribote-signalp-results") {
      return <SignalpResults config={config} />;
    }

    if (control === "ribote-codon-export") {
      return <CodonExport config={config} />;
    }

    if (control === "ribote-codon-results") {
      return <CodonResults config={config} />;
    }

    return null;
  }
});

initializeAnalysisActionBridge();
initializeProgressDocking();
initializeButtonStateBridge();
initializeRiboteExportBridge();
