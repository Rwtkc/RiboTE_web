import { useEffect, useMemo, useState } from "react";
import { FormControl, MenuItem, Select, ThemeProvider } from "@mui/material";
import { setShinyInputValue } from "../utils/shinyBridge";
import {
  buildDragPreviewLabel,
  dragSelectionForSample,
  moveSamplesToZone,
  nextSelectedSamples,
  normalizeSelectedSamples
} from "../utils/loadDataSampleSelection";
import {
  riboteSelectFieldSx,
  riboteSelectInputProps,
  riboteSelectMenuPaperSx,
  riboteSelectTheme
} from "./shared/riboteSelectTheme";

function syncInputValue(inputId, value) {
  if (!inputId) {
    return;
  }

  setShinyInputValue(inputId, value ?? "", { priority: "event" });
}

function renderSpeciesValue(selected) {
  return selected ? selected : <span style={{ color: "#a9b3bf" }}>Choose your species</span>;
}

function detectDelimiterFromHeader(headerLine) {
  const delimiters = ["\t", ",", ";", "|"];
  let bestDelimiter = "\t";
  let bestScore = -1;

  delimiters.forEach((delimiter) => {
    const score = headerLine.split(delimiter).length;
    if (score > bestScore) {
      bestScore = score;
      bestDelimiter = delimiter;
    }
  });

  return bestDelimiter;
}

async function extractSampleNamesFromFile(file) {
  const chunk = await file.slice(0, 65536).text();
  const firstLine = chunk.split(/\r?\n/).find((line) => line.trim().length > 0) || "";
  if (!firstLine) {
    return [];
  }

  const delimiter = detectDelimiterFromHeader(firstLine);
  const columns = firstLine.split(delimiter).map((part) => part.trim());
  return columns.slice(1).filter(Boolean);
}

function buildAssignments(sampleNames) {
  return {
    unassigned: [...sampleNames],
    rna: [],
    ribo: []
  };
}

function serializeSampleTypeManifest(sampleNames, assignments) {
  if (sampleNames.length === 0 || assignments.unassigned.length > 0) {
    return null;
  }

  const zoneForSample = {};
  assignments.rna.forEach((sample) => {
    zoneForSample[sample] = "RNA-seq";
  });
  assignments.ribo.forEach((sample) => {
    zoneForSample[sample] = "Ribo-seq";
  });

  if (Object.keys(zoneForSample).length !== sampleNames.length) {
    return null;
  }

  return sampleNames.map((sampleName) => ({
    sample_name: sampleName,
    sample_type: zoneForSample[sampleName]
  }));
}

function buildPairRows(rnaSamples, existingRows = []) {
  const existingByRna = new Map(existingRows.map((row) => [row.rnaSample, row]));

  return rnaSamples.map((rnaSample, index) => {
    const existing = existingByRna.get(rnaSample);
    return {
      rnaSample,
      riboSample: existing?.riboSample || "",
      groupRole: existing?.groupRole || (index < Math.ceil(rnaSamples.length / 2) ? "Control" : "Treatment")
    };
  });
}

function buildPairManifest(pairRows, riboSamples) {
  if (pairRows.length === 0) {
    return null;
  }

  const riboSelections = pairRows.map((row) => row.riboSample).filter(Boolean);
  if (riboSelections.length !== pairRows.length) {
    return null;
  }

  if (new Set(riboSelections).size !== riboSelections.length) {
    return null;
  }

  if (!riboSelections.every((sample) => riboSamples.includes(sample))) {
    return null;
  }

  const roles = pairRows.map((row) => row.groupRole).filter(Boolean);
  if (roles.length !== pairRows.length) {
    return null;
  }

  if (!roles.includes("Control") || !roles.includes("Treatment")) {
    return null;
  }

  return pairRows.map((row) => ({
    rna_sample: row.rnaSample,
    ribo_sample: row.riboSample,
    group_role: row.groupRole
  }));
}

function DropZone({ title, subtitle, zoneKey, samples, selectedSamples, onDropSamples, onSampleSelect, onDragStart }) {
  const handleDrop = (event) => {
    event.preventDefault();
    const serializedSamples = event.dataTransfer.getData("application/x-ribote-samples");
    let movedSamples = [];

    if (serializedSamples) {
      try {
        movedSamples = JSON.parse(serializedSamples);
      } catch (error) {
        movedSamples = [];
      }
    }

    if (!Array.isArray(movedSamples) || movedSamples.length === 0) {
      const sampleName = event.dataTransfer.getData("text/plain");
      movedSamples = sampleName ? [sampleName] : [];
    }

    if (movedSamples.length > 0) {
      onDropSamples(movedSamples, zoneKey);
    }
  };

  return (
    <div
      className="ribote-sample-modal__zone"
      onDragOver={(event) => event.preventDefault()}
      onDrop={handleDrop}
    >
      <div className="ribote-sample-modal__zone-header">
        <span>{title}</span>
        <small>{subtitle}</small>
      </div>
      <div className="ribote-sample-modal__zone-body">
        {samples.map((sample) => {
          const isSelected = selectedSamples.includes(sample);
          return (
            <button
              key={sample}
              type="button"
              className={`ribote-sample-card${isSelected ? " is-selected" : ""}`}
              draggable
              aria-pressed={isSelected ? "true" : "false"}
              onClick={(event) => {
                onSampleSelect(sample, event.ctrlKey || event.metaKey);
              }}
              onDragStart={(event) => {
                const draggedSamples = onDragStart(sample, event);
                event.dataTransfer.setData("text/plain", sample);
                event.dataTransfer.setData("application/x-ribote-samples", JSON.stringify(draggedSamples));
              }}
            >
              {sample}
            </button>
          );
        })}
        {samples.length === 0 ? <div className="ribote-sample-modal__empty">Drop sample cards here</div> : null}
      </div>
    </div>
  );
}

export default function LoadDataControls({ config }) {
  const demo = config.demo_values || {};
  const speciesChoices = Array.isArray(config.species_choices)
    ? config.species_choices
    : config.species_choices
      ? [config.species_choices]
      : [];
  const [dataSource, setDataSource] = useState("Upload File");
  const [species, setSpecies] = useState("");
  const [files, setFiles] = useState([]);
  const [demoFileName, setDemoFileName] = useState("");
  const [sampleNames, setSampleNames] = useState([]);
  const [assignments, setAssignments] = useState(buildAssignments([]));
  const [selectedSamples, setSelectedSamples] = useState([]);
  const [pairRows, setPairRows] = useState([]);
  const [modalOpen, setModalOpen] = useState(false);
  const [step, setStep] = useState("assign");

  useEffect(() => {
    syncInputValue(config.data_source_id, dataSource);
  }, [config.data_source_id, dataSource]);

  useEffect(() => {
    syncInputValue(config.species_id, species);
  }, [config.species_id, species]);

  const availableSamples = useMemo(
    () => [...assignments.unassigned, ...assignments.rna, ...assignments.ribo],
    [assignments]
  );

  useEffect(() => {
    setSelectedSamples((current) => normalizeSelectedSamples(current, availableSamples));
  }, [availableSamples]);

  const resetSampleConfiguration = (nextSampleNames) => {
    setSampleNames(nextSampleNames);
    setAssignments(buildAssignments(nextSampleNames));
    setSelectedSamples([]);
    setPairRows([]);
    setModalOpen(false);
    setStep("assign");
  };

  const applyDemoConfiguration = () => {
    const nextSampleNames = Array.isArray(demo.sample_names) ? demo.sample_names : [];
    const nextSampleTypeManifest = Array.isArray(demo.sample_type_manifest) ? demo.sample_type_manifest : [];
    const nextPairManifest = Array.isArray(demo.pair_manifest) ? demo.pair_manifest : [];

    setSampleNames(nextSampleNames);
    setAssignments({
      unassigned: [],
      rna: nextSampleTypeManifest
        .filter((row) => row.sample_type === "RNA-seq")
        .map((row) => row.sample_name),
      ribo: nextSampleTypeManifest
        .filter((row) => row.sample_type === "Ribo-seq")
        .map((row) => row.sample_name)
    });
    setSelectedSamples([]);
    setPairRows(
      nextPairManifest.map((row) => ({
        rnaSample: row.rna_sample,
        riboSample: row.ribo_sample,
        groupRole: row.group_role
      }))
    );
    setModalOpen(false);
    setStep("assign");
  };

  useEffect(() => {
    const input = document.getElementById(config.file_input_id);

    if (!input) {
      return undefined;
    }

    const handleChange = async () => {
      const nextFiles = Array.from(input.files || []);
      setFiles(nextFiles);

      if (nextFiles.length > 0) {
        setDemoFileName("");
        const nextSampleNames = await extractSampleNamesFromFile(nextFiles[0]);
        resetSampleConfiguration(nextSampleNames);
      } else {
        resetSampleConfiguration([]);
      }
    };

    handleChange();
    input.addEventListener("change", handleChange);

    return () => {
      input.removeEventListener("change", handleChange);
    };
  }, [config.file_input_id]);

  const fileSummary = useMemo(() => {
    if (files.length > 0) {
      return files.map((file) => file.name).join(", ");
    }

    if (demoFileName) {
      return demoFileName;
    }

    return "No matrix selected";
  }, [demoFileName, files]);

  const fileSummaryClassName =
    files.length > 0 || demoFileName !== ""
      ? "ribote-file-picker__summary"
      : "ribote-file-picker__summary ribote-file-picker__summary--empty";

  const sampleTypeManifest = useMemo(
    () => serializeSampleTypeManifest(sampleNames, assignments),
    [assignments, sampleNames]
  );

  const pairManifest = useMemo(
    () => buildPairManifest(pairRows, assignments.ribo),
    [assignments.ribo, pairRows]
  );

  useEffect(() => {
    syncInputValue(
      config.sample_type_manifest_id,
      sampleTypeManifest ? JSON.stringify(sampleTypeManifest) : ""
    );
  }, [config.sample_type_manifest_id, sampleTypeManifest]);

  useEffect(() => {
    syncInputValue(
      config.pair_manifest_id,
      pairManifest ? JSON.stringify(pairManifest) : ""
    );
  }, [config.pair_manifest_id, pairManifest]);

  const canConfigureSamples = species.trim() !== "" && (files.length > 0 || demoFileName !== "") && sampleNames.length > 0;
  const canAdvanceToPairing =
    assignments.unassigned.length === 0 &&
    assignments.rna.length > 0 &&
    assignments.ribo.length > 0 &&
    assignments.rna.length === assignments.ribo.length;
  const canConfirm = species.trim() !== "" && (files.length > 0 || demoFileName !== "") && !!sampleTypeManifest && !!pairManifest;

  const pairingSummary = useMemo(() => {
    if (!pairManifest) {
      return "Sample pairing is required before saving.";
    }

    const controlCount = pairManifest.filter((row) => row.group_role === "Control").length;
    const treatmentCount = pairManifest.filter((row) => row.group_role === "Treatment").length;
    return `${pairManifest.length} pairs configured | Control ${controlCount} | Treatment ${treatmentCount}`;
  }, [pairManifest]);

  const pairingHint = useMemo(() => {
    if (!canConfigureSamples) {
      return "";
    }

    if (!sampleTypeManifest) {
      return "Next step: click Configure Samples to assign every sample card to RNA-seq or Ribo-seq.";
    }

    if (!pairManifest) {
      return "Continue in Configure Samples to finish one-to-one pairing and mark Control / Treatment.";
    }

    return "";
  }, [canConfigureSamples, pairManifest, sampleTypeManifest]);

  const handleSampleSelect = (sampleName, additive) => {
    setSelectedSamples((current) => nextSelectedSamples(current, sampleName, additive));
  };

  const handleDragStart = (sampleName, event) => {
    const draggedSamples = dragSelectionForSample(selectedSamples, sampleName);
    setSelectedSamples(draggedSamples);

    if (event?.dataTransfer) {
      const dragPreview = document.createElement("div");
      dragPreview.className = "ribote-sample-card ribote-sample-card--drag-preview";
      dragPreview.textContent = buildDragPreviewLabel(draggedSamples);
      document.body.appendChild(dragPreview);
      event.dataTransfer.setDragImage(dragPreview, 24, 20);
      window.requestAnimationFrame(() => {
        dragPreview.remove();
      });
    }

    return draggedSamples;
  };

  const moveSamples = (movedSamples, targetZone) => {
    const normalizedMovedSamples = Array.isArray(movedSamples) ? movedSamples.filter(Boolean) : [];
    setAssignments((current) => moveSamplesToZone(current, normalizedMovedSamples, targetZone, sampleNames).assignments);
    setSelectedSamples(normalizedMovedSamples);
  };

  const openPairingModal = () => {
    setSelectedSamples([]);
    setStep("assign");
    setModalOpen(true);
  };

  const advanceToPairing = () => {
    setSelectedSamples([]);
    setPairRows((current) => buildPairRows(assignments.rna, current));
    setStep("pair");
  };

  const usedRiboSamples = new Set(pairRows.map((row) => row.riboSample).filter(Boolean));
  const pairingSummaryClassName = pairManifest
    ? "ribote-sample-config__summary"
    : "ribote-sample-config__hint";

  return (
    <ThemeProvider theme={riboteSelectTheme}>
      <div className="ribote-control-card ribote-control-card--load-data">
        <div className="ribote-control-section">
          <div className="ribote-field">
            <span className="ribote-field__label">1. Select Species</span>
            <FormControl fullWidth>
              <Select
                value={species}
                onChange={(event) => setSpecies(event.target.value)}
                displayEmpty
                sx={riboteSelectFieldSx}
                inputProps={riboteSelectInputProps}
                MenuProps={{ PaperProps: { sx: riboteSelectMenuPaperSx } }}
                renderValue={renderSpeciesValue}
              >
                {speciesChoices.map((choice) => (
                  <MenuItem key={choice} value={choice}>
                    {choice}
                  </MenuItem>
                ))}
              </Select>
            </FormControl>
          </div>

          <div className="ribote-field">
            <span className="ribote-field__label">2. Gene Matrix</span>
            <div className="ribote-file-picker">
              <div className="ribote-file-picker__field">
                <button
                  type="button"
                  className="ribote-file-picker__button"
                  onClick={() => document.getElementById(config.file_input_id)?.click()}
                >
                  Browse Matrix
                </button>
                <span className={fileSummaryClassName}>{fileSummary}</span>
              </div>
            </div>
          </div>

          <div className="ribote-field ribote-field--sample-config">
            <span className="ribote-field__label">3. Sample Pairing</span>
            <div className="ribote-sample-config">
              <div className={pairingSummaryClassName}>{pairingSummary}</div>
              <button
                type="button"
                className={`ribote-btn ribote-btn--secondary ribote-btn--block${canConfigureSamples && !pairManifest ? " ribote-btn--attention" : ""}`}
                disabled={!canConfigureSamples}
                onClick={openPairingModal}
              >
                Configure Samples
              </button>
              {pairingHint ? <div className="ribote-sample-config__hint">{pairingHint}</div> : null}
            </div>
          </div>
        </div>

        <div className="ribote-control-actions">
          <button
            type="button"
            className="ribote-btn ribote-btn--primary"
            disabled={!canConfirm}
            onClick={() => {
              setShinyInputValue(config.save_button_id, String(Date.now()), {
                priority: "event"
              });
            }}
          >
            Save / Confirm
          </button>
          <button
            type="button"
            className="ribote-btn ribote-btn--ghost"
            onClick={() => {
              setDataSource("Upload File");
              setSpecies(demo.species || "");
              setFiles([]);
              setDemoFileName(demo.file_name || "all.count.txt");
              applyDemoConfiguration();
              setShinyInputValue(config.demo_button_id, String(Date.now()), {
                priority: "event"
              });
            }}
          >
            Load Demo
          </button>
        </div>

        {modalOpen ? (
          <div className="ribote-sample-modal">
            <div className="ribote-sample-modal__backdrop" onClick={() => setModalOpen(false)} />
            <div className="ribote-sample-modal__dialog" role="dialog" aria-modal="true" aria-label="Configure sample pairing">
              <div className="ribote-sample-modal__dialog-header">
                <div>
                  <h3>Configure Sample Pairing</h3>
                  <p>
                    Step {step === "assign" ? "1" : "2"} of 2. Drag cards into RNA-seq / Ribo-seq, then create one-to-one
                    pairs and assign Control or Treatment.
                  </p>
                </div>
                <button
                  type="button"
                  className="ribote-sample-modal__close"
                  onClick={() => setModalOpen(false)}
                >
                  Close
                </button>
              </div>

              {step === "assign" ? (
                <div className="ribote-sample-modal__body">
                  <div className="ribote-sample-modal__zone-grid">
                    <DropZone
                      title="Unassigned"
                      subtitle={`${assignments.unassigned.length} sample cards`}
                      zoneKey="unassigned"
                      samples={assignments.unassigned}
                      selectedSamples={selectedSamples}
                      onDropSamples={moveSamples}
                      onSampleSelect={handleSampleSelect}
                      onDragStart={handleDragStart}
                    />
                    <DropZone
                      title="RNA-seq"
                      subtitle={`${assignments.rna.length} assigned`}
                      zoneKey="rna"
                      samples={assignments.rna}
                      selectedSamples={selectedSamples}
                      onDropSamples={moveSamples}
                      onSampleSelect={handleSampleSelect}
                      onDragStart={handleDragStart}
                    />
                    <DropZone
                      title="Ribo-seq"
                      subtitle={`${assignments.ribo.length} assigned`}
                      zoneKey="ribo"
                      samples={assignments.ribo}
                      selectedSamples={selectedSamples}
                      onDropSamples={moveSamples}
                      onSampleSelect={handleSampleSelect}
                      onDragStart={handleDragStart}
                    />
                  </div>
                  <div className="ribote-sample-modal__hint">
                    Move every sample into RNA-seq or Ribo-seq. Hold Ctrl or Command to select multiple cards, then drag any selected card as a batch. The two partitions must contain the same number of cards.
                  </div>
                </div>
              ) : (
                <div className="ribote-sample-modal__body">
                  <div className="ribote-sample-modal__pair-grid">
                    {pairRows.map((row, index) => (
                      <div key={row.rnaSample} className="ribote-sample-modal__pair-row">
                        <div className="ribote-sample-modal__pair-sample">{row.rnaSample}</div>
                        <FormControl fullWidth>
                          <Select
                            value={row.riboSample}
                            onChange={(event) => {
                              const value = event.target.value;
                              setPairRows((current) =>
                                current.map((entry, entryIndex) =>
                                  entryIndex === index ? { ...entry, riboSample: value } : entry
                                )
                              );
                            }}
                            sx={riboteSelectFieldSx}
                            inputProps={riboteSelectInputProps}
                            MenuProps={{ PaperProps: { sx: riboteSelectMenuPaperSx } }}
                            displayEmpty
                            renderValue={(selected) =>
                              selected ? selected : <span style={{ color: "#a9b3bf" }}>Choose matching Ribo-seq</span>
                            }
                          >
                            <MenuItem value="">Choose matching Ribo-seq</MenuItem>
                            {assignments.ribo.map((sample) => {
                              const isTaken = usedRiboSamples.has(sample) && sample !== row.riboSample;
                              return (
                                <MenuItem key={sample} value={sample} disabled={isTaken}>
                                  {sample}
                                </MenuItem>
                              );
                            })}
                          </Select>
                        </FormControl>
                        <FormControl fullWidth>
                          <Select
                            value={row.groupRole}
                            onChange={(event) => {
                              const value = event.target.value;
                              setPairRows((current) =>
                                current.map((entry, entryIndex) =>
                                  entryIndex === index ? { ...entry, groupRole: value } : entry
                                )
                              );
                            }}
                            sx={riboteSelectFieldSx}
                            inputProps={riboteSelectInputProps}
                            MenuProps={{ PaperProps: { sx: riboteSelectMenuPaperSx } }}
                          >
                            <MenuItem value="Control">Control</MenuItem>
                            <MenuItem value="Treatment">Treatment</MenuItem>
                          </Select>
                        </FormControl>
                      </div>
                    ))}
                  </div>
                  <div className="ribote-sample-modal__hint">
                    Each RNA-seq sample must map to exactly one Ribo-seq sample, and each pair must be tagged as Control or
                    Treatment.
                  </div>
                </div>
              )}

              <div className="ribote-sample-modal__footer">
                {step === "pair" ? (
                  <button
                    type="button"
                    className="ribote-btn ribote-btn--secondary"
                    onClick={() => setStep("assign")}
                  >
                    Back
                  </button>
                ) : (
                  <span />
                )}
                {step === "assign" ? (
                  <button
                    type="button"
                    className="ribote-btn ribote-btn--primary"
                    disabled={!canAdvanceToPairing}
                    onClick={advanceToPairing}
                  >
                    Next: Pair Mapping
                  </button>
                ) : (
                  <button
                    type="button"
                    className="ribote-btn ribote-btn--primary"
                    disabled={!pairManifest}
                    onClick={() => setModalOpen(false)}
                  >
                    Apply Pairing
                  </button>
                )}
              </div>
            </div>
          </div>
        ) : null}
      </div>
    </ThemeProvider>
  );
}





