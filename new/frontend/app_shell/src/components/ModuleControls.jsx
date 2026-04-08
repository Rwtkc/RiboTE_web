import { useEffect, useMemo, useRef, useState } from "react";
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
  Checkbox,
  FormControl,
  FormControlLabel,
  MenuItem,
  Select,
  ThemeProvider,
  Typography
} from "@mui/material";
import { setShinyInputValue } from "../utils/shinyBridge";
import {
  riboteSelectFieldSx,
  riboteSelectInputProps,
  riboteSelectMenuPaperSx,
  riboteSelectTheme
} from "./shared/riboteSelectTheme";

function defaultFieldValue(field) {
  if (field.type === "multiselect" || field.type === "codon_picker") {
    return Array.isArray(field.default) ? field.default : [];
  }

  if (field.type === "checkbox") {
    return Boolean(field.default);
  }

  return field.default ?? "";
}

function buildInitialState(sections) {
  const state = {};

  (sections || []).forEach((section) => {
    (section.fields || []).forEach((field) => {
      state[field.key] = defaultFieldValue(field);
    });
  });

  return state;
}

function buildPreservedState(sections, currentState = {}) {
  if (!Array.isArray(sections) || sections.length === 0) {
    return currentState;
  }

  const nextState = buildInitialState(sections);

  (sections || []).forEach((section) => {
    (section.fields || []).forEach((field) => {
      if (!(field.key in currentState)) {
        return;
      }

      const currentValue = currentState[field.key];

      if (field.type === "select") {
        const optionValues = new Set((field.options || []).map((option) => option.value));
        if (optionValues.has(currentValue)) {
          nextState[field.key] = currentValue;
        }
        return;
      }

      if (field.type === "multiselect" || field.type === "codon_picker") {
        const optionValues = new Set((field.options || []).map((option) => option.value));
        const preserved = Array.isArray(currentValue)
          ? currentValue.filter((value) => optionValues.has(value))
          : [];
        nextState[field.key] = preserved;
        return;
      }

      nextState[field.key] = currentValue;
    });
  });

  return nextState;
}

function stateValueEqual(leftValue, rightValue) {
  if (Array.isArray(leftValue) || Array.isArray(rightValue)) {
    if (!Array.isArray(leftValue) || !Array.isArray(rightValue)) {
      return false;
    }

    return leftValue.length === rightValue.length && leftValue.every((value, index) => Object.is(value, rightValue[index]));
  }

  return Object.is(leftValue, rightValue);
}

function shallowStateEqual(leftState = {}, rightState = {}) {
  const leftKeys = Object.keys(leftState || {});
  const rightKeys = Object.keys(rightState || {});

  if (leftKeys.length !== rightKeys.length) {
    return false;
  }

  return leftKeys.every((key) => Object.prototype.hasOwnProperty.call(rightState, key) && stateValueEqual(leftState[key], rightState[key]));
}

const gseaCollectionOptionsBySpecies = {
  hg38: [
    { label: "Hallmark", value: "hallmark" },
    { label: "Reactome", value: "reactome" },
    { label: "GO Biological Process", value: "go_bp" }
  ],
  osa_IRGSP_1: [
    { label: "GO Biological Process", value: "go_bp" },
    { label: "GO Molecular Function", value: "go_mf" },
    { label: "GO Cellular Component", value: "go_cc" },
    { label: "KEGG", value: "kegg" }
  ]
};

function readSavedSpeciesKey() {
  const summaryText = document.getElementById("load_data-session_summary")?.textContent || "";

  if (summaryText.includes("Oryza sativa (IRGSP 1.0)")) {
    return "osa_IRGSP_1";
  }

  if (summaryText.includes("Homo sapiens (hg38)")) {
    return "hg38";
  }

  return "";
}

function buildSpeciesScopedSections(sections, scopeId) {
  if (scopeId !== "gsea-controls_host") {
    return sections;
  }

  const speciesKey = readSavedSpeciesKey();
  const collectionOptions = gseaCollectionOptionsBySpecies[speciesKey];

  if (!collectionOptions) {
    return sections;
  }

  return (sections || []).map((section) => ({
    ...section,
    fields: (section.fields || []).map((field) =>
      field.key === "gsea_collection"
        ? {
            ...field,
            options: collectionOptions,
            default: collectionOptions[0]?.value ?? field.default
          }
        : field
    )
  }));
}

function ensureModuleControlStateStore() {
  return (window.__rnametaModuleControlState ??= {});
}

function setScopeFieldValue(scopeId, inputId, value) {
  if (!scopeId || !inputId) {
    return;
  }

  const store = ensureModuleControlStateStore();
  const scopeState = (store[scopeId] ??= {});
  scopeState[inputId] = value ?? "";
}

function removeScopeState(scopeId) {
  if (!scopeId) {
    return;
  }

  const store = ensureModuleControlStateStore();
  delete store[scopeId];
}

function ChevronDownIcon() {
  return (
    <Box
      component="svg"
      viewBox="0 0 16 16"
      aria-hidden="true"
      focusable="false"
      sx={{
        color: "var(--rm-muted)",
        display: "block",
        width: "1rem",
        height: "1rem"
      }}
    >
      <path
        d="M4.25 6.25L8 10l3.75-3.75"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.8"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </Box>
  );
}

const accordionCardSx = {
  border: "0.0625rem solid rgba(133, 155, 122, 0.18)",
  borderRadius: "1rem",
  backgroundColor: "rgba(255, 251, 246, 0.85)",
  boxShadow: "none",
  overflow: "hidden",
  "&:before": {
    display: "none"
  }
};

const accordionSummarySx = {
  px: "1rem",
  py: "0.15rem",
  minHeight: "3.6rem",
  "& .MuiAccordionSummary-content": {
    my: "0.75rem"
  },
  "& .MuiAccordionSummary-expandIconWrapper": {
    color: "var(--rm-muted)"
  }
};

const accordionSectionTitleSx = {
  color: "var(--rm-text)",
  fontSize: "1.18rem",
  fontWeight: 800,
  lineHeight: 1.25
};

const accordionDetailsSx = {
  px: "1rem",
  pt: "0.05rem",
  pb: "1rem"
};

const checkboxSx = {
  color: "rgba(133, 155, 122, 0.72)",
  "&.Mui-checked": {
    color: "var(--rm-accent-deep)"
  },
  "& .MuiSvgIcon-root": {
    fontSize: "1.35rem"
  }
};

const checkboxLabelSx = {
  alignItems: "flex-start",
  gap: "0.55rem",
  "& .MuiFormControlLabel-label": {
    color: "var(--rm-text)",
    fontFamily: "\"Montserrat\", sans-serif",
    fontSize: "1.2rem",
    fontWeight: 700,
    lineHeight: 1.45
  },
  "& .MuiCheckbox-root": {
    marginTop: "0.08rem",
    padding: "0.12rem"
  },
  ml: 0
};

const multiSelectSx = {
  ...riboteSelectFieldSx,
  "& .MuiSelect-select": {
    minHeight: "3.75rem",
    display: "flex",
    alignItems: "center",
    padding: "0.85rem 1rem !important",
    fontFamily: "\"Montserrat\", sans-serif",
    fontSize: "1.05rem",
    fontWeight: 600,
    lineHeight: 1.5
  }
};

function formatMultiSelectValue(values, placeholder = "Select items") {
  if (!Array.isArray(values) || values.length === 0) {
    return placeholder;
  }

  if (values.length <= 4) {
    return values.join(", ");
  }

  return `${values.slice(0, 4).join(", ")} +${values.length - 4}`;
}

function CodonPickerField({ field, selectedValues, setState, scopeId }) {
  const [modalOpen, setModalOpen] = useState(false);
  const [query, setQuery] = useState("");
  const options = field.options || [];
  const filteredOptions = useMemo(() => {
    const keyword = query.trim().toUpperCase();
    if (!keyword) {
      return options;
    }

    return options.filter((option) => String(option.label || option.value || "").toUpperCase().includes(keyword));
  }, [options, query]);

  const summaryText = useMemo(() => {
    if (!selectedValues.length) {
      return "No codons selected yet.";
    }

    if (selectedValues.length <= 8) {
      return selectedValues.join(", ");
    }

    return `${selectedValues.slice(0, 8).join(", ")} +${selectedValues.length - 8} more`;
  }, [selectedValues]);

  const toggleCodon = (codonValue) => {
    const nextValue = selectedValues.includes(codonValue)
      ? selectedValues.filter((value) => value !== codonValue)
      : [...selectedValues, codonValue];
    updateFieldValue(field, nextValue, setState, scopeId);
  };

  const clearSelection = () => {
    updateFieldValue(field, [], setState, scopeId);
  };

  return (
    <>
      <div className="ribote-field ribote-field--sample-config">
        <span className="ribote-field__label">{field.label}</span>
        <div className="ribote-sample-config ribote-codon-picker">
          <button
            type="button"
            className="ribote-btn ribote-btn--secondary ribote-btn--block"
            onClick={() => setModalOpen(true)}
          >
            Choose Codons
          </button>
          <div className={selectedValues.length ? "ribote-sample-config__summary" : "ribote-sample-config__hint"}>
            {selectedValues.length
              ? `${selectedValues.length} codons selected | ${summaryText}`
              : "Open the codon panel to choose one or more codons for downstream views."}
          </div>
        </div>
      </div>

      {modalOpen ? (
        <div className="ribote-sample-modal">
          <div className="ribote-sample-modal__backdrop" onClick={() => setModalOpen(false)} />
          <div className="ribote-sample-modal__dialog" role="dialog" aria-modal="true" aria-label="Choose codons">
            <div className="ribote-sample-modal__dialog-header">
              <div>
                <h3>Choose Codons</h3>
                <p>Select from the 61 sense codons used in the codon-usage workspace. Stop codons (TAA, TAG, TGA) are excluded.</p>
              </div>
              <button
                type="button"
                className="ribote-sample-modal__close"
                onClick={() => setModalOpen(false)}
              >
                Close
              </button>
            </div>

            <div className="ribote-sample-modal__body">
              <div className="ribote-field">
                <span className="ribote-field__label">Search Codons</span>
                <input
                  className="ribote-input"
                  type="text"
                  value={query}
                  placeholder="Filter codons"
                  onChange={(event) => setQuery(event.target.value)}
                />
              </div>

              <div className="ribote-codon-picker__grid">
                {filteredOptions.map((option) => {
                  const active = selectedValues.includes(option.value);

                  return (
                    <button
                      key={option.value}
                      type="button"
                      className={`ribote-codon-card${active ? " is-active" : ""}`}
                      onClick={() => toggleCodon(option.value)}
                    >
                      {option.label}
                    </button>
                  );
                })}
              </div>

              {filteredOptions.length === 0 ? (
                <div className="ribote-sample-modal__empty">No codons match the current filter.</div>
              ) : null}

              <div className="ribote-sample-modal__hint">
                {selectedValues.length
                  ? `Current selection: ${summaryText}`
                  : "No codons selected yet."}
              </div>
            </div>

            <div className="ribote-sample-modal__footer">
              <button
                type="button"
                className="ribote-btn ribote-btn--secondary"
                onClick={clearSelection}
              >
                Clear Selection
              </button>
              <button
                type="button"
                className="ribote-btn ribote-btn--primary"
                onClick={() => setModalOpen(false)}
              >
                Use Selected Codons
              </button>
            </div>
          </div>
        </div>
      ) : null}
    </>
  );
}

function updateFieldValue(field, value, setState, scopeId) {
  setState((current) => ({
    ...current,
    [field.key]: value
  }));

  setScopeFieldValue(scopeId, field.id, value);
  setShinyInputValue(field.id, value, { priority: "event" });
}

function renderField(field, state, setState, scopeId) {
  if (field.type === "select") {
    return (
      <label key={field.key} className="ribote-field">
        <span className="ribote-field__label">{field.label}</span>
        <FormControl fullWidth>
          <Select
            value={state[field.key] ?? ""}
            onChange={(event) => updateFieldValue(field, event.target.value, setState, scopeId)}
            sx={riboteSelectFieldSx}
            inputProps={riboteSelectInputProps}
            MenuProps={{ PaperProps: { sx: riboteSelectMenuPaperSx } }}
          >
            {(field.options || []).map((option) => (
              <MenuItem key={option.value} value={option.value}>
                {option.label}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      </label>
    );
  }

  if (field.type === "number") {
    return (
      <label key={field.key} className="ribote-field">
        <span className="ribote-field__label">{field.label}</span>
        <input
          className="ribote-input"
          type="number"
          value={state[field.key] ?? ""}
          min={field.min}
          max={field.max}
          step={field.step}
          placeholder={field.placeholder}
          onChange={(event) => updateFieldValue(field, event.target.value, setState, scopeId)}
        />
      </label>
    );
  }

  if (field.type === "multiselect") {
    const selectedValues = Array.isArray(state[field.key]) ? state[field.key] : [];

    return (
      <label key={field.key} className="ribote-field">
        <span className="ribote-field__label">{field.label}</span>
        <FormControl fullWidth>
          <Select
            multiple
            value={selectedValues}
            displayEmpty
            onChange={(event) => {
              const nextValue = Array.isArray(event.target.value)
                ? event.target.value
                : typeof event.target.value === "string"
                  ? event.target.value.split(",").filter(Boolean)
                  : [];
              updateFieldValue(field, nextValue, setState, scopeId);
            }}
            renderValue={(selected) => formatMultiSelectValue(selected, field.placeholder || "Select codons")}
            sx={multiSelectSx}
            inputProps={riboteSelectInputProps}
            MenuProps={{
              PaperProps: {
                sx: {
                  ...riboteSelectMenuPaperSx,
                  maxHeight: "20rem"
                }
              }
            }}
          >
            {(field.options || []).map((option) => {
              const checked = selectedValues.includes(option.value);

              return (
                <MenuItem key={option.value} value={option.value}>
                  <Checkbox checked={checked} sx={checkboxSx} />
                  <Typography component="span" sx={{ fontSize: "1.05rem", fontWeight: 600 }}>
                    {option.label}
                  </Typography>
                </MenuItem>
              );
            })}
          </Select>
        </FormControl>
      </label>
    );
  }

  if (field.type === "codon_picker") {
    const selectedValues = Array.isArray(state[field.key]) ? state[field.key] : [];

    return (
      <CodonPickerField
        key={field.key}
        field={field}
        selectedValues={selectedValues}
        setState={setState}
        scopeId={scopeId}
      />
    );
  }

  if (field.type === "checkbox") {
    return (
      <FormControlLabel
        key={field.key}
        className="ribote-checkbox"
        sx={checkboxLabelSx}
        control={
          <Checkbox
            checked={Boolean(state[field.key])}
            onChange={(event) => updateFieldValue(field, event.target.checked, setState, scopeId)}
            sx={checkboxSx}
          />
        }
        label={field.label}
      />
    );
  }

  return (
    <label key={field.key} className="ribote-field">
      <span className="ribote-field__label">{field.label}</span>
      <input
        className="ribote-input"
        type="text"
        value={state[field.key] ?? ""}
        placeholder={field.placeholder}
        onChange={(event) => updateFieldValue(field, event.target.value, setState, scopeId)}
      />
    </label>
  );
}

export default function ModuleControls({ config }) {
  const sections = config.sections || [];
  const rootRef = useRef(null);
  const [state, setState] = useState(() => buildInitialState(sections));
  const [scopeId, setScopeId] = useState(null);
  const [externalContextVersion, setExternalContextVersion] = useState(0);
  const resolvedSections = useMemo(
    () => buildSpeciesScopedSections(sections, scopeId),
    [sections, scopeId, externalContextVersion]
  );
  const effectiveState = useMemo(() => buildPreservedState(resolvedSections, state), [resolvedSections, state]);

  useEffect(() => {
    const hostElement = rootRef.current?.parentElement;
    const nextScopeId = hostElement?.id || null;
    setScopeId(nextScopeId);

    return () => {
      if (nextScopeId) {
        removeScopeState(nextScopeId);
      }
    };
  }, []);

  useEffect(() => {
    if (!shallowStateEqual(state, effectiveState)) {
      setState(effectiveState);
    }
  }, [state, effectiveState]);

  useEffect(() => {
    if (scopeId !== "gsea-controls_host") {
      return undefined;
    }

    const target = document.getElementById("load_data-session_summary");
    if (!target) {
      return undefined;
    }

    const observer = new MutationObserver(() => {
      setExternalContextVersion((version) => version + 1);
    });
    observer.observe(target, {
      childList: true,
      characterData: true,
      subtree: true
    });
    setExternalContextVersion((version) => version + 1);

    return () => {
      observer.disconnect();
    };
  }, [scopeId]);

  useEffect(() => {
    resolvedSections.forEach((section) => {
      (section.fields || []).forEach((field) => {
        setScopeFieldValue(scopeId, field.id, effectiveState[field.key]);
        setShinyInputValue(field.id, effectiveState[field.key], { priority: "event" });
      });
    });
  }, [scopeId, resolvedSections, effectiveState]);

  const renderedSections = useMemo(
    () =>
      resolvedSections.map((section) => {
        const fields = (section.fields || []).map((field) => renderField(field, effectiveState, setState, scopeId));

        if (section.collapsible) {
          return (
            <Accordion
              key={section.title}
              defaultExpanded={section.defaultExpanded !== false}
              disableGutters
              elevation={0}
              sx={accordionCardSx}
            >
              <AccordionSummary expandIcon={<ChevronDownIcon />} sx={accordionSummarySx}>
                <Typography component="span" sx={accordionSectionTitleSx}>
                  {section.title}
                </Typography>
              </AccordionSummary>
              <AccordionDetails sx={accordionDetailsSx}>
                <div className="ribote-control-section ribote-control-section--plain">
                  {fields}
                </div>
              </AccordionDetails>
            </Accordion>
          );
        }

        return (
          <div key={section.title} className="ribote-control-section">
            <div className="ribote-control-section__title">{section.title}</div>
            {fields}
          </div>
        );
      }),
    [resolvedSections, effectiveState, scopeId]
  );

  return (
    <ThemeProvider theme={riboteSelectTheme}>
      <div ref={rootRef} className="ribote-control-card">{renderedSections}</div>
    </ThemeProvider>
  );
}
