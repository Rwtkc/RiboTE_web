import { useEffect, useMemo, useState } from "react";
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
  FormControl,
  MenuItem,
  Select,
  Stack,
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

function syncInputValue(inputId, value) {
  if (!inputId) {
    return;
  }

  setShinyInputValue(inputId, String(value ?? ""), { priority: "event" });
}

function buildInitialState(defaults) {
  return {
    format: defaults.format ?? "png",
    width: String(defaults.width ?? 3000),
    height: String(defaults.height ?? 1800),
    dpi: String(defaults.dpi ?? 300),
    dataFormat: defaults.data_format ?? "csv"
  };
}

function formatViewLabel(view) {
  switch (view) {
    case "barplot":
      return "Library Size";
    case "qc":
      return "QC";
    case "data":
      return "Data";
    default:
      return "Current View";
  }
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

const cardSx = {
  border: "0.0625rem solid rgba(133, 155, 122, 0.16)",
  borderRadius: "1rem",
  backgroundColor: "rgba(248, 242, 234, 0.68)",
  boxShadow: "none",
  overflow: "hidden",
  "&:before": {
    display: "none"
  }
};

const accordionSummarySx = {
  px: "1.1rem",
  py: "0.2rem",
  minHeight: "3.9rem",
  "& .MuiAccordionSummary-content": {
    my: "0.8rem"
  },
  "& .MuiAccordionSummary-expandIconWrapper": {
    color: "var(--rm-muted)"
  }
};

const sectionTitleSx = {
  color: "var(--rm-text)",
  fontSize: "1.18rem",
  fontWeight: 800
};

const accordionDetailsSx = {
  px: "1.1rem",
  pt: "0.1rem",
  pb: "1.1rem"
};

const labelSx = {
  mb: "0.45rem",
  color: "var(--rm-text)",
  fontSize: "1.08rem",
  fontWeight: 700,
  lineHeight: 1.35
};

const helperSx = {
  color: "var(--rm-muted)",
  fontSize: "1rem",
  fontWeight: 600,
  lineHeight: 1.55
};

const eyebrowSx = {
  display: "inline-flex",
  alignSelf: "flex-start",
  px: "0.65rem",
  py: "0.35rem",
  borderRadius: "999px",
  backgroundColor: "rgba(133, 155, 122, 0.12)",
  color: "var(--rm-accent-deep)",
  fontSize: "0.7rem",
  fontWeight: 800,
  letterSpacing: "0.14em",
  textTransform: "uppercase"
};

const exportMenuPaperSx = {
  ...riboteSelectMenuPaperSx,
  "& .MuiMenuItem-root": {
    minHeight: "3rem",
    fontFamily: "\"Montserrat\", sans-serif",
    fontSize: "1.08rem",
    color: "var(--rm-text)"
  }
};

function FieldLabel({ children, htmlFor }) {
  return (
    <Typography component="label" htmlFor={htmlFor} sx={labelSx}>
      {children}
    </Typography>
  );
}

function SelectField({ id, label, value, onChange, options }) {
  return (
    <Box>
      <FieldLabel htmlFor={id}>{label}</FieldLabel>
      <FormControl fullWidth>
        <Select
          id={id}
          value={value}
          onChange={onChange}
          sx={riboteSelectFieldSx}
          inputProps={riboteSelectInputProps}
          MenuProps={{ PaperProps: { sx: exportMenuPaperSx } }}
        >
          {(options || []).map((option) => (
            <MenuItem key={option.value} value={option.value}>
              {option.label}
            </MenuItem>
          ))}
        </Select>
      </FormControl>
    </Box>
  );
}

function TextField({ id, label, value, onChange, inputMode, pattern }) {
  return (
    <Box className="ribote-field">
      <FieldLabel htmlFor={id}>{label}</FieldLabel>
      <input
        id={id}
        type="text"
        className="ribote-input"
        value={value}
        onChange={onChange}
        data-shiny-no-bind-input="true"
        inputMode={inputMode}
        pattern={pattern}
      />
    </Box>
  );
}

export default function PreprocessExport({ config }) {
  const defaults = config.defaults || {};
  const ids = config.ids || {};
  const choices = config.choices || {};
  const ready = Boolean(config.ready);
  const figureDisabled = Boolean(config.figureDisabled);
  const dataDisabled = Boolean(config.dataDisabled);
  const currentView = String(config.currentView || "data");
  const [state, setState] = useState(buildInitialState(defaults));

  useEffect(() => {
    setState(buildInitialState(defaults));
  }, [defaults]);

  useEffect(() => {
    syncInputValue(ids.format, state.format);
    syncInputValue(ids.width, state.width);
    syncInputValue(ids.height, state.height);
    syncInputValue(ids.dpi, state.dpi);
    syncInputValue(ids.dataFormat, state.dataFormat);
  }, [ids, state]);

  const content = useMemo(() => (
    <Stack spacing={2}>
      {currentView !== "data" ? (
        <div className="ribote-control-section ribote-control-section--export ribote-control-section--export-figure-plain">
          <Box sx={eyebrowSx}>Export Figure</Box>
          <Box sx={helperSx}>Active view: {formatViewLabel(currentView)}</Box>
          <SelectField
            id={ids.format}
            label="Format"
            value={state.format}
            onChange={(event) => setState((current) => ({ ...current, format: event.target.value }))}
            options={choices.format || []}
          />
          <TextField
            id={ids.width}
            label="Width (px)"
            value={state.width}
            onChange={(event) => setState((current) => ({ ...current, width: event.target.value }))}
            inputMode="decimal"
          />
          <TextField
            id={ids.height}
            label="Height (px)"
            value={state.height}
            onChange={(event) => setState((current) => ({ ...current, height: event.target.value }))}
            inputMode="decimal"
          />
          <TextField
            id={ids.dpi}
            label="DPI"
            value={state.dpi}
            onChange={(event) => setState((current) => ({ ...current, dpi: event.target.value }))}
            inputMode="numeric"
            pattern="[0-9]*"
          />
          <button
            type="button"
            className="ribote-btn ribote-btn--primary ribote-btn--block"
            data-rnameta-lock-during-analysis="true"
            disabled={figureDisabled}
            onClick={() => {
              setShinyInputValue(ids.trigger, String(Date.now()), { priority: "event" });
            }}
          >
            Export
          </button>
        </div>
      ) : null}

      <div className="ribote-control-section ribote-control-section--export ribote-control-section--export-data-plain">
        <Box sx={eyebrowSx}>Export Data</Box>
        <SelectField
          id={ids.dataFormat}
          label="Data Format"
          value={state.dataFormat}
          onChange={(event) => setState((current) => ({ ...current, dataFormat: event.target.value }))}
          options={choices.dataFormat || []}
        />
        <button
          type="button"
          className="ribote-btn ribote-btn--primary ribote-btn--block"
          data-rnameta-lock-during-analysis="true"
          disabled={dataDisabled}
          onClick={() => {
            setShinyInputValue(ids.dataTrigger, String(Date.now()), { priority: "event" });
          }}
        >
          Export
        </button>
      </div>
    </Stack>
  ), [choices.dataFormat, choices.format, currentView, dataDisabled, figureDisabled, ids, state]);

  if (!ready) {
    return null;
  }

  return (
    <ThemeProvider theme={riboteSelectTheme}>
      <Box className="ribote-preprocess-export-react">
        <Accordion defaultExpanded disableGutters sx={cardSx}>
          <AccordionSummary expandIcon={<ChevronDownIcon />} sx={accordionSummarySx}>
            <Typography sx={sectionTitleSx}>Export</Typography>
          </AccordionSummary>
          <AccordionDetails sx={accordionDetailsSx}>
            {content}
          </AccordionDetails>
        </Accordion>
      </Box>
    </ThemeProvider>
  );
}
