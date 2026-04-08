import { createTheme } from "@mui/material";

export const riboteSelectTheme = createTheme({
  typography: {
    fontFamily: "\"Montserrat\", sans-serif"
  }
});

export const riboteSelectFieldSx = {
  minHeight: "3.75rem",
  borderRadius: "0.875rem",
  backgroundColor: "var(--rm-surface-soft)",
  color: "var(--rm-text)",
  fontFamily: "\"Montserrat\", sans-serif",
  fontSize: "1.1rem",
  boxShadow: "none",
  "& .MuiOutlinedInput-notchedOutline": {
    borderColor: "var(--rm-border)"
  },
  "&:hover .MuiOutlinedInput-notchedOutline": {
    borderColor: "rgba(133, 155, 122, 0.45)"
  },
  "&.Mui-focused .MuiOutlinedInput-notchedOutline": {
    borderColor: "rgba(133, 155, 122, 0.55)",
    borderWidth: "0.0625rem"
  },
  "& .MuiSelect-select": {
    minHeight: "3.75rem",
    display: "flex",
    alignItems: "center",
    padding: "0 1rem !important",
    fontFamily: "\"Montserrat\", sans-serif",
    fontSize: "1.1rem",
    fontWeight: 500
  },
  "& .MuiSvgIcon-root": {
    color: "var(--rm-muted)",
    fontSize: "1.2rem"
  }
};

export const riboteSelectMenuPaperSx = {
  mt: "0.35rem",
  border: "0.0625rem solid rgba(133, 155, 122, 0.18)",
  borderRadius: "0.9rem",
  backgroundColor: "rgba(255, 252, 246, 0.98)",
  boxShadow: "0 1rem 1.875rem rgba(58, 49, 38, 0.1)",
  "& .MuiMenuItem-root": {
    minHeight: "3.1rem",
    fontFamily: "\"Montserrat\", sans-serif",
    fontSize: "1.1rem",
    color: "var(--rm-text)"
  },
  "& .MuiMenuItem-root:hover": {
    backgroundColor: "rgba(232, 237, 226, 0.5)"
  },
  "& .MuiMenuItem-root.Mui-selected": {
    backgroundColor: "rgba(232, 237, 226, 0.72)"
  },
  "& .MuiMenuItem-root.Mui-selected:hover": {
    backgroundColor: "rgba(232, 237, 226, 0.82)"
  }
};

export const riboteSelectInputProps = {
  "data-shiny-no-bind-input": "true"
};
