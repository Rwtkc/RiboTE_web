export function setShinyInputValue(inputId, value, options = { priority: "event" }) {
  if (!inputId || !window.Shiny) {
    return;
  }

  const normalized = value ?? "";

  if (typeof window.Shiny.setInputValue === "function") {
    window.Shiny.setInputValue(inputId, normalized, options);
    return;
  }

  if (typeof window.Shiny.onInputChange === "function") {
    window.Shiny.onInputChange(inputId, normalized);
  }
}
