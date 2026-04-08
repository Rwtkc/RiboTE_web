const ANALYSIS_LOCK_SELECTOR = [
  "[data-rnameta-analysis-trigger='true']",
  "[data-rnameta-lock-during-analysis='true']"
].join(", ");

function isAnalysisLockManagedButton(node) {
  return Boolean(node?.matches?.(ANALYSIS_LOCK_SELECTOR));
}

function applyButtonState(message) {
  if (!message?.id) {
    return;
  }

  const node = document.getElementById(message.id);

  if (!node) {
    return;
  }

  const disabled = Boolean(message.disabled);

  if (!disabled && window.__rnametaAnalysisLocked && isAnalysisLockManagedButton(node)) {
    return;
  }

  node.disabled = disabled;

  if (disabled) {
    node.setAttribute("disabled", "disabled");
    node.setAttribute("aria-disabled", "true");
    return;
  }

  node.removeAttribute("disabled");
  node.setAttribute("aria-disabled", "false");
}

export function initializeButtonStateBridge() {
  const shiny = window.Shiny;

  if (!shiny?.addCustomMessageHandler) {
    return;
  }

  shiny.addCustomMessageHandler("rnameta:set-button-state", applyButtonState);
}
