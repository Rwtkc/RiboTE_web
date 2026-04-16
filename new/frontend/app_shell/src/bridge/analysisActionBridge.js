import { setShinyInputValue } from "../utils/shinyBridge";

const ANALYSIS_LOCK_SELECTOR = [
  "[data-rnameta-analysis-trigger='true']",
  "[data-rnameta-lock-during-analysis='true']"
].join(", ");

function publishAnalysisLockState(locked, owner = null, label = "") {
  window.__rnametaAnalysisLocked = Boolean(locked);
  window.__rnametaAnalysisOwner = owner || null;
  window.__rnametaAnalysisLabel = locked ? label || "Analysis" : "";
  window.dispatchEvent(new CustomEvent("rnameta:analysis-lock-state", {
    detail: {
      locked: Boolean(locked),
      owner: owner || null,
      label: locked ? label || "Analysis" : ""
    }
  }));
}

function flushModuleControlState(button) {
  if (!button) {
    return;
  }

  const sidebar = button.closest(".ribote-sidebar");
  const controlsHost = sidebar?.querySelector("[data-rnameta-control='ribote-module-controls']");
  const hostId = controlsHost?.id;

  if (!hostId) {
    return;
  }

  const scopeState = window.__rnametaModuleControlState?.[hostId];

  if (!scopeState || typeof scopeState !== "object") {
    return;
  }

  Object.entries(scopeState).forEach(([inputId, value]) => {
    setShinyInputValue(inputId, value, { priority: "event" });
  });
}

function buildAnalysisRequest(button) {
  if (!button?.id) {
    return null;
  }

  const sidebar = button.closest(".ribote-sidebar");
  const controlsHost = sidebar?.querySelector("[data-rnameta-control='ribote-module-controls']");
  const hostId = controlsHost?.id;
  const scopeState = hostId ? window.__rnametaModuleControlState?.[hostId] : null;

  return {
    token: String(Date.now()),
    controls: scopeState && typeof scopeState === "object" ? { ...scopeState } : {}
  };
}

function triggerAnalysisRun(button) {
  if (!button?.id) {
    return;
  }

  const owner = button.dataset.rnametaAnalysisOwner || null;
  const label = button.textContent?.trim() || "Analysis";
  const usesSnapshotRequest = button.dataset.rnametaAnalysisSnapshot === "true";
  lockAnalysisButtons(owner, label);
  window.setTimeout(() => {
    if (usesSnapshotRequest) {
      const requestId = button.id.replace(/run_analysis$/, "analysis_request");
      setShinyInputValue(requestId, buildAnalysisRequest(button), { priority: "event" });
      return;
    }

    setShinyInputValue(button.id, String(Date.now()), { priority: "event" });
  }, 0);
}

function setButtonDisabledState(button, disabled) {
  if (!button) {
    return;
  }

  if (disabled) {
    if (!button.hasAttribute("data-rnameta-disabled-before-lock")) {
      button.setAttribute("data-rnameta-disabled-before-lock", button.disabled ? "true" : "false");
    }

    button.disabled = true;
    button.setAttribute("disabled", "disabled");
    button.setAttribute("aria-disabled", "true");
    return;
  }

  const disabledBeforeLock = button.getAttribute("data-rnameta-disabled-before-lock") === "true";
  button.removeAttribute("data-rnameta-disabled-before-lock");

  button.disabled = disabledBeforeLock;

  if (disabledBeforeLock) {
    button.setAttribute("disabled", "disabled");
    button.setAttribute("aria-disabled", "true");
    return;
  }

  button.removeAttribute("disabled");
  button.setAttribute("aria-disabled", "false");
}

function lockAnalysisButtons(owner, label = "") {
  publishAnalysisLockState(true, owner || window.__rnametaAnalysisOwner || null, label);

  document.querySelectorAll(ANALYSIS_LOCK_SELECTOR).forEach((button) => {
    setButtonDisabledState(button, true);
  });
}

function unlockAnalysisButtons() {
  publishAnalysisLockState(false, null);

  document.querySelectorAll(ANALYSIS_LOCK_SELECTOR).forEach((button) => {
    setButtonDisabledState(button, false);
  });
}

export function initializeAnalysisActionBridge() {
  document.addEventListener(
    "click",
    (event) => {
      const button = event.target.closest("[data-rnameta-analysis-trigger='true']");

      if (!button || button.disabled) {
        return;
      }

      event.preventDefault();
      event.stopImmediatePropagation();
      flushModuleControlState(button);
      triggerAnalysisRun(button);
    },
    true
  );

  window.addEventListener("rnameta:action-lock", () => {
    lockAnalysisButtons(window.__rnametaAnalysisOwner || null, window.__rnametaAnalysisLabel || "");
  });

  window.addEventListener("rnameta:action-unlock", () => {
    if (window.__rnametaAnalysisOwner) {
      return;
    }

    unlockAnalysisButtons();
  });

  if (window.Shiny?.addCustomMessageHandler) {
    window.Shiny.addCustomMessageHandler("rnameta:set-analysis-lock", (message) => {
    if (message?.locked) {
        lockAnalysisButtons(message.owner || null, message.label || "");
        return;
      }

      unlockAnalysisButtons();
    });
  }

  document.addEventListener("click", (event) => {
    const button = event.target.closest("[data-rnameta-analysis-trigger='true']");

    if (!button || button.disabled) {
      return;
    }

    lockAnalysisButtons(button.dataset.rnametaAnalysisOwner || null, button.textContent?.trim() || "Analysis");
  });
}
