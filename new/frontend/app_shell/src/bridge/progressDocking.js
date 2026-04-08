function getVisibleSlots() {
  return Array.from(document.querySelectorAll("[data-rnameta-progress-slot]")).filter((candidate) => {
    const rects = candidate.getClientRects();
    const style = window.getComputedStyle(candidate);

    return rects.length > 0 && style.display !== "none" && style.visibility !== "hidden";
  });
}

function resolveOwnerSlot(panel) {
  const ownerKey = panel.dataset.progressOwner;

  if (!ownerKey) {
    return null;
  }

  return document.querySelector(`[data-rnameta-progress-slot="${ownerKey}"]`);
}

function resolvePendingOwnerSlot() {
  const ownerKey = window.__rnametaAnalysisOwner;

  if (!ownerKey) {
    return null;
  }

  return document.querySelector(`[data-rnameta-progress-slot="${ownerKey}"]`);
}

function dockProgressPanel() {
  const panel = document.getElementById("shiny-notification-panel");
  const slots = Array.from(document.querySelectorAll("[data-rnameta-progress-slot]"));

  if (!panel || !slots.length) {
    return;
  }

  const hasActiveNotification = Boolean(panel.querySelector(".shiny-notification"));

  if (!hasActiveNotification) {
    delete panel.dataset.progressOwner;
    return;
  }

  let slot = resolveOwnerSlot(panel);

  if (!slot) {
    slot = resolvePendingOwnerSlot();

    if (slot?.dataset?.rnametaProgressSlot) {
      panel.dataset.progressOwner = slot.dataset.rnametaProgressSlot;
    }
  }

  if (!slot) {
    const visibleSlots = getVisibleSlots();
    slot = visibleSlots[0] ?? slots[0];

    if (slot?.dataset?.rnametaProgressSlot) {
      panel.dataset.progressOwner = slot.dataset.rnametaProgressSlot;
    }
  }

  if (!slot) {
    return;
  }

  if (panel.parentElement !== slot) {
    slot.appendChild(panel);
  }

  panel.classList.add("rnameta-progress-panel");
}

export function initializeProgressDocking() {
  const runDock = () => window.requestAnimationFrame(dockProgressPanel);

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", runDock, { once: true });
  } else {
    runDock();
  }

  document.addEventListener("shown.bs.tab", runDock);
  document.addEventListener("shown.bs.collapse", runDock);

  const observer = new MutationObserver(() => {
    runDock();
  });

  observer.observe(document.body, {
    childList: true,
    subtree: true,
    attributes: true,
    attributeFilter: ["class", "style", "aria-expanded"]
  });
}
