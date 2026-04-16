let frontendProgressCloseTimer = null;
let suppressedCompletedProgressKey = null;
let lastFrontendResultRenderAt = 0;

function clearFrontendProgressCloseTimer() {
  if (frontendProgressCloseTimer) {
    window.clearTimeout(frontendProgressCloseTimer);
    frontendProgressCloseTimer = null;
  }
}

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

function resolveSlotByOwner(ownerKey) {
  if (ownerKey) {
    const ownerSlot = document.querySelector(`[data-rnameta-progress-slot="${ownerKey}"]`);

    if (ownerSlot) {
      return ownerSlot;
    }
  }

  return getVisibleSlots()[0] ?? document.querySelector("[data-rnameta-progress-slot]");
}

function removeFrontendProgress() {
  clearFrontendProgressCloseTimer();
  document.querySelectorAll(".rnameta-frontend-progress").forEach((progress) => {
    progress.remove();
  });
}

function shouldWaitForResultRender(progress) {
  const ownerKey = progress?.dataset?.ownerKey || "";
  const completedAt = Number(progress?.dataset?.completedAt || 0);

  if (!ownerKey.includes("codon") || !completedAt) {
    return false;
  }

  if (lastFrontendResultRenderAt >= completedAt) {
    return false;
  }

  return Date.now() - completedAt < 8000;
}

function getProgressCompletionKey(ownerKey, state) {
  return [
    ownerKey || "default",
    state?.message || "",
    state?.detail || "",
    Math.round(Number(state?.percent) || 0)
  ].join("|");
}

function scheduleFrontendProgressClose(progress, completionKey, delay = 1000) {
  if (!progress || progress.dataset.closeScheduled === "true") {
    return;
  }

  progress.dataset.closeScheduled = "true";
  progress.dataset.completionKey = completionKey || progress.dataset.completionKey || "";
  clearFrontendProgressCloseTimer();
  frontendProgressCloseTimer = window.setTimeout(() => {
    if (shouldWaitForResultRender(progress)) {
      progress.dataset.closeScheduled = "false";
      frontendProgressCloseTimer = null;
      scheduleFrontendProgressClose(progress, progress.dataset.completionKey, 200);
      return;
    }

    suppressedCompletedProgressKey = progress.dataset.completionKey || null;
    progress.remove();
    frontendProgressCloseTimer = null;
  }, delay);
}

function escapeHtml(value) {
  return String(value || "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function normalizeAnalysisLabel(label) {
  return String(label || "Analysis").replace(/^Run\s+/i, "") || "Analysis";
}

function parsePercent(value) {
  if (value == null) {
    return null;
  }

  const match = String(value).match(/-?\d+(?:\.\d+)?/);

  if (!match) {
    return null;
  }

  const numericValue = Number.parseFloat(match[0]);

  if (!Number.isFinite(numericValue)) {
    return null;
  }

  return Math.max(0, Math.min(100, numericValue));
}

function formatProgressDetail(detail, percent) {
  const safePercent = Math.round(Math.max(0, Math.min(100, Number(percent) || 0)));
  const rawDetail = String(detail || "").trim();

  if (!rawDetail) {
    return `${safePercent}%`;
  }

  if (/^\s*-?\d+(?:\.\d+)?%\s*\|/u.test(rawDetail)) {
    return rawDetail.replace(/^\s*-?\d+(?:\.\d+)?%/u, `${safePercent}%`);
  }

  return rawDetail;
}

function formatDisplayedProgressDetail(detail, percent) {
  const visiblePercent = Number(percent) > 0 ? Number(percent) : 6;

  return formatProgressDetail(detail, visiblePercent);
}

function readShinyProgressState(panel) {
  const notification = panel?.querySelector(".shiny-notification");

  if (!notification) {
    return null;
  }

  const progress = notification.querySelector(".progress");
  const bar = notification.querySelector(".progress-bar");

  if (!progress && !bar) {
    return null;
  }

  const message = notification.querySelector(".progress-message")?.textContent?.trim() || "";
  const detail = notification.querySelector(".progress-detail")?.textContent?.trim() || "";
  const ariaPercent = parsePercent(progress?.getAttribute("aria-valuenow"));
  const normalizedAriaPercent = ariaPercent != null && ariaPercent > 0 && ariaPercent <= 1
    ? ariaPercent * 100
    : ariaPercent;
  const percentCandidates = [
    parsePercent(detail),
    parsePercent(bar?.style?.width),
    parsePercent(bar?.getAttribute("style")),
    normalizedAriaPercent
  ].filter((value) => value != null && Number.isFinite(value));
  const percent = percentCandidates.length ? Math.max(...percentCandidates) : 0;

  return {
    message,
    detail: formatProgressDetail(detail, percent),
    percent
  };
}

function buildInitialProgressState(label = "Analysis") {
  const displayLabel = normalizeAnalysisLabel(label);

  return {
    message: `Running ${displayLabel}`,
    detail: "0% | Initializing analysis workspace",
    percent: 0
  };
}

function updateFrontendProgress(progress, state, completionKey = null) {
  if (!progress || !state) {
    return;
  }

  const percent = Math.max(0, Math.min(100, Number(state.percent) || 0));
  const visiblePercent = percent > 0 ? percent : 6;
  const bar = progress.querySelector(".progress-bar");
  const progressTrack = progress.querySelector(".progress");
  const messageNode = progress.querySelector(".progress-message");
  const detailNode = progress.querySelector(".progress-detail");

  if (bar) {
    bar.style.width = `${visiblePercent}%`;
  }

  if (progressTrack) {
    progressTrack.setAttribute("aria-valuenow", String(Math.round(percent)));
  }

  if (messageNode && state.message) {
    messageNode.textContent = state.message;
  }

  if (detailNode && state.detail) {
    detailNode.textContent = formatDisplayedProgressDetail(state.detail, percent);
  }

  if (percent >= 100) {
    progress.dataset.complete = "true";
    progress.dataset.completedAt = progress.dataset.completedAt || String(Date.now());
    scheduleFrontendProgressClose(progress, completionKey);
    return;
  }

  progress.dataset.complete = "false";
  progress.dataset.closeScheduled = "false";
  progress.dataset.completionKey = "";
  progress.dataset.completedAt = "";
  suppressedCompletedProgressKey = null;
  clearFrontendProgressCloseTimer();
}

function showFrontendProgress(ownerKey, label = "Analysis", state = null, completionKey = null) {
  const slot = resolveSlotByOwner(ownerKey);

  if (!slot) {
    return null;
  }

  document.querySelectorAll(".rnameta-frontend-progress").forEach((progress) => {
    if (progress.parentElement !== slot) {
      progress.remove();
    }
  });

  let progress = slot.querySelector(".rnameta-frontend-progress");

  if (!progress) {
    const initial = buildInitialProgressState(label);
    progress = document.createElement("div");
    progress.className = "rnameta-frontend-progress rnameta-progress-panel";
    progress.innerHTML = `
    <div class="rnameta-progress-card">
      <div class="progress" role="progressbar" aria-valuemin="0" aria-valuemax="100" aria-valuenow="0">
        <div class="progress-bar" style="width: 6%;"></div>
      </div>
      <div class="progress-text">
        <span class="progress-message">${escapeHtml(initial.message)}</span>
        <span class="progress-detail">${escapeHtml(initial.detail)}</span>
      </div>
    </div>
  `;

    slot.appendChild(progress);
  }

  progress.dataset.ownerKey = ownerKey || "";
  updateFrontendProgress(progress, state || buildInitialProgressState(label), completionKey);

  return progress;
}

function hideShinyProgressPanel(panel) {
  if (!panel) {
    return;
  }

  panel.classList.remove("rnameta-progress-panel");
  panel.classList.add("rnameta-progress-source");

  if (panel.parentElement !== document.body) {
    document.body.appendChild(panel);
  }
}

function dockProgressPanel() {
  const panel = document.getElementById("shiny-notification-panel");
  const slots = Array.from(document.querySelectorAll("[data-rnameta-progress-slot]"));

  if (!panel || !slots.length) {
    return;
  }

  const shinyProgressState = readShinyProgressState(panel);

  if (!shinyProgressState) {
    delete panel.dataset.progressOwner;
    panel.classList.remove("rnameta-progress-source");

    const completedProgress = document.querySelector(".rnameta-frontend-progress[data-complete=\"true\"]");

    if (completedProgress) {
      scheduleFrontendProgressClose(completedProgress, completedProgress.dataset.completionKey);
    } else if (!window.__rnametaAnalysisLocked) {
      removeFrontendProgress();
    }

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

  const ownerKey = slot.dataset?.rnametaProgressSlot || panel.dataset.progressOwner || null;
  const completionKey = getProgressCompletionKey(ownerKey, shinyProgressState);

  if (shinyProgressState.percent >= 100 && suppressedCompletedProgressKey === completionKey) {
    hideShinyProgressPanel(panel);
    return;
  }

  showFrontendProgress(
    ownerKey,
    window.__rnametaAnalysisLabel || shinyProgressState.message || "Analysis",
    shinyProgressState,
    completionKey
  );
  hideShinyProgressPanel(panel);
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

  window.addEventListener("rnameta:analysis-lock-state", (event) => {
    const detail = event.detail || {};

    if (detail.locked) {
      showFrontendProgress(detail.owner || null, detail.label || "Analysis");
      return;
    }

    const progress = document.querySelector(".rnameta-frontend-progress");
    const percent = parsePercent(progress?.querySelector(".progress")?.getAttribute("aria-valuenow"));

    if (progress && percent != null && percent >= 100) {
      scheduleFrontendProgressClose(progress, progress.dataset.completionKey);
      return;
    }

    removeFrontendProgress();
  });

  window.addEventListener("rnameta:results-rendered", () => {
    lastFrontendResultRenderAt = Date.now();
  });

  const observer = new MutationObserver(() => {
    runDock();
  });

  observer.observe(document.body, {
    childList: true,
    subtree: true,
    characterData: true,
    attributes: true,
    attributeFilter: ["class", "style", "aria-expanded"]
  });
}
