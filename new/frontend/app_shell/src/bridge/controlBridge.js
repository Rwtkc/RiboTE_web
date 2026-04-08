import { createRoot } from "react-dom/client";
import { setShinyInputValue } from "../utils/shinyBridge";

function shieldReactManagedInputs(scope) {
  if (!scope) {
    return;
  }

  const mark = () => {
    scope.querySelectorAll("input, select, textarea").forEach((element) => {
      element.setAttribute("data-shiny-no-bind-input", "true");
    });
  };

  mark();
  window.Shiny?.unbindAll?.(scope);

  const observer = new MutationObserver(() => {
    mark();
    window.Shiny?.unbindAll?.(scope);
  });

  observer.observe(scope, {
    childList: true,
    subtree: true
  });
}

function ensureBridgeState() {
  return (window.__rnaMetaBridge ??= {
    handlersBound: false,
    updateControlListeners: [],
    clearControlListeners: [],
    latestControlEvents: {}
  });
}

function notifyHostReady(element) {
  const readyInputId = element?.dataset?.rnametaReadyInput;

  if (!readyInputId) {
    return;
  }

  setShinyInputValue(readyInputId, String(Date.now()), { priority: "event" });
}

export function initializeControlBridge({ allowedControls, renderControl }) {
  const allowed = new Set(allowedControls);
  const registry = new Map();
  const bridge = ensureBridgeState();

  function mountHost(element) {
    const control = element.dataset.rnametaControl;
    if (!control || !allowed.has(control)) {
      return;
    }

    const existingEntry = registry.get(element.id);
    if (existingEntry) {
      if (existingEntry.element === element && existingEntry.element.isConnected) {
        return;
      }

      existingEntry.root.unmount();
      registry.delete(element.id);
    }

    const root = createRoot(element);
    registry.set(element.id, { root, control, element });
    root.render(null);
    shieldReactManagedInputs(element);
    notifyHostReady(element);
  }

  function mountAllHosts() {
    document.querySelectorAll("[data-rnameta-control]").forEach((element) => {
      mountHost(element);
    });
  }

  function updateControl(message) {
    if (!allowed.has(message.control)) {
      return;
    }

    const current = registry.get(message.id);
    if (!current || !current.element.isConnected) {
      if (current && !current.element.isConnected) {
        current.root.unmount();
        registry.delete(message.id);
      }

      const element = document.getElementById(message.id);
      if (!element) {
        return;
      }

      mountHost(element);
    }

    const host = registry.get(message.id);
    if (!host || !host.element.isConnected) {
      return;
    }

    host.root.render(renderControl(message.control, message.config));
    shieldReactManagedInputs(host.element);
  }

  function clearControl(message) {
    if (!message.id) {
      registry.forEach((entry) => {
        entry.root.render(null);
      });
      return;
    }

    const host = registry.get(message.id);
    if (!host) {
      return;
    }

    host.root.render(null);
  }

  function bindShinyHandlers() {
    if (bridge.handlersBound) {
      return;
    }

    const addHandler = window.Shiny?.addCustomMessageHandler;
    if (typeof addHandler !== "function") {
      window.setTimeout(bindShinyHandlers, 60);
      return;
    }

    addHandler("rnameta:update-control", (message) => {
      bridge.latestControlEvents[message?.id ?? "__all__"] = {
        type: "update",
        message
      };
      bridge.updateControlListeners.forEach((listener) => {
        listener(message);
      });
    });

    addHandler("rnameta:clear-control", (message) => {
      bridge.latestControlEvents[message?.id ?? "__all__"] = {
        type: "clear",
        message: message ?? {}
      };
      bridge.clearControlListeners.forEach((listener) => {
        listener(message ?? {});
      });
    });

    bridge.handlersBound = true;
    registry.forEach((entry) => {
      if (entry.element?.isConnected) {
        notifyHostReady(entry.element);
      }
    });
  }

  function initializeControls() {
    mountAllHosts();
    bridge.updateControlListeners.push(updateControl);
    bridge.clearControlListeners.push(clearControl);
    bindShinyHandlers();

    Object.values(bridge.latestControlEvents).forEach((event) => {
      if (event.type === "update") {
        updateControl(event.message);
      } else {
        clearControl(event.message);
      }
    });

    if (document.body) {
      const observer = new MutationObserver(() => {
        mountAllHosts();
      });

      observer.observe(document.body, { childList: true, subtree: true });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeControls, { once: true });
  } else {
    initializeControls();
  }
}
