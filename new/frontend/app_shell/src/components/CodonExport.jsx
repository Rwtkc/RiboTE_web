import { useEffect, useRef, useState } from "react";
import NetworkExport from "./NetworkExport";

const HIDE_DELAY_MS = 220;

function ensureActiveViewStore() {
  return (window.__riboteActiveViewState ??= {});
}

function readPersistedActiveView(key) {
  if (!key) {
    return null;
  }

  return ensureActiveViewStore()[key] || null;
}

export default function CodonExport({ config }) {
  const activeViewInputId = config?.activeViewInputId || "";
  const persistedActiveViewKey = activeViewInputId ? `ribote-active-view:${activeViewInputId}` : null;
  const [desiredView, setDesiredView] = useState(() => readPersistedActiveView(persistedActiveViewKey));
  const desiredViewHasData = desiredView ? Boolean(config?.readyByView?.[desiredView]) : null;
  const shouldHideStaleExport = Boolean(desiredView && desiredView !== config?.currentView && desiredViewHasData === false);
  const [renderConfig, setRenderConfig] = useState(() => (config?.ready ? config : null));
  const [isVisible, setIsVisible] = useState(() => Boolean(config?.ready));
  const hideTimerRef = useRef(null);

  useEffect(() => {
    setDesiredView(readPersistedActiveView(persistedActiveViewKey));
  }, [persistedActiveViewKey]);

  useEffect(() => {
    const handleActiveViewChange = (event) => {
      if (event?.detail?.key !== persistedActiveViewKey) {
        return;
      }

      setDesiredView(event.detail.view || null);
    };

    window.addEventListener("ribote:active-view-change", handleActiveViewChange);
    return () => {
      window.removeEventListener("ribote:active-view-change", handleActiveViewChange);
    };
  }, [persistedActiveViewKey]);

  useEffect(() => {
    if (hideTimerRef.current) {
      window.clearTimeout(hideTimerRef.current);
      hideTimerRef.current = null;
    }

    if (shouldHideStaleExport) {
      setRenderConfig(null);
      setIsVisible(false);
      return undefined;
    }

    if (config?.ready) {
      setRenderConfig(config);
      setIsVisible(true);
      return undefined;
    }

    if (!renderConfig) {
      setIsVisible(false);
      return undefined;
    }

    setIsVisible(false);
    hideTimerRef.current = window.setTimeout(() => {
      setRenderConfig(null);
      hideTimerRef.current = null;
    }, HIDE_DELAY_MS);

    return () => {
      if (hideTimerRef.current) {
        window.clearTimeout(hideTimerRef.current);
        hideTimerRef.current = null;
      }
    };
  }, [config, renderConfig, shouldHideStaleExport]);

  if (!renderConfig && !isVisible) {
    return null;
  }

  return (
    <div
      className="ribote-export-shell"
      data-ribote-export-ready={isVisible ? "true" : "false"}
      aria-hidden={isVisible ? "false" : "true"}
    >
      <NetworkExport config={renderConfig || { ready: false }} />
    </div>
  );
}
