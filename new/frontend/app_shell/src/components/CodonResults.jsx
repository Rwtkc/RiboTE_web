import { useEffect, useMemo, useRef, useState } from "react";
import { setShinyInputValue } from "../utils/shinyBridge";
import {
  BiasByGroupView,
  CbiAssociationsView,
  CodonEnrichmentShiftedView,
  CodonHeatmapView,
  CodonRunEnrichmentView,
  CodonRunZscoreView,
  InputSummaryView,
  PermutationSupportView,
  SelectedCodonAcrossGroupsView,
  SelectedCodonBurdenView,
  SelectedLoadEffectView,
  TeBiasSelectedLoadView,
  UsageByGroupView,
  UsageVsRnaView
} from "./CodonResultsViews.jsx";
import {
  ensureActiveViewStore,
  ensureCodonGroupViewStore,
  ensureCodonViewRequestStore,
  normalizeViews,
  readAnalysisLocked,
  readPersistedActiveView,
  readPersistedGroupViews,
  readPersistedViewRequests
} from "./CodonResultsViewUtils.jsx";
export default function CodonResults({ config }) {
  const views = useMemo(() => normalizeViews(config?.views), [config?.views]);
  const validViewIds = useMemo(() => new Set(views.map((view) => view.id)), [views]);
  const activeViewInputId = config?.activeViewInputId || "";
  const persistedActiveViewKey = activeViewInputId ? `ribote-active-view:${activeViewInputId}` : null;
  const configuredActiveView = config?.activeView || views[0]?.id || "";
  const configuredActiveViewToken = Number.isFinite(Number(config?.activeViewToken)) ? Number(config.activeViewToken) : 0;
  const configuredActiveViewRequestSeq = Number.isFinite(Number(config?.activeViewRequestSeq))
    ? Number(config.activeViewRequestSeq)
    : 0;
  const initialRequestState = readPersistedViewRequests(persistedActiveViewKey);
  const [activeView, setActiveView] = useState(() => readPersistedActiveView(persistedActiveViewKey) || configuredActiveView);
  const pendingActiveViewRef = useRef(initialRequestState.pending);
  const queuedActiveViewRef = useRef(initialRequestState.queued);
  const nextResultViewRequestSeqRef = useRef(initialRequestState.nextRequestSeq);
  const lastConfiguredActiveViewRef = useRef(configuredActiveView);
  const lastConfiguredActiveViewTokenRef = useRef(configuredActiveViewToken);
  const lastConfiguredActiveViewRequestSeqRef = useRef(configuredActiveViewRequestSeq);
  const groupViewSelectionsRef = useRef(readPersistedGroupViews(persistedActiveViewKey));
  const [analysisLocked, setAnalysisLocked] = useState(() => readAnalysisLocked());
  const groups = useMemo(() => Array.from(new Set(views.map((view) => view.group))), [views]);
  const viewsByGroup = useMemo(() => views.reduce((catalog, view) => {
    if (!catalog[view.group]) {
      catalog[view.group] = [];
    }

    catalog[view.group].push(view);
    return catalog;
  }, {}), [views]);
  const currentView = views.find((view) => view.id === activeView) || views.find((view) => view.id === configuredActiveView) || views[0] || null;
  const activeGroup = currentView?.group || groups[0] || "";
  const groupViews = useMemo(() => views.filter((view) => view.group === activeGroup), [activeGroup, views]);
  const sidebarGroupHostId = config?.sidebarGroupHostId || "";
  const summaryHostId = config?.summaryHostId || "";

  useEffect(() => {
    if (!persistedActiveViewKey || !activeView) {
      return;
    }

    ensureActiveViewStore()[persistedActiveViewKey] = activeView;
    window.dispatchEvent(new CustomEvent("ribote:active-view-change", {
      detail: {
        key: persistedActiveViewKey,
        view: activeView
      }
    }));
  }, [activeView, persistedActiveViewKey]);

  useEffect(() => {
    groupViewSelectionsRef.current = readPersistedGroupViews(persistedActiveViewKey);
  }, [persistedActiveViewKey]);

  useEffect(() => {
    const requestState = readPersistedViewRequests(persistedActiveViewKey);
    pendingActiveViewRef.current = requestState.pending;
    queuedActiveViewRef.current = requestState.queued;
    nextResultViewRequestSeqRef.current = requestState.nextRequestSeq;
  }, [persistedActiveViewKey]);

  useEffect(() => {
    if (!persistedActiveViewKey || !currentView?.group || !currentView?.id) {
      return;
    }

    const nextSelections = {
      ...groupViewSelectionsRef.current,
      [currentView.group]: currentView.id
    };

    groupViewSelectionsRef.current = nextSelections;
    ensureCodonGroupViewStore()[persistedActiveViewKey] = nextSelections;
  }, [currentView?.group, currentView?.id, persistedActiveViewKey]);

  useEffect(() => {
    const handleLockState = (event) => {
      setAnalysisLocked(Boolean(event?.detail?.locked));
    };

    window.addEventListener("rnameta:analysis-lock-state", handleLockState);
    return () => {
      window.removeEventListener("rnameta:analysis-lock-state", handleLockState);
    };
  }, []);

  useEffect(() => {
    if (!sidebarGroupHostId) {
      return;
    }

    const host = document.getElementById(sidebarGroupHostId);
    if (!host) {
      return;
    }

    host.dataset.riboteCodonActiveGroup = activeGroup;
  }, [activeGroup, sidebarGroupHostId]);

  useEffect(() => {
    if (!summaryHostId) {
      return;
    }

    const host = document.getElementById(summaryHostId);
    if (!host) {
      return;
    }

    host.dataset.riboteCodonActiveGroup = activeGroup;
  }, [activeGroup, summaryHostId]);

  const dispatchActiveViewRequest = (request) => {
    if (!request || !activeViewInputId || analysisLocked) {
      return false;
    }

    pendingActiveViewRef.current = request;
    if (persistedActiveViewKey) {
      ensureCodonViewRequestStore()[persistedActiveViewKey] = {
        pending: pendingActiveViewRef.current,
        queued: queuedActiveViewRef.current,
        nextRequestSeq: nextResultViewRequestSeqRef.current
      };
    }
    setShinyInputValue(activeViewInputId, {
      view: request.id,
      seq: request.requestSeq
    }, { priority: "event" });
    return true;
  };

  useEffect(() => {
    const pendingView = pendingActiveViewRef.current;
    const queuedView = queuedActiveViewRef.current;
    const persistedActiveView = readPersistedActiveView(persistedActiveViewKey);
    const configuredChanged = lastConfiguredActiveViewRef.current !== configuredActiveView;
    const configuredTokenChanged = lastConfiguredActiveViewTokenRef.current !== configuredActiveViewToken;
    const configuredRequestSeqChanged = lastConfiguredActiveViewRequestSeqRef.current !== configuredActiveViewRequestSeq;
    const hasValidLocalView = activeView && validViewIds.has(activeView);

    lastConfiguredActiveViewRef.current = configuredActiveView;
    lastConfiguredActiveViewTokenRef.current = configuredActiveViewToken;
    lastConfiguredActiveViewRequestSeqRef.current = configuredActiveViewRequestSeq;

    if (pendingView) {
      const requestAcknowledged = configuredActiveView === pendingView.id
        && configuredActiveViewRequestSeq >= pendingView.requestSeq;

      if (requestAcknowledged) {
        pendingActiveViewRef.current = null;
        if (persistedActiveViewKey) {
          ensureCodonViewRequestStore()[persistedActiveViewKey] = {
            pending: null,
            queued: queuedActiveViewRef.current,
            nextRequestSeq: nextResultViewRequestSeqRef.current
          };
        }

        if (queuedView) {
          if (queuedView.id === configuredActiveView && configuredActiveViewRequestSeq >= queuedView.requestSeq) {
            queuedActiveViewRef.current = null;
            if (persistedActiveViewKey) {
              ensureCodonViewRequestStore()[persistedActiveViewKey] = {
                pending: null,
                queued: null,
                nextRequestSeq: nextResultViewRequestSeqRef.current
              };
            }
          } else if (dispatchActiveViewRequest(queuedView)) {
            queuedActiveViewRef.current = null;
            if (persistedActiveViewKey) {
              ensureCodonViewRequestStore()[persistedActiveViewKey] = {
                pending: pendingActiveViewRef.current,
                queued: null,
                nextRequestSeq: nextResultViewRequestSeqRef.current
              };
            }
            return;
          } else {
            return;
          }
        }
      } else {
        return;
      }
    }

    if (!configuredChanged) {
      if (!configuredTokenChanged && !configuredRequestSeqChanged) {
        if (persistedActiveView && activeView === persistedActiveView) {
          return;
        }

        if (hasValidLocalView && activeView !== configuredActiveView) {
          return;
        }
      }

      if (persistedActiveView && activeView === persistedActiveView) {
        return;
      }

      if (hasValidLocalView && activeView !== configuredActiveView) {
        return;
      }
    }

    if ((pendingActiveViewRef.current || queuedActiveViewRef.current) && hasValidLocalView && activeView !== configuredActiveView) {
      return;
    }

    setActiveView((current) => (current === configuredActiveView ? current : configuredActiveView));
  }, [
    activeView,
    activeViewInputId,
    analysisLocked,
    configuredActiveView,
    configuredActiveViewRequestSeq,
    configuredActiveViewToken,
    persistedActiveViewKey,
    validViewIds
  ]);

  useEffect(() => {
    if (!activeViewInputId || analysisLocked) {
      return undefined;
    }

    if (!pendingActiveViewRef.current && queuedActiveViewRef.current) {
      if (dispatchActiveViewRequest(queuedActiveViewRef.current)) {
        queuedActiveViewRef.current = null;
        if (persistedActiveViewKey) {
          ensureCodonViewRequestStore()[persistedActiveViewKey] = {
            pending: pendingActiveViewRef.current,
            queued: null,
            nextRequestSeq: nextResultViewRequestSeqRef.current
          };
        }
      }
    }
    return undefined;
  }, [activeViewInputId, analysisLocked, persistedActiveViewKey]);

  const handleViewChange = (nextView) => {
    if (!nextView || nextView === activeView) {
      return;
    }

    const nextRequest = {
      id: nextView,
      requestSeq: nextResultViewRequestSeqRef.current + 1
    };
    nextResultViewRequestSeqRef.current = nextRequest.requestSeq;
    if (persistedActiveViewKey) {
      ensureCodonViewRequestStore()[persistedActiveViewKey] = {
        pending: pendingActiveViewRef.current,
        queued: queuedActiveViewRef.current,
        nextRequestSeq: nextResultViewRequestSeqRef.current
      };
    }

    if (!pendingActiveViewRef.current) {
      if (!dispatchActiveViewRequest(nextRequest)) {
        queuedActiveViewRef.current = nextRequest;
        if (persistedActiveViewKey) {
          ensureCodonViewRequestStore()[persistedActiveViewKey] = {
            pending: pendingActiveViewRef.current,
            queued: queuedActiveViewRef.current,
            nextRequestSeq: nextResultViewRequestSeqRef.current
          };
        }
      }
    } else {
      queuedActiveViewRef.current = nextRequest;
      if (persistedActiveViewKey) {
        ensureCodonViewRequestStore()[persistedActiveViewKey] = {
          pending: pendingActiveViewRef.current,
          queued: queuedActiveViewRef.current,
          nextRequestSeq: nextResultViewRequestSeqRef.current
        };
      }
    }

    setActiveView(nextView);
  };

  const handleGroupChange = (nextGroup) => {
    if (!nextGroup || nextGroup === activeGroup) {
      return;
    }

    const rememberedViewId = groupViewSelectionsRef.current[nextGroup];
    const nextView = (viewsByGroup[nextGroup] || []).find((view) => view.id === rememberedViewId) || (viewsByGroup[nextGroup] || [])[0] || null;
    handleViewChange(nextView?.id || "");
  };

  useEffect(() => {
    let firstFrame = null;
    let secondFrame = null;

    firstFrame = window.requestAnimationFrame(() => {
      secondFrame = window.requestAnimationFrame(() => {
        window.dispatchEvent(new CustomEvent("rnameta:results-rendered", {
          detail: {
            module: "codon",
            view: activeView,
            token: configuredActiveViewToken
          }
        }));
      });
    });

    return () => {
      if (firstFrame !== null) {
        window.cancelAnimationFrame(firstFrame);
      }

      if (secondFrame !== null) {
        window.cancelAnimationFrame(secondFrame);
      }
    };
  }, [activeView, configuredActiveViewToken]);

  if (!views.length) {
    return null;
  }

  let content = null;
  if (currentView?.id === "input_summary") {
    content = <InputSummaryView viewConfig={config?.inputSummary} />;
  } else if (currentView?.id === "selected_codon_usage") {
    content = <UsageByGroupView viewConfig={config?.selectedCodonUsage} />;
  } else if (currentView?.id === "selected_codon_vs_rna") {
    content = <UsageVsRnaView viewConfig={config?.selectedCodonVsRna} />;
  } else if (currentView?.id === "cbi_tai_by_group") {
    content = <BiasByGroupView viewConfig={config?.cbiTaiByGroup} />;
  } else if (currentView?.id === "cbi_associations") {
    content = <CbiAssociationsView viewConfig={config?.cbiAssociations} />;
  } else if (currentView?.id === "selected_codon_burden") {
    content = <SelectedCodonBurdenView viewConfig={config?.selectedCodonBurden} />;
  } else if (currentView?.id === "codon_enrichment_shifted") {
    content = <CodonEnrichmentShiftedView viewConfig={config?.codonEnrichmentShifted} />;
  } else if (currentView?.id === "selected_codon_across_groups") {
    content = <SelectedCodonAcrossGroupsView viewConfig={config?.selectedCodonAcrossGroups} />;
  } else if (currentView?.id === "permutation_support") {
    content = <PermutationSupportView viewConfig={config?.permutationSupport} />;
  } else if (currentView?.id === "te_bias_selected_load") {
    content = <TeBiasSelectedLoadView viewConfig={config?.teBiasSelectedLoad} />;
  } else if (currentView?.id === "selected_load_effect") {
    content = <SelectedLoadEffectView viewConfig={config?.selectedLoadEffect} />;
  } else if (currentView?.id === "codon_clustering") {
    content = <CodonHeatmapView viewConfig={config?.codonClustering} />;
  } else if (currentView?.id === "codon_usage_heatmap") {
    content = <CodonHeatmapView viewConfig={config?.codonUsageHeatmap} />;
  } else if (currentView?.id === "codon_run_zscore") {
    content = <CodonRunZscoreView viewConfig={config?.codonRunZscore} />;
  } else if (currentView?.id === "codon_run_enrichment") {
    content = <CodonRunEnrichmentView viewConfig={config?.codonRunEnrichment} />;
  } else {
    content = null;
  }

  return (
    <div className="ribote-codon-results">
      {config?.note ? (
        <div className="ribote-result-card">
          <p className="ribote-result-card__copy">{config.note}</p>
        </div>
      ) : null}

      <div className="ribote-canvas-tab-stack">
        <div className="ribote-canvas-group-row">
          <div className="ribote-canvas-group-row__label">Result Groups</div>
          <div className="ribote-canvas-group-tabs">
            {groups.map((group) => (
              <button
                key={group}
                type="button"
                className={`ribote-canvas-group-tab${group === activeGroup ? " is-active" : ""}`}
                onClick={() => handleGroupChange(group)}
              >
                {group}
              </button>
            ))}
          </div>
        </div>

        <div className="ribote-canvas-view-tab-panel">
          <div className="ribote-canvas-view-tab-panel__header">
            <span className="ribote-canvas-view-tab-panel__eyebrow">{activeGroup}</span>
            <span className="ribote-canvas-view-tab-panel__title">{currentView?.title}</span>
          </div>
          <div className="ribote-canvas-view-tab-panel__divider" />
          <div className="ribote-canvas-tabs ribote-canvas-tabs--results">
            {groupViews.map((view) => (
              <button
                key={view.id}
                type="button"
                className={`ribote-canvas-tab${view.id === activeView ? " is-active" : ""}`}
                onClick={() => handleViewChange(view.id)}
              >
                {view.title}
              </button>
            ))}
          </div>
        </div>
      </div>

      {content}
    </div>
  );
}
