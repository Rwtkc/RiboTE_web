import { useEffect, useMemo, useState } from "react";

export default function ModuleCanvas({ config }) {
  const views = config.views || [];
  const showPlaceholderViewContent = config.showPlaceholderViewContent !== false;
  const [activeId, setActiveId] = useState(views[0]?.id || null);
  const [activeGroupLabel, setActiveGroupLabel] = useState(null);

  useEffect(() => {
    setActiveId(views[0]?.id || null);
  }, [views]);

  const activeView = useMemo(() => {
    return views.find((view) => view.id === activeId) || views[0] || null;
  }, [activeId, views]);

  const groupedViews = useMemo(() => {
    const groups = [];
    const groupIndex = new Map();

    views.forEach((view) => {
      const groupLabel = view.group || "Views";

      if (!groupIndex.has(groupLabel)) {
        groupIndex.set(groupLabel, groups.length);
        groups.push({
          label: groupLabel,
          views: []
        });
      }

      groups[groupIndex.get(groupLabel)].views.push(view);
    });

    return groups;
  }, [views]);

  useEffect(() => {
    const nextActiveView = views.find((view) => view.id === activeId) || views[0] || null;
    const nextGroupLabel = nextActiveView?.group || groupedViews[0]?.label || null;
    setActiveGroupLabel(nextGroupLabel);
  }, [activeId, groupedViews, views]);

  const activeGroup = useMemo(() => {
    return groupedViews.find((group) => group.label === activeGroupLabel) || groupedViews[0] || null;
  }, [activeGroupLabel, groupedViews]);

  if (views.length === 0) {
    return null;
  }

  return (
    <div className="ribote-canvas-card">
      {config.showStatus !== false ? (
        <div className={`ribote-canvas-status ribote-canvas-status--${config.status || "idle"}`}>
          <span className="ribote-canvas-status__label">
            {config.status === "ready" ? "Result Layout Ready" : "Waiting for Run"}
          </span>
          <span className="ribote-canvas-status__copy">
            {config.summary || config.emptyMessage}
          </span>
        </div>
      ) : null}

      {views.length > 1 ? (
        <div className="ribote-canvas-tab-stack">
          {groupedViews.length > 1 ? (
            <div className="ribote-canvas-group-row">
              <div className="ribote-canvas-group-row__label">View Groups</div>
              <div className="ribote-canvas-group-tabs">
              {groupedViews.map((group) => (
                <button
                  key={group.label}
                  type="button"
                  className={`ribote-canvas-group-tab${group.label === activeGroup?.label ? " is-active" : ""}`}
                  onClick={() => {
                    setActiveGroupLabel(group.label);
                    if (!group.views.some((view) => view.id === activeId)) {
                      setActiveId(group.views[0]?.id || null);
                    }
                  }}
                >
                  {group.label}
                </button>
              ))}
              </div>
            </div>
          ) : null}

          {activeGroup ? (
            <div className="ribote-canvas-view-tab-panel">
              <div className="ribote-canvas-view-tab-panel__header">
                <span className="ribote-canvas-view-tab-panel__eyebrow">Current Group</span>
                <span className="ribote-canvas-view-tab-panel__title">{activeGroup.label}</span>
              </div>
              <div className="ribote-canvas-view-tab-panel__divider" />
              <div className="ribote-canvas-tabs ribote-canvas-tabs--results">
                {activeGroup.views.map((view) => (
                  <button
                    key={view.id}
                    type="button"
                    className={`ribote-canvas-tab${view.id === activeId ? " is-active" : ""}`}
                    onClick={() => setActiveId(view.id)}
                  >
                    {view.title}
                  </button>
                ))}
              </div>
            </div>
          ) : null}
        </div>
      ) : null}

      {activeView && showPlaceholderViewContent ? (
        <div className="ribote-canvas-view">
          <div className="ribote-canvas-view__header">
            <h4>{activeView.title}</h4>
            {config.showViewBadge !== false ? (
              <span className="ribote-canvas-view__badge">
                {config.status === "ready" ? "ready" : "idle"}
              </span>
            ) : null}
          </div>
          <p className="ribote-canvas-view__copy">{activeView.description}</p>
          <div className="ribote-canvas-view__frame">
            <div className="ribote-canvas-view__grid" />
            <div className="ribote-canvas-view__overlay">
              <span>{activeView.title}</span>
              <small>
                {config.status === "ready"
                  ? "This result panel layout is prepared and ready for the codon analysis output."
                  : "Run the module to prepare this result panel layout."}
              </small>
            </div>
          </div>
        </div>
      ) : null}
    </div>
  );
}
