import { useEffect, useMemo, useRef, useState } from "react";
import * as d3 from "d3";
import { setShinyInputValue } from "../utils/shinyBridge";
import {
  buildLegendCountsWithMeta,
  drawMarginalDensityScatterChart,
  drawScatterChart,
  ensureActiveViewStore,
  formatLogTick,
  mapExpressionScatterData,
  mapScatterData,
  mapVolcanoData,
  readPersistedActiveView,
  shallowObjectArrayEqual,
  useD3Chart
} from "./TranslationEfficiencyCharts.jsx";
export default function TranslationEfficiencyResults({ config }) {
  const table = config.table || {};
  const columns = table.columns || [];
  const rows = table.rows || [];
  const page = table.page || 1;
  const pageCount = table.pageCount || 1;
  const totalRows = table.totalRows || rows.length;
  const pageInputId = table.pageInputId;
  const searchQuery = table.searchQuery || "";
  const searchInputId = table.searchInputId;
  const activeViewInputId = table.activeViewInputId;
  const persistedActiveViewKey = activeViewInputId ? `ribote-active-view:${activeViewInputId}` : null;
  const configuredActiveView = table.activeView || "data";
  const [activeTab, setActiveTab] = useState(() => readPersistedActiveView(persistedActiveViewKey) || configuredActiveView);
  const pendingActiveViewRef = useRef(null);
  const lastConfiguredActiveViewRef = useRef(configuredActiveView);
  const [searchValue, setSearchValue] = useState("");
  const [pageJumpValue, setPageJumpValue] = useState("");
  const renderReadyInputId = config.renderReadyInputId;
  const renderToken = config.renderToken || "";
  const thresholds = config.thresholds || {};
  const volcanoThresholdX = Number.isFinite(Number(thresholds.foldChange)) ? Number(thresholds.foldChange) : 0;
  const volcanoThresholdY = Number.isFinite(Number(thresholds.significance)) ? Number(thresholds.significance) : 0;

  const incomingVolcanoData = useMemo(() => mapVolcanoData(config.charts?.volcano), [config.charts?.volcano]);
  const incomingInputScatterData = useMemo(
    () => mapExpressionScatterData(config.charts?.scatterInput, "input1", "input2", "diffExp"),
    [config.charts?.scatterInput]
  );
  const incomingScatterRpfData = useMemo(() => mapScatterData(config.charts?.scatter, "log2RPFFC"), [config.charts?.scatter]);
  const incomingTeExpressionScatterData = useMemo(
    () => mapExpressionScatterData(config.charts?.scatterTeExpression, "TE_A1", "TE_A2", "diffTE"),
    [config.charts?.scatterTeExpression]
  );
  const incomingScatterTeData = useMemo(() => mapScatterData(config.charts?.scatterTe, "log2FoldChange"), [config.charts?.scatterTe]);
  const chartDisplayMeta = config.charts?.displayMeta || {};
  const [cachedVolcanoData, setCachedVolcanoData] = useState([]);
  const [cachedInputScatterData, setCachedInputScatterData] = useState([]);
  const [cachedScatterRpfData, setCachedScatterRpfData] = useState([]);
  const [cachedTeExpressionScatterData, setCachedTeExpressionScatterData] = useState([]);
  const [cachedScatterTeData, setCachedScatterTeData] = useState([]);
  const volcanoData = cachedVolcanoData.length > 0 ? cachedVolcanoData : incomingVolcanoData;
  const inputScatterData = cachedInputScatterData.length > 0 ? cachedInputScatterData : incomingInputScatterData;
  const scatterRpfData = cachedScatterRpfData.length > 0 ? cachedScatterRpfData : incomingScatterRpfData;
  const teExpressionScatterData = cachedTeExpressionScatterData.length > 0 ? cachedTeExpressionScatterData : incomingTeExpressionScatterData;
  const scatterTeData = cachedScatterTeData.length > 0 ? cachedScatterTeData : incomingScatterTeData;
  const volcanoLegendCounts = useMemo(() => buildLegendCountsWithMeta(volcanoData, chartDisplayMeta.volcano), [volcanoData, chartDisplayMeta.volcano]);
  const inputExpressionLegendCounts = useMemo(() => buildLegendCountsWithMeta(inputScatterData, chartDisplayMeta.scatterInput), [inputScatterData, chartDisplayMeta.scatterInput]);
  const teExpressionLegendCounts = useMemo(() => buildLegendCountsWithMeta(teExpressionScatterData, chartDisplayMeta.scatterTeExpression), [teExpressionScatterData, chartDisplayMeta.scatterTeExpression]);
  const scatterTeLegendCounts = useMemo(() => buildLegendCountsWithMeta(scatterTeData, chartDisplayMeta.scatterTe), [scatterTeData, chartDisplayMeta.scatterTe]);

  useEffect(() => {
    if (renderToken) {
      setCachedVolcanoData([]);
      setCachedInputScatterData([]);
      setCachedScatterRpfData([]);
      setCachedTeExpressionScatterData([]);
      setCachedScatterTeData([]);
    }
  }, [renderToken]);

  useEffect(() => {
    if (incomingVolcanoData.length > 0) {
      setCachedVolcanoData((currentData) => (
        shallowObjectArrayEqual(currentData, incomingVolcanoData) ? currentData : incomingVolcanoData
      ));
    }
  }, [incomingVolcanoData]);

  useEffect(() => {
    if (incomingInputScatterData.length > 0) {
      setCachedInputScatterData((currentData) => (
        shallowObjectArrayEqual(currentData, incomingInputScatterData) ? currentData : incomingInputScatterData
      ));
    }
  }, [incomingInputScatterData]);

  useEffect(() => {
    if (incomingScatterRpfData.length > 0) {
      setCachedScatterRpfData((currentData) => (
        shallowObjectArrayEqual(currentData, incomingScatterRpfData) ? currentData : incomingScatterRpfData
      ));
    }
  }, [incomingScatterRpfData]);

  useEffect(() => {
    if (incomingTeExpressionScatterData.length > 0) {
      setCachedTeExpressionScatterData((currentData) => (
        shallowObjectArrayEqual(currentData, incomingTeExpressionScatterData) ? currentData : incomingTeExpressionScatterData
      ));
    }
  }, [incomingTeExpressionScatterData]);

  useEffect(() => {
    if (incomingScatterTeData.length > 0) {
      setCachedScatterTeData((currentData) => (
        shallowObjectArrayEqual(currentData, incomingScatterTeData) ? currentData : incomingScatterTeData
      ));
    }
  }, [incomingScatterTeData]);

  const volcanoRef = useD3Chart((element) => drawScatterChart(element, volcanoData, {
    title: "Translation Efficiency Volcano Plot",
    titleClass: "ribote-d3-chart-title--qc",
    xLabel: "log2 Fold Change",
    yLabel: "-log10 Significance",
    chartHeight: 600,
    legendCounts: volcanoLegendCounts,
    referenceLines: {
      x: [-volcanoThresholdX, volcanoThresholdX],
      y: [volcanoThresholdY]
    },
    displayMeta: chartDisplayMeta.volcano,
    extraRows: (item) => `
      <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">pvalue:</span><span class="ribote-d3-tooltip__value">${d3.format(".3g")(item.pvalue)}</span></div>
      <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">padj:</span><span class="ribote-d3-tooltip__value">${d3.format(".3g")(item.padj)}</span></div>`
  }), [activeTab, volcanoData, volcanoLegendCounts, volcanoThresholdX, volcanoThresholdY, chartDisplayMeta.volcano]);
  const teExpressionScatterRef = useD3Chart((element) => drawScatterChart(element, teExpressionScatterData, {
    title: "Translation Efficiency by Group",
    titleClass: "ribote-d3-chart-title--library",
    xLabel: "Translation efficiency (Control, log2)",
    yLabel: "Translation efficiency (Treatment, log2)",
    chartHeight: 600,
    scaleType: "log2",
    legendCounts: teExpressionLegendCounts,
    displayMeta: chartDisplayMeta.scatterTeExpression,
    extraRows: (item) => `
      <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">TE_A1:</span><span class="ribote-d3-tooltip__value">${formatLogTick(item.x)}</span></div>
      <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">TE_A2:</span><span class="ribote-d3-tooltip__value">${formatLogTick(item.y)}</span></div>`
  }), [activeTab, teExpressionLegendCounts, teExpressionScatterData, chartDisplayMeta.scatterTeExpression]);
  const inputScatterRef = useD3Chart((element) => drawScatterChart(element, inputScatterData, {
    title: "RNA Expression by Group",
    titleClass: "ribote-d3-chart-title--library",
    xLabel: "RNA expression (Control, log2)",
    yLabel: "RNA expression (Treatment, log2)",
    chartHeight: 600,
    scaleType: "log2",
    legendCounts: inputExpressionLegendCounts,
    displayMeta: chartDisplayMeta.scatterInput,
    extraRows: (item) => `
      <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Input1:</span><span class="ribote-d3-tooltip__value">${formatLogTick(item.x)}</span></div>
      <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">Input2:</span><span class="ribote-d3-tooltip__value">${formatLogTick(item.y)}</span></div>`
  }), [activeTab, inputExpressionLegendCounts, inputScatterData, chartDisplayMeta.scatterInput]);
  const scatterRpfRef = useD3Chart((element) => drawScatterChart(element, scatterRpfData, {
    title: "RNA vs Ribo Fold Change",
    titleClass: "ribote-d3-chart-title--library",
    xLabel: "RNA_log2FC",
    yLabel: "Ribo_log2FC",
    chartHeight: 600,
    showLegend: false,
    showCorrelation: true,
    pointColor: "#7b8d94",
    displayMeta: chartDisplayMeta.scatter,
    extraRows: (item) => `
      <div class="ribote-d3-tooltip__row"><span class="ribote-d3-tooltip__key">TE_log2FC:</span><span class="ribote-d3-tooltip__value">${d3.format(".3f")(item.te)}</span></div>`
  }), [activeTab, scatterRpfData, chartDisplayMeta.scatter]);
  const scatterTeRef = useD3Chart((element) => drawMarginalDensityScatterChart(element, scatterTeData, {
    title: "RNA vs TE Fold Change",
    titleClass: "ribote-d3-chart-title--library",
    xLabel: "RNA_log2FC",
    yLabel: "TE_log2FC",
    chartHeight: 600,
    legendCounts: scatterTeLegendCounts,
    displayMeta: chartDisplayMeta.scatterTe,
  }), [activeTab, scatterTeData, scatterTeLegendCounts, chartDisplayMeta.scatterTe]);

  useEffect(() => {
    setSearchValue(searchQuery);
  }, [searchQuery]);

  useEffect(() => {
    setPageJumpValue(String(page));
  }, [page]);

  useEffect(() => {
    if (!persistedActiveViewKey || !activeTab) {
      return;
    }

    ensureActiveViewStore()[persistedActiveViewKey] = activeTab;
  }, [activeTab, persistedActiveViewKey]);

  useEffect(() => {
    const pendingView = pendingActiveViewRef.current;
    const persistedActiveView = readPersistedActiveView(persistedActiveViewKey);
    const configuredChanged = lastConfiguredActiveViewRef.current !== configuredActiveView;

    lastConfiguredActiveViewRef.current = configuredActiveView;

    if (pendingView) {
      if (configuredActiveView === pendingView) {
        pendingActiveViewRef.current = null;
      } else if (configuredActiveView !== activeTab) {
        return;
      }
    }

    if (!configuredChanged && persistedActiveView && activeTab === persistedActiveView) {
      return;
    }

    setActiveTab((currentTab) => (currentTab === configuredActiveView ? currentTab : configuredActiveView));
  }, [activeTab, configuredActiveView, persistedActiveViewKey]);

  useEffect(() => {
    if (!activeViewInputId || activeTab === configuredActiveView) {
      return;
    }

    setShinyInputValue(activeViewInputId, activeTab, { priority: "event" });
  }, [activeTab, activeViewInputId, configuredActiveView]);

  useEffect(() => {
    if (!renderReadyInputId || !renderToken) {
      return undefined;
    }

    let firstFrame = null;
    let secondFrame = null;

    firstFrame = requestAnimationFrame(() => {
      secondFrame = requestAnimationFrame(() => {
        setShinyInputValue(renderReadyInputId, renderToken, { priority: "event" });
      });
    });

    return () => {
      if (firstFrame !== null) {
        cancelAnimationFrame(firstFrame);
      }

      if (secondFrame !== null) {
        cancelAnimationFrame(secondFrame);
      }
    };
  }, [
    activeTab,
    columns.length,
    rows.length,
    renderReadyInputId,
    renderToken,
    inputScatterData.length,
    teExpressionScatterData.length,
    scatterTeData.length,
    volcanoData.length
  ]);

  const isVolcanoReady = volcanoData.length > 0;
  const isScatterReady = inputScatterData.length > 0 &&
    teExpressionScatterData.length > 0 &&
    scatterTeData.length > 0;

  const submitSearch = () => {
    if (!searchInputId) {
      return;
    }

    setShinyInputValue(searchInputId, searchValue, { priority: "event" });
  };

  const submitPageJump = () => {
    if (!pageInputId) {
      return;
    }

    const requestedPage = Number.parseInt(pageJumpValue, 10);
    if (Number.isNaN(requestedPage)) {
      setPageJumpValue(String(page));
      return;
    }

    const normalizedPage = Math.max(1, Math.min(pageCount, requestedPage));
    setShinyInputValue(pageInputId, String(normalizedPage), { priority: "event" });
  };

  const handleTabChange = (nextTab) => {
    if (nextTab === activeTab) {
      return;
    }

    pendingActiveViewRef.current = nextTab;
    setActiveTab(nextTab);
  };

  return (
    <div className="ribote-translation-results">
      <div className="ribote-canvas-tabs ribote-canvas-tabs--results">
        {[
          { id: "data", title: "Data" },
          { id: "volcano", title: "TE Volcano Plot" },
          { id: "scatter", title: "TE Scatter Plots" }
        ].map((tab) => (
          <button
            key={tab.id}
            type="button"
            className={`ribote-canvas-tab${tab.id === activeTab ? " is-active" : ""}`}
            onClick={() => handleTabChange(tab.id)}
          >
            {tab.title}
          </button>
        ))}
      </div>

      {activeTab === "data" ? (
        <div className="ribote-translation-panel ribote-translation-panel--data">
          <div className="ribote-table-toolbar">
            <label className="ribote-table-toolbar__search">
              <span>Search</span>
              <div className="ribote-table-toolbar__search-controls">
                <input
                  type="text"
                  className="ribote-input"
                  value={searchValue}
                  onChange={(event) => setSearchValue(event.target.value)}
                  onKeyDown={(event) => {
                    if (event.key === "Enter") {
                      event.preventDefault();
                      submitSearch();
                    }
                  }}
                  placeholder="Search rows"
                />
                <button type="button" className="ribote-btn ribote-btn--secondary" onClick={submitSearch}>
                  Apply
                </button>
              </div>
            </label>
            <div className="ribote-table-toolbar__page-jump">
              <span>Go to page</span>
              <div className="ribote-table-toolbar__page-jump-controls">
                <input
                  type="number"
                  min="1"
                  max={pageCount}
                  className="ribote-input"
                  value={pageJumpValue}
                  onChange={(event) => setPageJumpValue(event.target.value)}
                  onKeyDown={(event) => {
                    if (event.key === "Enter") {
                      event.preventDefault();
                      submitPageJump();
                    }
                  }}
                />
                <button type="button" className="ribote-btn ribote-btn--secondary" onClick={submitPageJump}>
                  Go
                </button>
              </div>
            </div>
          </div>
          <div className="ribote-table-wrap ribote-table-wrap--te">
            <table className="ribote-preview-table ribote-preview-table--te">
              <thead>
                <tr>
                  {columns.map((column) => (
                    <th key={column}>{column}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {rows.length > 0 ? (
                  rows.map((row, rowIndex) => (
                    <tr key={`row-${rowIndex}`}>
                      {columns.map((column) => (
                        <td key={`${rowIndex}-${column}`}>{formatTeTableCell(row[column])}</td>
                      ))}
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td colSpan={columns.length || 1} className="ribote-preview-table__empty">
                      No rows matched the current search.
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
          <div className="ribote-table-pagination">
            <span className="ribote-table-pagination__meta">
              Page {page} of {pageCount} | {totalRows} rows
            </span>
            <div className="ribote-table-pagination__actions">
              <button
                type="button"
                className="ribote-btn ribote-btn--secondary"
                disabled={page <= 1}
                onClick={() => setShinyInputValue(pageInputId, String(page - 1), { priority: "event" })}
              >
                Previous
              </button>
              <button
                type="button"
                className="ribote-btn ribote-btn--secondary"
                disabled={page >= pageCount}
                onClick={() => setShinyInputValue(pageInputId, String(page + 1), { priority: "event" })}
              >
                Next
              </button>
            </div>
          </div>
        </div>
      ) : null}

      {activeTab === "volcano" ? (
        <div className="ribote-translation-panel ribote-translation-panel--volcano">
          <div className="ribote-d3-card">
            {isVolcanoReady ? (
              <div ref={volcanoRef} className="ribote-d3-host" />
            ) : (
              <div className="ribote-d3-loading">Preparing volcano plot...</div>
            )}
          </div>
        </div>
      ) : null}

      {activeTab === "scatter" ? (
        <div className="ribote-translation-panel ribote-translation-panel--scatter">
          <div className="ribote-d3-card">
            {isScatterReady ? (
              <div ref={teExpressionScatterRef} className="ribote-d3-host" />
            ) : (
              <div className="ribote-d3-loading">Preparing scatter plots...</div>
            )}
          </div>
          <div className="ribote-d3-card">
            {isScatterReady ? (
              <div ref={inputScatterRef} className="ribote-d3-host" />
            ) : (
              <div className="ribote-d3-loading">Preparing scatter plots...</div>
            )}
          </div>
          {/* Temporary hide RNA vs Ribo Fold Change in the TE workspace.
          <div className="ribote-d3-card">
            {isScatterReady ? (
              <div ref={scatterRpfRef} className="ribote-d3-host" />
            ) : (
              <div className="ribote-d3-loading">Preparing scatter plots...</div>
            )}
          </div>
          */}
          <div className="ribote-d3-card">
            {isScatterReady ? (
              <div ref={scatterTeRef} className="ribote-d3-host" />
            ) : (
              <div className="ribote-d3-loading">Preparing scatter plots...</div>
            )}
          </div>
        </div>
      ) : null}
    </div>
  );
}

function formatTeTableCell(value) {
  if (value === null || value === undefined) {
    return "";
  }

  const raw = String(value).trim();
  if (!raw) {
    return "";
  }

  if (/^0\d+/u.test(raw) || !/^[+-]?(?:\d+\.?\d*|\.\d+)(?:e[+-]?\d+)?$/iu.test(raw)) {
    return raw;
  }

  const numeric = Number(raw);
  if (!Number.isFinite(numeric)) {
    return raw;
  }

  if (Number.isInteger(numeric) && !/[.eE]/.test(raw)) {
    return raw;
  }

  return numeric.toFixed(4);
}
