import { setShinyInputValue } from "../utils/shinyBridge";

export default function ModuleExport({ config }) {
  const buttons = config.buttons || [];

  return (
    <div className="ribote-export-card">
      <div className="ribote-export-card__title">Export Zone</div>
      <div className="ribote-export-buttons">
        {buttons.map((button) => (
          <button
            key={button.inputId}
            type="button"
            className="ribote-btn ribote-btn--secondary ribote-btn--block"
            onClick={() => {
              setShinyInputValue(button.inputId, String(Date.now()), {
                priority: "event"
              });
            }}
          >
            {button.label}
          </button>
        ))}
      </div>
    </div>
  );
}
