export default function HeroPanel({ hero }) {
  const badges = hero.badges || [];
  const logoSrc = hero.logo_src || "";
  const logoAlt = hero.logo_alt || `${hero.title || "RiboTE"} logo`;

  return (
    <section className="welcome-layout">
      <div className="welcome-hero">
        <div className="welcome-hero__grid">
          <div className="welcome-hero__copy">
            <div className="eyebrow eyebrow-light">
              {hero.eyebrow || "Translation Efficiency Analysis Workspace"}
            </div>
            <h1>{hero.title || "RiboTE"}</h1>
            <p className="welcome-hero__lead">
              {hero.description ||
                "RiboTE reorganizes matched RNA and Ribo count analysis into modular interpretation views."}
            </p>
            <p className="welcome-hero__supporting">
              {hero.supporting ||
                "RiboTE helps move from count matrices to translation-aware interpretation surfaces."}
            </p>
            <div className="welcome-badges">
              {badges.map((item) => (
                <div key={item} className="welcome-badge">
                  <span>{item}</span>
                </div>
              ))}
            </div>
          </div>
          {logoSrc ? (
            <div className="welcome-hero__media">
              <img
                className="welcome-hero__logo"
                src={logoSrc}
                alt={logoAlt}
              />
            </div>
          ) : null}
        </div>
      </div>
    </section>
  );
}
