import HeroPanel from "./components/HeroPanel";

export default function App({ config }) {
  const hero = config.hero || {};

  return (
    <div className="shell-app">
      <div className="shell-noise" aria-hidden="true" />
      <main className="shell-main">
        <HeroPanel hero={hero} />
      </main>
    </div>
  );
}
