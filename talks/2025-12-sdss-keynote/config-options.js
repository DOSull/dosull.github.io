// Full list of configuration options available here:
// https://github.com/hakimel/reveal.js#configuration
Reveal.initialize({
  controls: true,
  controlsTutorial: true,
  progress: true,
  loop: false,
  slideNumber: true,
  history: true,
  center: true,
  rollingLinks: false,
  previewLinks: false,
  viewDistance: 3,
  touch: true,
  width: 1440,
  height: 1080,
  margin: 0.05,
  minScale: 0.2,
  maxScale: 1.0,
  backgroundTransition: 'none',  //'none'
  transition: 'linear', // default/cube/page/concave/zoom/linear/fade/none
  controls: true,
  progress: true,
  center: true,
  hash: true,
  // Learn about plugins: https://revealjs.com/plugins/
  plugins: [ RevealMath.MathJax3, RevealZoom, RevealNotes, RevealSearch, RevealMarkdown, RevealHighlight ]
});
