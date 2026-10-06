import * as maplibregl from 'https://unpkg.com/maplibre-gl@^6.12.0/dist/maplibre-gl.mjs';

const INITIAL_CENTER = [-69.96054583285647, -22.20155145232283];
const INITIAL_ZOOM = 3.1;

const chapters = [
  {
    intro: true,
    title: 'Your Story Title',
    body: 'A short introduction to what this story covers and how to navigate it.',
    center: INITIAL_CENTER,
    zoom: INITIAL_ZOOM
  },

  { iso: 'BRA', title: 'Brazil',     
    body: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',    
    image: 'images/brazil.jpg',
    imageCaption: 'Election posters for Bolsonaro. Source: CNN.',
    center: [-63, -15],   
    zoom: 3.8 
  },
  { iso: 'CHL', title: 'Chile',      body: 'Text about Chile…',      center: [-71.5, -37],   zoom: 3.7 },
  { iso: 'PER', title: 'Peru',       body: 'Text about Peru…',       center: [-75, -9.2],    zoom: 4.9 },
  { iso: 'COL', title: 'Colombia',   body: 'Text about Colombia…',   center: [-73, 4.2],     zoom: 5.20 },
  { iso: 'HND', title: 'Honduras',   body: 'Text about Honduras…',   center: [-86.6, 14.8],  zoom: 6.2 },
  { iso: 'CRI', title: 'Costa Rica', body: 'Text about Costa Rica…', center: [-84.2, 9.9],   zoom: 6.8 },
];


const map = new maplibregl.Map({
  container: 'map',
  center: INITIAL_CENTER,
  zoom: INITIAL_ZOOM,
  minZoom: 3,
  style: 'basemap/basemap.json' // My custom basemap!
});

map.addControl(new maplibregl.NavigationControl({ visualizePitch: true }), 'top-right');

map.on('load', () => {

  map.addSource('glow-bands', {
    type: 'geojson',
    data: 'glow_bands.geojson'
  });

  map.addLayer({
    id: 'country-glow',
    type: 'fill',
    source: 'glow-bands',
    filter: ['in', ['get', 'iso'], ['literal', chapters.filter(c => c.iso).map(c => c.iso)]],
    paint: {
      'fill-color': '#1b56b5',
      'fill-opacity': 0,
      'fill-opacity-transition': { duration: 0 },
      'fill-antialias': false,
    }
  }, 'cities');

  goTo(0);

});

let current = 0; 

let glowIso = null;
let glowRaf = null;
const GLOW_FADE_MS = 400;

function updateGlow(iso) {
  if (!map.getLayer('country-glow')) return;
  const prev = glowIso;
  const next = iso ?? null;
  if (prev === next) return;
  glowIso = next;

  if (glowRaf) cancelAnimationFrame(glowRaf);

  const start = performance.now();
  const step = (now) => {
    const p = Math.max(0, Math.min(1, (now - start) / GLOW_FADE_MS));
    map.setPaintProperty('country-glow', 'fill-opacity', [
      '*',
      ['get', 'opacity'],
      ['case',
        ['==', ['get', 'iso'], next ?? ''], p,
        ['==', ['get', 'iso'], prev ?? ''], 1 - p,
        0
      ]
    ]);
    glowRaf = p < 1 ? requestAnimationFrame(step) : null;
  };
  glowRaf = requestAnimationFrame(step);
}

function goTo(i) {
  current = Math.max(0, Math.min(chapters.length - 1, i));
  const c = chapters[current];

  document.getElementById('title').textContent = c.title;
  document.getElementById('body').textContent = c.body;

  // Hide the counter on the intro, and don't count the intro as a country
  const counter = document.getElementById('counter');
  counter.textContent = c.intro ? '' : `${current} / ${chapters.length - 1}`;

  const hero = document.getElementById('hero');
  hero.hidden = !c.image;
  if (c.image) {
    document.getElementById('hero-img').src = c.image;
    document.getElementById('hero-img').alt = c.imageAlt ?? '';
    document.getElementById('hero-caption').textContent = c.imageCaption ?? '';
  }
  
  // Relabel the button on the intro
  document.getElementById('next').textContent = c.intro ? 'Start' : 'Next';

  document.getElementById('prev').disabled = current === 0;
  document.getElementById('next').disabled = current === chapters.length - 1;

  // Optional: toggle a class so CSS can style the intro differently
  document.getElementById('story').classList.toggle('is-intro', !!c.intro);


  updateGlow(c.iso);

  map.flyTo({
    center: c.center,
    zoom: c.zoom,
    bearing: c.bearing ?? 0,
    pitch: c.pitch ?? 0,
    speed: 0.8,
    essential: true,
  });
}

document.getElementById('next').onclick = () => goTo(current + 1);
document.getElementById('prev').onclick = () => goTo(current - 1);
document.addEventListener('keydown', e => {
  if (e.key === 'ArrowRight') goTo(current + 1);
  if (e.key === 'ArrowLeft') goTo(current - 1);
});