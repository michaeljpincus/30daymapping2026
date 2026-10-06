import * as turf from '@turf/turf';
import fs from 'node:fs';

const countries = JSON.parse(fs.readFileSync('map_countries.geojson', 'utf8'));

const steps = 8;
const maxDist = 60; // km of glow depth, adjust to taste
const bands = [];

for (const raw of countries.features) {
  const f = turf.simplify(raw, { tolerance: 0.01, highQuality: false });

  // Buffer each level once
  const levels = [f];
  for (let i = 1; i <= steps; i++) {
    levels.push(turf.buffer(f, -(i * maxDist / steps), { units: 'kilometers' }));
  }

  for (let i = 0; i < steps; i++) {
    const outer = levels[i];
    const inner = levels[i + 1];
    if (!outer) break; // polygon fully shrunk away

    const band = inner ? turf.difference(turf.featureCollection([outer, inner])) : outer;
    if (band) {
      band.properties = {
        iso: raw.properties.ISO_A3,
        opacity: 0.5 * (1 - i / steps)
      };
      bands.push(band);
    }
  }
}

fs.writeFileSync('glow_bands.geojson', JSON.stringify(turf.featureCollection(bands)));
console.log(`Wrote ${bands.length} bands`);