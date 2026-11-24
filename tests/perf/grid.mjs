// What the accounting grid costs, measured in a real Chromium.   bash build/build.sh && node tests/perf/grid.mjs
//   1  a table that is turned: the cube built again from a million lines, as the module times it
//   2  the lines behind a cell, down to all million of them (one pass over the journal), as the module times it
//   3  scrolling: the callback of every animation frame that draws, timed in the page; once with the context's methods real, once with them empty (what is left is the module:
//      the scene, the cells, the crossing and the glue), so that the browser's share is told apart from ours
// Numbers vary by some percent between runs and a lot between machines (this is a software-rendered headless Chromium): compare before and after on one machine.
import { fileURLToPath } from 'node:url';
import { serve, launch } from '../../lib/runtime/tests/e2e/lib.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const median = (xs) => [...xs].sort((a, b) => a - b)[xs.length >> 1];
const p95 = (xs) => [...xs].sort((a, b) => a - b)[Math.min(xs.length - 1, Math.floor(xs.length * 0.95))];
const MOVES = 300;
const GRID_X = 16, GRID_Y = 182, HEAD_W = 240, HEAD_H = 32, ROW_H = 28, COL_W = 136;

async function session({ emptyCalls = false }) {
  const { server, base } = await serve(root);
  const { browser, page, errors } = await launch();
  try {
    await page.addInitScript((emptyCalls) => {
      window.__frames = [];
      let draws = 0;
      const proto = CanvasRenderingContext2D.prototype;
      if (emptyCalls) {
        for (const k of Object.getOwnPropertyNames(proto)) {
          const d = Object.getOwnPropertyDescriptor(proto, k);
          if (k === 'constructor' || k === 'measureText' || k === 'clearRect' || k === 'fillText') continue;
          if (typeof d.value === 'function') proto[k] = k.startsWith('create') ? d.value : function () {};
          else if (d.set && k !== 'font') Object.defineProperty(proto, k, { ...d, set() {} });       // the font stays: measureText must measure as it does when it draws
        }
      }
      // a frame that draws writes cells: count the text it draws (the host clears and fills in several ways, this is the one every frame of the grid has)
      const fillText = proto.fillText;
      proto.fillText = function (...a) { draws++; return emptyCalls ? undefined : fillText.apply(this, a); };
      const next = window.requestAnimationFrame.bind(window);
      window.requestAnimationFrame = (cb) => next((t) => { draws = 0; const t0 = performance.now(); cb(t); window.__frames.push({ ms: performance.now() - t0, draws }); });
    }, emptyCalls);
    await page.setViewportSize({ width: 1400, height: 1500 });
    await page.goto(`${base}/web/`);
    await page.waitForFunction(() => window.__module?.exports?.lines, null, { timeout: 60000 });
    await page.waitForTimeout(800);
    const ex = (name) => page.evaluate((n) => window.__module.exports[n](), name);
    const box = await page.locator('#gfx').boundingBox();
    const at = (x, y) => [box.x + x, box.y + y];
    return { page, ex, at, box, errors, close: async () => { await browser.close(); server.close(); } };
  } catch (e) { await browser.close(); server.close(); throw e; }
}

// 1 and 2: what the module times, with the context's methods real
{
  const s = await session({});
  const gen = await s.ex('generatedMs'), first = await s.ex('pivotMs');
  console.log(`the journal: ${(await s.ex('lines')).toLocaleString('en')} lines made in ${gen.toFixed(1)} ms; the first cube built in ${first.toFixed(1)} ms`);
  const buttons = [[330, 'Cost centres × Periods'], [470, 'Periods × Accounts'], [640, 'Accounts × Cost centres'], [120, 'Accounts × Periods']];
  const turns = {};
  for (let round = 0; round < 3; round++) {
    for (const [x, name] of buttons) {
      await s.page.mouse.click(...s.at(x, 54 + 17));
      await s.page.waitForTimeout(250);
      (turns[name] ??= []).push(await s.ex('pivotMs'));
    }
  }
  for (const [, name] of buttons) console.log(`turning to ${name.padEnd(24)} median ${median(turns[name]).toFixed(1)} ms, the cube built again from ${(await s.ex('lines')).toLocaleString('en')} lines`);
  // the lines behind the cell of the total row and the total column: all of them
  await s.page.mouse.click(...s.at(GRID_X + HEAD_W + COL_W / 2, GRID_Y + HEAD_H + ROW_H / 2));
  await s.page.keyboard.press('Enter');
  await s.page.waitForTimeout(400);
  console.log(`opening the lines behind the grand total (${(await s.ex('tabCount')) === 2 ? 'a tab of all of them' : 'no tab?'}): ${(await s.ex('updateMs')).toFixed(1)} ms for one pass over ${(await s.ex('lines')).toLocaleString('en')} lines`);
  // one cell
  await s.page.mouse.click(...s.at(16 + 40, 144 + 18));
  await s.page.waitForTimeout(200);
  await s.page.mouse.click(...s.at(GRID_X + HEAD_W + COL_W * 3 + COL_W / 2, GRID_Y + HEAD_H + ROW_H * 3 + ROW_H / 2));
  await s.page.keyboard.press('Enter');
  await s.page.waitForTimeout(300);
  console.log(`opening the lines behind one cell: ${(await s.ex('updateMs')).toFixed(1)} ms`);
  await s.close();
}

// 3: scrolling a table in which every group is open (83 rows, 35 columns), then the table of all million lines
async function scroll(emptyCalls, detail) {
  const s = await session({ emptyCalls });
  if (detail) {                                                    // the grand total's lines: a table of a million rows
    await s.page.mouse.click(...s.at(GRID_X + HEAD_W + COL_W / 2, GRID_Y + HEAD_H + ROW_H / 2));
    await s.page.keyboard.press('Enter');
  } else {
    await s.page.mouse.click(...s.at(s.box.width - 150, 98 + 17));    // Open all
  }
  await s.page.waitForTimeout(500);
  await s.page.evaluate(() => { window.__frames.length = 0; });
  await s.page.mouse.move(...s.at(600, 400));
  for (let i = 0; i < MOVES; i++) { await s.page.mouse.wheel(0, i % 40 < 20 ? 56 : -56); await s.page.waitForTimeout(12); }
  await s.page.waitForTimeout(200);
  const frames = (await s.page.evaluate(() => window.__frames)).filter((f) => f.draws > 0).map((f) => f.ms);
  if (frames.length === 0) console.log('   (no frame drew; page errors: ' + (s.errors.join(' | ') || 'none') + ')');
  await s.close();
  return frames;
}
for (const [label, detail] of [['the pivot table, every group open (83 rows, 35 columns)', false], ['the lines of the grand total (1 000 000 rows)', true]]) {
  const real = await scroll(false, detail);
  const empty = await scroll(true, detail);
  console.log(`scrolling ${label}`);
  console.log(`   Canvas 2D calls real : ${real.length} drawn frames, the callback takes ${median(real).toFixed(2)} ms (median), ${p95(real).toFixed(2)} ms (95th percentile)`);
  console.log(`   Canvas 2D calls empty: ${empty.length} drawn frames, the callback takes ${median(empty).toFixed(2)} ms (median): the module's own part (the scene, the cells, the crossing)`);
}
