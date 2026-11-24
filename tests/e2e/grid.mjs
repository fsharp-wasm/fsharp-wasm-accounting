// End-to-end: the accounting grid, compiled from F# to WasmGC (dist/AccountingGrid.wasm), runs in a real Chromium.  It draws everything on one canvas, so it is driven by
// pointer, wheel and key events at canvas coordinates; what it holds is read from the counts the module exports, and what it shows from the canvas.
//   bash build/build.sh && node tests/e2e/grid.mjs
import { fileURLToPath } from 'node:url';
import { serve, launch, checker } from '../../lib/runtime/tests/e2e/lib.mjs';

const { server, base } = await serve(fileURLToPath(new URL('../..', import.meta.url)));
const { check, failures } = checker();
const { browser, page, errors } = await launch();
const shot = process.env.GRID_SHOTS ?? '';

try {
  await page.setViewportSize({ width: 1400, height: 1500 });             // tall enough that the page does not scroll when the canvas takes the focus
  await page.goto(`${base}/web/`);
  await page.waitForFunction(() => /Loaded|✗|Error/.test(document.getElementById('status')?.textContent ?? ''), null, { timeout: 60000 }).catch(() => {});
  await page.waitForFunction(() => window.__module?.exports?.lines, null, { timeout: 60000 });
  const ex = (name) => page.evaluate((n) => window.__module.exports[n](), name);
  const counts = async () => ({ rows: await ex('rowsShown'), cols: await ex('colsShown'), tabs: await ex('tabCount'), active: await ex('activeTab'), r: await ex('selectedRow'), c: await ex('selectedCol') });
  await page.waitForTimeout(600);
  const box = await page.locator('#gfx').boundingBox();
  const at = (x, y) => [box.x + x, box.y + y];
  const look = () => page.evaluate(() => {                                          // a sample of the pixels, and how many colours there are
    const c = document.getElementById('gfx');
    const d = c.getContext('2d').getImageData(0, 0, c.width, c.height).data;
    const px = []; const colours = new Set();
    for (let i = 0; i < d.length; i += 4 * 17) { px.push(d[i], d[i + 1], d[i + 2]); colours.add((d[i] << 16) | (d[i + 1] << 8) | d[i + 2]); }
    return { px, colours: colours.size };
  });
  const differs = (a, b) => a.length !== b.length || a.some((v, i) => Math.abs(v - b[i]) > 3);
  const changed = async (name, act) => { const before = (await look()).px; await act(); await page.waitForTimeout(300); check(name, differs((await look()).px, before)); };
  const press = async (key) => { await page.keyboard.press(key); await page.waitForTimeout(80); };
  const GRID_X = 16, GRID_Y = 182, HEAD_W = 240, HEAD_H = 32, ROW_H = 28, COL_W = 136;       // the layout of the page (Layout.gridRect, Grid.metrics)
  const cell = (r, c) => at(GRID_X + HEAD_W + c * COL_W + COL_W / 2, GRID_Y + HEAD_H + r * ROW_H + ROW_H / 2);
  const rowHeader = (r, x = 14) => at(GRID_X + x, GRID_Y + HEAD_H + r * ROW_H + ROW_H / 2);
  const colHeader = (c, x = 12) => at(GRID_X + HEAD_W + c * COL_W + x, GRID_Y + HEAD_H / 2);

  check('the module holds a million journal lines', (await ex('lines')) === 1000000, String(await ex('lines')));
  console.log(`  (a million lines made in ${(await ex('generatedMs')).toFixed(1)} ms, the cube built in ${(await ex('pivotMs')).toFixed(1)} ms)`);
  const start = await counts();
  check('it starts with the total, the classes and the groups (18 rows) and the years and quarters (11 columns), one tab', start.rows === 18 && start.cols === 11 && start.tabs === 1 && start.r === -1, JSON.stringify(start));
  check('the page is painted', (await look()).colours > 12);
  if (shot) await page.screenshot({ path: `${shot}/grid-start.png` });

  // a click on a cell selects it; the same click again opens the lines behind it as a tab
  await page.mouse.click(...cell(1, 2));
  await page.waitForTimeout(200);
  let s = await counts();
  check('a click on a cell selects it', s.r === 1 && s.c === 2, JSON.stringify(s));
  await press('ArrowDown'); await press('ArrowRight');
  s = await counts();
  check('the arrow keys move the selection', s.r === 2 && s.c === 3, JSON.stringify(s));
  // the caret of a group row opens it (Product sales is row 2: Total, Revenue, Product sales ...)
  const before = (await counts()).rows;
  await page.mouse.click(...rowHeader(2, 30));
  await page.waitForTimeout(250);
  check('a click on the caret of a group opens it: its five accounts appear', (await counts()).rows === before + 5, JSON.stringify(await counts()));
  await page.mouse.click(...rowHeader(2, 30));
  await page.waitForTimeout(250);
  check('and the same click folds it again', (await counts()).rows === before);
  // a column: the caret of FY 2025 (column 1) folds its quarters
  const colsBefore = (await counts()).cols;
  await page.mouse.click(...colHeader(1, 14));
  await page.waitForTimeout(250);
  check('a click on the caret of a column folds it to its total (the four quarters go)', (await counts()).cols === colsBefore - 4, JSON.stringify(await counts()));
  await page.mouse.click(...colHeader(1, 14));
  await page.waitForTimeout(250);
  check('and opens it again', (await counts()).cols === colsBefore);
  // the keyboard: Space folds the row of the selection, Shift+Space the column
  await page.mouse.click(...cell(1, 2));
  await press('Space');
  check('Space folds the row of the selected cell', (await counts()).rows < before, JSON.stringify(await counts()));
  await press('Space');
  await page.keyboard.down('Shift'); await press('Space'); await page.keyboard.up('Shift');
  check('Shift and Space open the column of the selected cell (a quarter: its three months appear)', (await counts()).cols === colsBefore + 3, JSON.stringify(await counts()));
  await page.keyboard.down('Shift'); await press('Space'); await page.keyboard.up('Shift');
  // the lines behind a cell
  await page.mouse.click(...cell(1, 2));
  await press('Enter');
  s = await counts();
  check('Enter opens the lines behind the cell as a tab, and it is on show', s.tabs === 2 && s.active === 1, JSON.stringify(s));
  if (shot) await page.screenshot({ path: `${shot}/grid-tab.png` });
  await press('ArrowDown'); await press('ArrowDown');
  s = await counts();
  check('with a tab on show the keys move its table, not the pivot', s.r === 1 && s.c === 2, JSON.stringify(s));
  // the tab strip is at y = 144: the pivot tab first, then the lines; a click on the first one goes back
  await page.mouse.click(...at(16 + 40, 144 + 18));
  await page.waitForTimeout(200);
  s = await counts();
  check('a click on the Pivot tab shows the pivot table again', s.active === 0 && s.tabs === 2, JSON.stringify(s));
  // the buttons: the second pivot, a measure, Fold all
  await changed('the second pivot (cost centres by periods) is another table', async () => { await page.mouse.click(...at(330, 54 + 17)); });
  check('it is the second pivot, and it has the regions and their centres (1 + 3 + 7 rows)', (await ex('pivot')) === 1 && (await counts()).rows === 1 + 3 + 7, JSON.stringify(await counts()));
  await page.mouse.click(...at(120, 54 + 17));
  await page.waitForTimeout(250);
  check('the first pivot again', (await ex('pivot')) === 0 && (await counts()).tabs === 1);
  await changed('another measure (Budget) is another table', async () => { await page.mouse.click(...at(16 + 64 + 100, 98 + 17)); });
  check('the measure is Budget', (await ex('measure')) === 1, String(await ex('measure')));
  // wheel: scrolling moves the table
  await page.mouse.click(...at(box.width - 150, 98 + 17));        // "Open all" is the left one of the two buttons at the right edge
  await page.waitForTimeout(250);
  check('Open all opens every group: 83 rows, 35 columns', (await counts()).rows === 1 + 4 + 13 + 65 && (await counts()).cols === 1 + 2 + 8 + 24, JSON.stringify(await counts()));
  await page.mouse.move(...cell(3, 3));
  const top = await ex('offsetY');
  await page.mouse.wheel(0, 300);                                  // three notches down (a browser says 100 pixels to a notch)
  await page.waitForTimeout(300);
  const down = await ex('offsetY');
  check('the wheel down scrolls the table down, 36 px a notch', Math.abs((down - top) - 108) < 1, `${top} -> ${down}`);
  await page.mouse.wheel(0, -100);
  await page.waitForTimeout(300);
  check('the wheel up scrolls it back, a notch', Math.abs((await ex('offsetY')) - (down - 36)) < 1, `${down} -> ${await ex('offsetY')}`);
  await page.keyboard.down('Shift'); await page.mouse.wheel(0, 200); await page.keyboard.up('Shift');
  await page.waitForTimeout(300);
  check('Shift and the wheel scroll sideways', (await ex('offsetX')) > 0 && Math.abs((await ex('offsetY')) - (down - 36)) < 1, `x ${await ex('offsetX')}, y ${await ex('offsetY')}`);
  if (shot) await page.screenshot({ path: `${shot}/grid-scrolled.png` });
  check('no errors in the page console', errors.length === 0, errors.join(' | '));
} finally { await browser.close(); server.close(); }
const failed = failures();
console.log(failed ? `\n${failed} check(s) failed` : '');
process.exit(failed ? 1 : 0);
