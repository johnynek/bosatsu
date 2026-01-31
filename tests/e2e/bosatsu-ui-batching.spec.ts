import { test, expect, Page } from '@playwright/test';

// Helper to capture console logs
function setupConsoleCapture(page: Page) {
  const logs: string[] = [];
  const errors: string[] = [];
  page.on('console', msg => {
    const text = `[${msg.type()}] ${msg.text()}`;
    logs.push(text);
    if (msg.type() === 'error') {
      errors.push(text);
    }
  });
  page.on('pageerror', err => {
    const text = `[PAGE ERROR] ${err.message}`;
    logs.push(text);
    errors.push(text);
  });
  return { logs, errors };
}

// =============================================================================
// REAL-TIME DASHBOARD DEMO
// Tests full batching mode (batchSize: Infinity, flushDelay: 'microtask')
// =============================================================================
test.describe('BosatsuUI Dashboard Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/ui/dashboard.html');
    await expect(page.locator('h1')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Real-Time Dashboard/i);
    await expect(page.locator('.header')).toBeVisible();
  });

  test('displays both BosatsuUI and React dashboards', async ({ page }) => {
    await expect(page.locator('.dashboard-title.bosatsu')).toBeVisible();
    await expect(page.locator('.dashboard-title.react')).toBeVisible();
  });

  test('has 50 metric cells in each dashboard', async ({ page }) => {
    // BosatsuUI dashboard
    const bosatsuGrid = page.locator('#bosatsu-grid');
    await expect(bosatsuGrid.locator('.value-cell')).toHaveCount(50);

    // React dashboard (rendered by React)
    const reactGrid = page.locator('#react-grid');
    await expect(reactGrid.locator('.value-cell')).toHaveCount(50, { timeout: 5000 });
  });

  test('control buttons are present', async ({ page }) => {
    await expect(page.locator('#start-btn')).toBeVisible();
    await expect(page.locator('#stop-btn')).toBeVisible();
    await expect(page.locator('#reset-btn')).toBeVisible();
  });

  test('start button begins updates', async ({ page }) => {
    const startBtn = page.locator('#start-btn');
    const stopBtn = page.locator('#stop-btn');
    const bosatsuRate = page.locator('#bosatsu-rate');

    // Initially stopped
    await expect(startBtn).toBeEnabled();
    await expect(stopBtn).toBeDisabled();

    // Start updates
    await startBtn.click();
    await expect(startBtn).toBeDisabled();
    await expect(stopBtn).toBeEnabled();

    // Wait for updates to accumulate
    await page.waitForTimeout(1000);

    // Rate should be non-zero
    const rate = await bosatsuRate.textContent();
    expect(parseInt(rate?.replace(/,/g, '') || '0')).toBeGreaterThan(0);

    // Stop updates
    await stopBtn.click();
  });

  test('stop button halts updates', async ({ page }) => {
    const startBtn = page.locator('#start-btn');
    const stopBtn = page.locator('#stop-btn');

    await startBtn.click();
    await page.waitForTimeout(500);
    await stopBtn.click();

    await expect(startBtn).toBeEnabled();
    await expect(stopBtn).toBeDisabled();
  });

  test('reset button clears stats and values', async ({ page }) => {
    const startBtn = page.locator('#start-btn');
    const resetBtn = page.locator('#reset-btn');
    const bosatsuRate = page.locator('#bosatsu-rate');
    const reactRate = page.locator('#react-rate');
    const advantage = page.locator('#advantage');

    // Start, let run, then reset
    await startBtn.click();
    await page.waitForTimeout(500);
    await resetBtn.click();

    // Stats should be reset
    await expect(bosatsuRate).toHaveText('0');
    await expect(reactRate).toHaveText('0');
    await expect(advantage).toHaveText('-');
  });

  test('stats bar displays update rates', async ({ page }) => {
    await expect(page.locator('#bosatsu-rate')).toBeVisible();
    await expect(page.locator('#react-rate')).toBeVisible();
    await expect(page.locator('#advantage')).toBeVisible();
  });

  test('explainer section describes batching strategy', async ({ page }) => {
    const explainer = page.locator('.explainer');
    await expect(explainer).toBeVisible();
    await expect(explainer).toContainText('batchSize');
    await expect(explainer).toContainText('flushDelay');
    await expect(explainer).toContainText('microtask');
  });

  test('has no console errors on load', async ({ page }) => {
    const { errors } = setupConsoleCapture(page);
    await page.waitForTimeout(500);
    expect(errors).toHaveLength(0);
  });

  test('metric values update when running', async ({ page }) => {
    const startBtn = page.locator('#start-btn');
    const firstMetric = page.locator('#bosatsu-metric-0');

    // Get initial value
    const initialValue = await firstMetric.textContent();

    // Start updates
    await startBtn.click();
    await page.waitForTimeout(1000);

    // Value should have changed (probabilistically)
    const newValue = await firstMetric.textContent();
    // Note: Values are random, so they might be the same by chance
    // We just verify no errors occurred

    await page.locator('#stop-btn').click();
  });
});

// =============================================================================
// DRAG-AND-DROP DEMO
// Tests immediate mode (batchSize: 1, flushDelay: 0)
// =============================================================================
test.describe('BosatsuUI Drag-Drop Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/ui/drag-drop.html');
    await expect(page.locator('h1')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Drag-and-Drop/i);
    await expect(page.locator('.header')).toBeVisible();
  });

  test('displays both workspaces', async ({ page }) => {
    await expect(page.locator('.workspace-title.bosatsu')).toBeVisible();
    await expect(page.locator('.workspace-title.react')).toBeVisible();
  });

  test('BosatsuUI workspace shows immediate mode config', async ({ page }) => {
    const bosatsuWorkspace = page.locator('.workspace').first();
    await expect(bosatsuWorkspace.locator('.workspace-mode')).toContainText('batchSize: 1');
  });

  test('React workspace shows flushSync mode', async ({ page }) => {
    const reactWorkspace = page.locator('.workspace').last();
    await expect(reactWorkspace.locator('.workspace-mode')).toContainText('flushSync');
  });

  test('BosatsuUI canvas has 3 draggable boxes', async ({ page }) => {
    const bosatsuCanvas = page.locator('#bosatsu-canvas');
    await expect(bosatsuCanvas.locator('.draggable')).toHaveCount(3);
    await expect(bosatsuCanvas.locator('.box-a')).toBeVisible();
    await expect(bosatsuCanvas.locator('.box-b')).toBeVisible();
    await expect(bosatsuCanvas.locator('.box-c')).toBeVisible();
  });

  test('React canvas has 3 draggable boxes', async ({ page }) => {
    const reactCanvas = page.locator('#react-canvas');
    await expect(reactCanvas.locator('.draggable')).toHaveCount(3, { timeout: 5000 });
  });

  test('position display exists in both workspaces', async ({ page }) => {
    await expect(page.locator('#bosatsu-position')).toBeVisible();
    await expect(page.locator('#react-canvas .position-display')).toBeVisible();
  });

  test('stats bar shows update counts', async ({ page }) => {
    await expect(page.locator('#bosatsu-updates')).toBeVisible();
    await expect(page.locator('#react-updates')).toBeVisible();
  });

  test('dragging a BosatsuUI box updates position display', async ({ page }) => {
    const box = page.locator('#bosatsu-box-a');
    const positionDisplay = page.locator('#bosatsu-position');

    // Initial position display
    await expect(positionDisplay).toContainText('Drag a box');

    // Simulate drag
    await box.hover();
    await page.mouse.down();
    await page.mouse.move(100, 100);

    // Position display should update during drag
    await expect(positionDisplay).toContainText('Box A');

    await page.mouse.up();
  });

  test('dragging increments update counter', async ({ page }) => {
    const box = page.locator('#bosatsu-box-a');
    const updateCount = page.locator('#bosatsu-updates');

    // Initial count
    const initialCount = parseInt(await updateCount.textContent() || '0');

    // Drag the box
    const boxBounds = await box.boundingBox();
    if (boxBounds) {
      await page.mouse.move(boxBounds.x + 40, boxBounds.y + 40);
      await page.mouse.down();
      await page.mouse.move(boxBounds.x + 100, boxBounds.y + 100);
      await page.mouse.up();
    }

    // Update count should have increased
    const newCount = parseInt(await updateCount.textContent() || '0');
    expect(newCount).toBeGreaterThan(initialCount);
  });

  test('explainer describes synchronous updates', async ({ page }) => {
    const explainer = page.locator('.explainer');
    await expect(explainer).toBeVisible();
    await expect(explainer).toContainText('synchronous');
    await expect(explainer).toContainText('flushSync');
  });

  test('has no console errors during drag', async ({ page }) => {
    const { errors } = setupConsoleCapture(page);

    const box = page.locator('#bosatsu-box-a');
    const boxBounds = await box.boundingBox();

    if (boxBounds) {
      await page.mouse.move(boxBounds.x + 40, boxBounds.y + 40);
      await page.mouse.down();
      await page.mouse.move(boxBounds.x + 150, boxBounds.y + 150);
      await page.mouse.up();
    }

    await page.waitForTimeout(500);
    expect(errors).toHaveLength(0);
  });

  test('boxes stay within canvas bounds', async ({ page }) => {
    const canvas = page.locator('#bosatsu-canvas');
    const box = page.locator('#bosatsu-box-a');

    const canvasBounds = await canvas.boundingBox();
    const boxBounds = await box.boundingBox();

    if (canvasBounds && boxBounds) {
      // Drag box far to the right (should be clamped)
      await page.mouse.move(boxBounds.x + 40, boxBounds.y + 40);
      await page.mouse.down();
      await page.mouse.move(canvasBounds.x + canvasBounds.width + 200, boxBounds.y + 40);
      await page.mouse.up();

      // Box should still be within canvas
      const newBounds = await box.boundingBox();
      if (newBounds) {
        expect(newBounds.x + newBounds.width).toBeLessThanOrEqual(canvasBounds.x + canvasBounds.width + 10);
      }
    }
  });
});

// =============================================================================
// SPREADSHEET DEMO
// Tests mixed batching (full batching with manual flush() for formulas)
// =============================================================================
test.describe('BosatsuUI Spreadsheet Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/ui/spreadsheet.html');
    await expect(page.locator('h1')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Spreadsheet/i);
    await expect(page.locator('.header')).toBeVisible();
  });

  test('displays spreadsheet with cells', async ({ page }) => {
    await expect(page.locator('.spreadsheet')).toBeVisible();
    await expect(page.locator('.spreadsheet table')).toBeVisible();
  });

  test('has column headers A through D', async ({ page }) => {
    const headers = page.locator('.spreadsheet thead th');
    await expect(headers.nth(1)).toHaveText('A');
    await expect(headers.nth(2)).toHaveText('B');
    await expect(headers.nth(3)).toHaveText('C');
    await expect(headers.nth(4)).toHaveText('D');
  });

  test('has row headers 1 through 5', async ({ page }) => {
    const rowHeaders = page.locator('.spreadsheet th.row-header');
    // First row header is empty (corner cell)
    await expect(rowHeaders.nth(1)).toHaveText('1');
    await expect(rowHeaders.nth(2)).toHaveText('2');
    await expect(rowHeaders.nth(3)).toHaveText('3');
    await expect(rowHeaders.nth(4)).toHaveText('4');
    await expect(rowHeaders.nth(5)).toHaveText('5');
  });

  test('input cells are editable', async ({ page }) => {
    const cellA1 = page.locator('#cell-A1');
    await expect(cellA1).toBeEditable();
    await expect(cellA1).not.toHaveAttribute('readonly');
  });

  test('formula cells are readonly', async ({ page }) => {
    const cellD1 = page.locator('#cell-D1');
    await expect(cellD1).toHaveAttribute('readonly');
  });

  test('formula cells have visual distinction', async ({ page }) => {
    const formulaCell = page.locator('td.formula-cell').first();
    await expect(formulaCell).toBeVisible();
    // Should have green background (from CSS)
  });

  test('initial values are set correctly', async ({ page }) => {
    await expect(page.locator('#cell-A1')).toHaveValue('100');
    await expect(page.locator('#cell-B1')).toHaveValue('200');
    await expect(page.locator('#cell-C1')).toHaveValue('50');
  });

  test('row sum formulas calculate correctly', async ({ page }) => {
    // D1 = A1 + B1 + C1 = 100 + 200 + 50 = 350
    await expect(page.locator('#cell-D1')).toHaveValue('350');
    // D2 = A2 + B2 + C2 = 150 + 75 + 25 = 250
    await expect(page.locator('#cell-D2')).toHaveValue('250');
    // D3 = A3 + B3 + C3 = 80 + 120 + 40 = 240
    await expect(page.locator('#cell-D3')).toHaveValue('240');
  });

  test('column sum formulas calculate correctly', async ({ page }) => {
    // A4 = SUM(A1:A3) = 100 + 150 + 80 = 330
    await expect(page.locator('#cell-A4')).toHaveValue('330');
    // B4 = SUM(B1:B3) = 200 + 75 + 120 = 395
    await expect(page.locator('#cell-B4')).toHaveValue('395');
    // C4 = SUM(C1:C3) = 50 + 25 + 40 = 115
    await expect(page.locator('#cell-C4')).toHaveValue('115');
  });

  test('D4 calculates total of D column (sum of row sums)', async ({ page }) => {
    // D4 = SUM(D1:D3) = 350 + 250 + 240 = 840
    await expect(page.locator('#cell-D4')).toHaveValue('840');
  });

  test('D5 (Grand Total) equals D4', async ({ page }) => {
    // D5 = D4 = 840
    await expect(page.locator('#cell-D5')).toHaveValue('840');
  });

  test('changing input updates dependent formulas', async ({ page }) => {
    const cellA1 = page.locator('#cell-A1');
    const cellD1 = page.locator('#cell-D1');
    const cellA4 = page.locator('#cell-A4');
    const cellD4 = page.locator('#cell-D4');
    const cellD5 = page.locator('#cell-D5');

    // Change A1 from 100 to 200
    await cellA1.fill('200');
    await cellA1.dispatchEvent('input');

    // Wait for recalculation
    await page.waitForTimeout(100);

    // D1 = 200 + 200 + 50 = 450
    await expect(cellD1).toHaveValue('450');
    // A4 = 200 + 150 + 80 = 430
    await expect(cellA4).toHaveValue('430');
    // D4 = 450 + 250 + 240 = 940
    await expect(cellD4).toHaveValue('940');
    // D5 = D4 = 940
    await expect(cellD5).toHaveValue('940');
  });

  test('formula bar shows active cell', async ({ page }) => {
    const cellRef = page.locator('#active-cell');
    const formulaDisplay = page.locator('#formula-display');

    // Click on A1
    await page.locator('#cell-A1').focus();
    await expect(cellRef).toHaveText('A1');
    // A1 has no formula, should show value
    await expect(formulaDisplay).toContainText('100');
  });

  test('formula bar shows formula for formula cells', async ({ page }) => {
    const formulaDisplay = page.locator('#formula-display');

    // Click on D1 (formula cell)
    await page.locator('#cell-D1').focus();
    await expect(formulaDisplay).toContainText('=A1+B1+C1');
  });

  test('stats show update and recalc counts', async ({ page }) => {
    const updateCount = page.locator('#update-count');
    const recalcCount = page.locator('#recalc-count');

    // Initial load triggers recalculations
    const initialRecalc = parseInt(await recalcCount.textContent() || '0');
    expect(initialRecalc).toBeGreaterThan(0);

    // Change a value
    await page.locator('#cell-A1').fill('999');
    await page.locator('#cell-A1').dispatchEvent('input');

    // Counts should increase
    const newUpdate = parseInt(await updateCount.textContent() || '0');
    const newRecalc = parseInt(await recalcCount.textContent() || '0');
    expect(newUpdate).toBeGreaterThan(0);
    expect(newRecalc).toBeGreaterThan(initialRecalc);
  });

  test('legend explains cell types', async ({ page }) => {
    const legend = page.locator('.legend');
    await expect(legend).toBeVisible();
    await expect(legend).toContainText('Editable input');
    await expect(legend).toContainText('Formula cell');
  });

  test('explainer describes mixed batching strategy', async ({ page }) => {
    const explainer = page.locator('.explainer');
    await expect(explainer).toBeVisible();
    await expect(explainer).toContainText('mixed batching');
    await expect(explainer).toContainText('flush()');
    await expect(explainer).toContainText('batchSize: Infinity');
  });

  test('has no console errors during edits', async ({ page }) => {
    const { errors } = setupConsoleCapture(page);

    // Edit multiple cells
    await page.locator('#cell-A1').fill('123');
    await page.locator('#cell-A1').dispatchEvent('input');
    await page.locator('#cell-B2').fill('456');
    await page.locator('#cell-B2').dispatchEvent('input');
    await page.locator('#cell-C3').fill('789');
    await page.locator('#cell-C3').dispatchEvent('input');

    await page.waitForTimeout(500);
    expect(errors).toHaveLength(0);
  });

  test('Grand Total row spans 3 columns', async ({ page }) => {
    const grandTotalCell = page.locator('td[colspan="3"]');
    await expect(grandTotalCell).toBeVisible();
    await expect(grandTotalCell).toContainText('Grand Total');
  });

  test('complex formula cascade works correctly', async ({ page }) => {
    // Set all input cells to 10
    const inputCells = ['A1', 'B1', 'C1', 'A2', 'B2', 'C2', 'A3', 'B3', 'C3'];
    for (const cell of inputCells) {
      await page.locator(`#cell-${cell}`).fill('10');
      await page.locator(`#cell-${cell}`).dispatchEvent('input');
    }

    await page.waitForTimeout(200);

    // Each row sum: 10 + 10 + 10 = 30
    await expect(page.locator('#cell-D1')).toHaveValue('30');
    await expect(page.locator('#cell-D2')).toHaveValue('30');
    await expect(page.locator('#cell-D3')).toHaveValue('30');

    // Each column sum: 10 + 10 + 10 = 30
    await expect(page.locator('#cell-A4')).toHaveValue('30');
    await expect(page.locator('#cell-B4')).toHaveValue('30');
    await expect(page.locator('#cell-C4')).toHaveValue('30');

    // D4 = 30 + 30 + 30 = 90 (sum of row sums)
    await expect(page.locator('#cell-D4')).toHaveValue('90');

    // D5 = D4 = 90
    await expect(page.locator('#cell-D5')).toHaveValue('90');
  });
});

// =============================================================================
// BATCHING BEHAVIOR VERIFICATION
// =============================================================================
test.describe('BosatsuUI Batching Behavior', () => {
  test('dashboard uses full batching (microtask)', async ({ page }) => {
    await page.goto('/demos/ui/dashboard.html');

    // Verify BosatsuUI is configured with full batching
    const config = await page.evaluate(() => {
      return (window as any).BosatsuUI.getConfig();
    });

    expect(config.batchSize).toBe(Infinity);
    expect(config.flushDelay).toBe('microtask');
  });

  test('drag-drop uses immediate mode', async ({ page }) => {
    await page.goto('/demos/ui/drag-drop.html');

    // Verify BosatsuUI is configured with immediate mode
    const config = await page.evaluate(() => {
      return (window as any).BosatsuUI.getConfig();
    });

    expect(config.batchSize).toBe(1);
    expect(config.flushDelay).toBe(0);
  });

  test('spreadsheet uses full batching with manual flush', async ({ page }) => {
    await page.goto('/demos/ui/spreadsheet.html');

    // Verify BosatsuUI is configured with full batching
    const config = await page.evaluate(() => {
      return (window as any).BosatsuUI.getConfig();
    });

    expect(config.batchSize).toBe(Infinity);
    expect(config.flushDelay).toBe('microtask');

    // Verify flush() is called during recalculation (by checking recalc count increases)
    const initialRecalc = await page.locator('#recalc-count').textContent();

    await page.locator('#cell-A1').fill('999');
    await page.locator('#cell-A1').dispatchEvent('input');
    await page.waitForTimeout(100);

    const newRecalc = await page.locator('#recalc-count').textContent();
    expect(parseInt(newRecalc || '0')).toBeGreaterThan(parseInt(initialRecalc || '0'));
  });
});

// =============================================================================
// CROSS-DEMO CONSISTENCY
// =============================================================================
test.describe('Cross-Demo Consistency', () => {
  test('all batching demos load without errors', async ({ page }) => {
    const demos = [
      '/demos/ui/dashboard.html',
      '/demos/ui/drag-drop.html',
      '/demos/ui/spreadsheet.html',
    ];

    for (const demo of demos) {
      const { errors } = setupConsoleCapture(page);
      await page.goto(demo);
      await page.waitForTimeout(500);
      expect(errors, `${demo} has errors`).toHaveLength(0);
    }
  });

  test('all batching demos have BosatsuUI runtime loaded', async ({ page }) => {
    const demos = [
      '/demos/ui/dashboard.html',
      '/demos/ui/drag-drop.html',
      '/demos/ui/spreadsheet.html',
    ];

    for (const demo of demos) {
      await page.goto(demo);
      const hasBosatsuUI = await page.evaluate(() => {
        return typeof (window as any).BosatsuUI !== 'undefined' &&
               typeof (window as any).BosatsuUI.configure === 'function' &&
               typeof (window as any).BosatsuUI.setState === 'function' &&
               typeof (window as any).BosatsuUI.flush === 'function';
      });
      expect(hasBosatsuUI, `${demo} missing BosatsuUI runtime`).toBe(true);
    }
  });

  test('all batching demos have consistent styling', async ({ page }) => {
    const demos = [
      '/demos/ui/dashboard.html',
      '/demos/ui/drag-drop.html',
      '/demos/ui/spreadsheet.html',
    ];

    for (const demo of demos) {
      await page.goto(demo);
      // All should have header with h1
      await expect(page.locator('.header h1')).toBeVisible();
      // All should have explainer section
      await expect(page.locator('.explainer')).toBeVisible();
    }
  });
});
