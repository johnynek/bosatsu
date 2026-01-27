import { test, expect } from '@playwright/test';

test.describe('JS/WASM Interop Demo', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demo/interop.html');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Bosatsu JS\/WASM Interop/);
    await expect(page.locator('h1')).toContainText('JS/WASM Interop');
  });

  test('source code panels load', async ({ page }) => {
    // Wait for source code to load
    await expect(page.locator('#compute-source')).not.toContainText('Loading...');
    await expect(page.locator('#orchestrator-source')).not.toContainText('Loading...');

    // Check that the source contains expected code
    await expect(page.locator('#compute-source')).toContainText('def fib');
    await expect(page.locator('#orchestrator-source')).toContainText('from Demo/Compute import');
  });

  test('WASM module initializes', async ({ page }) => {
    // Wait for WASM to load (may take a moment)
    await expect(page.locator('#wasm-status')).toContainText('ready', { timeout: 10000 });
  });

  test('run button computes fib and factorial', async ({ page }) => {
    // Wait for WASM to be ready
    await expect(page.locator('#wasm-status')).toContainText('ready', { timeout: 10000 });

    // Set input value
    await page.locator('#input-n').fill('10');

    // Click run button
    await page.locator('#btn-run').click();

    // Check results - fib(10) = 55, factorial(10) = 3628800
    await expect(page.locator('#fib-js-result')).toContainText('55');
    await expect(page.locator('#fib-wasm-result')).toContainText('55');
    await expect(page.locator('#fact-js-result')).toContainText('3628800');
    await expect(page.locator('#fact-wasm-result')).toContainText('3628800');
  });

  test('JS and WASM results match', async ({ page }) => {
    // Wait for WASM to be ready
    await expect(page.locator('#wasm-status')).toContainText('ready', { timeout: 10000 });

    // Set input value
    await page.locator('#input-n').fill('15');

    // Click run button
    await page.locator('#btn-run').click();

    // Verify match status appears
    await expect(page.locator('#match-status')).toContainText('match', { timeout: 5000 });
  });

  test('benchmark runs successfully', async ({ page }) => {
    // Wait for WASM to be ready
    await expect(page.locator('#wasm-status')).toContainText('ready', { timeout: 10000 });

    // Set input value
    await page.locator('#input-n').fill('20');

    // Click benchmark button
    await page.locator('#btn-benchmark').click();

    // Check that benchmark results appear (contains "avg:")
    await expect(page.locator('#fib-js-time')).toContainText('avg:', { timeout: 15000 });
    await expect(page.locator('#fib-wasm-time')).toContainText('avg:');

    // Check that speedup is calculated
    await expect(page.locator('#match-status')).toContainText('speedup');
  });

  test('tabs switch correctly', async ({ page }) => {
    // Click C tab
    await page.locator('.tab[data-tab="c-tab"]').click();
    await expect(page.locator('#c-tab')).toBeVisible();
    await expect(page.locator('#js-tab')).not.toBeVisible();

    // Click WASM tab
    await page.locator('.tab[data-tab="wasm-tab"]').click();
    await expect(page.locator('#wasm-tab')).toBeVisible();
    await expect(page.locator('#c-tab')).not.toBeVisible();

    // Click back to JS tab
    await page.locator('.tab[data-tab="js-tab"]').click();
    await expect(page.locator('#js-tab')).toBeVisible();
  });
});

test.describe('Simple Multi-Target Demo', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demo/');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Bosatsu.*Demo/);
  });

  test('has link to interop demo', async ({ page }) => {
    const interopLink = page.locator('a[href="interop.html"]');
    await expect(interopLink).toBeVisible();
  });
});

test.describe('Landing Page', () => {
  test('has link to interop demo', async ({ page }) => {
    await page.goto('/');
    const interopLink = page.locator('a[href="demo/interop.html"]');
    await expect(interopLink).toBeVisible();
  });
});
