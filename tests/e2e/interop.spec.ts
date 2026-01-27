import { test, expect } from '@playwright/test';

test.describe('JS/WASM Interop Demo', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demo/interop.html');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Bosatsu Multi-Target/);
    await expect(page.locator('h1')).toContainText('Multi-Target');
  });

  test('all modules load', async ({ page }) => {
    // Wait for all modules to load (indicated by .ready class on status dots)
    await expect(page.locator('#js-status')).toHaveClass(/ready/, { timeout: 15000 });
    await expect(page.locator('#wasm-status')).toHaveClass(/ready/, { timeout: 15000 });
    await expect(page.locator('#bridge-status')).toHaveClass(/ready/, { timeout: 15000 });

    // Buttons should be enabled
    await expect(page.locator('#fib-btn')).toBeEnabled();
    await expect(page.locator('#fact-btn')).toBeEnabled();
  });

  test('fibonacci computation works in WASM mode', async ({ page }) => {
    // Wait for modules to load
    await expect(page.locator('#bridge-status')).toHaveClass(/ready/, { timeout: 15000 });

    // Should be in WASM mode by default (first mode button is active)
    await expect(page.locator('.mode-btn[data-target="fib"][data-mode="wasm"]')).toHaveClass(/active/);

    // Set input and compute
    await page.locator('#fib-n').fill('10');
    await page.locator('#fib-btn').click();

    // Check result - fib(10) = 55
    await expect(page.locator('#fib-result')).toContainText('55');
    // Check execution badge shows WASM
    await expect(page.locator('#fib-result .execution-badge')).toContainText('WASM');
  });

  test('factorial computation works', async ({ page }) => {
    // Wait for modules to load
    await expect(page.locator('#bridge-status')).toHaveClass(/ready/, { timeout: 15000 });

    // Set input and compute
    await page.locator('#fact-n').fill('5');
    await page.locator('#fact-btn').click();

    // Check result - 5! = 120
    await expect(page.locator('#fact-result')).toContainText('120');
  });

  test('mode toggle switches between JS and WASM', async ({ page }) => {
    // Wait for modules to load
    await expect(page.locator('#bridge-status')).toHaveClass(/ready/, { timeout: 15000 });

    // Start in WASM mode (default)
    await expect(page.locator('.mode-btn[data-target="fib"][data-mode="wasm"]')).toHaveClass(/active/);

    // Click Pure JS button for fib
    await page.locator('.mode-btn[data-target="fib"][data-mode="js"]').click();
    await expect(page.locator('.mode-btn[data-target="fib"][data-mode="js"]')).toHaveClass(/active/);

    // Compute should still work
    await page.locator('#fib-n').fill('10');
    await page.locator('#fib-btn').click();
    await expect(page.locator('#fib-result')).toContainText('55');
    // Check execution badge shows Pure JS
    await expect(page.locator('#fib-result .execution-badge')).toContainText('Pure JS');

    // Switch back to WASM
    await page.locator('.mode-btn[data-target="fib"][data-mode="wasm"]').click();
    await expect(page.locator('.mode-btn[data-target="fib"][data-mode="wasm"]')).toHaveClass(/active/);
  });

  test('validates input correctly', async ({ page }) => {
    // Wait for modules to load
    await expect(page.locator('#bridge-status')).toHaveClass(/ready/, { timeout: 15000 });

    // Try invalid fib input (too large)
    await page.locator('#fib-n').fill('50');
    // Validation message should appear
    await expect(page.locator('#fib-validation')).toContainText('Must be 0-40');

    // Try valid fib input
    await page.locator('#fib-n').fill('10');
    await expect(page.locator('#fib-validation')).toBeEmpty();
    await page.locator('#fib-btn').click();
    await expect(page.locator('#fib-result')).toContainText('55');
  });

  test('shows Bosatsu source code', async ({ page }) => {
    // Check the code blocks are visible with Bosatsu code
    const codeBlocks = page.locator('.code-block');
    await expect(codeBlocks.first()).toContainText('def');
    await expect(codeBlocks.first()).toContainText('validate_fib_input');
  });

  test('displays timing information', async ({ page }) => {
    // Wait for modules to load
    await expect(page.locator('#bridge-status')).toHaveClass(/ready/, { timeout: 15000 });

    // Compute fibonacci
    await page.locator('#fib-n').fill('15');
    await page.locator('#fib-btn').click();

    // Check timing is displayed
    await expect(page.locator('#fib-result .result-timing')).toContainText('ms');
  });

  test('displays fibonacci sequence', async ({ page }) => {
    // Wait for modules to load
    await expect(page.locator('#bridge-status')).toHaveClass(/ready/, { timeout: 15000 });

    // Compute fibonacci
    await page.locator('#fib-n').fill('10');
    await page.locator('#fib-btn').click();

    // Check sequence is displayed
    await expect(page.locator('#fib-result .sequence')).toBeVisible();
    // First items should be 0, 1, 1, 2, 3, 5...
    await expect(page.locator('#fib-result .seq-item').first()).toContainText('0');
  });
});

test.describe('Landing Page', () => {
  test('has link to interop demo', async ({ page }) => {
    await page.goto('/');
    const interopLink = page.locator('a[href="demo/interop.html"]');
    await expect(interopLink).toBeVisible();
  });
});
