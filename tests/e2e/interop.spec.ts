import { test, expect } from '@playwright/test';

test.describe('WASM Computation Demo', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demo/interop.html');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Bosatsu WASM/);
    await expect(page.locator('h1')).toContainText('WASM Computation');
  });

  test('WASM module loads', async ({ page }) => {
    // Wait for WASM to load
    await expect(page.locator('#status')).toContainText('ready', { timeout: 15000 });

    // Buttons should be enabled
    await expect(page.locator('#fib-btn')).toBeEnabled();
    await expect(page.locator('#fact-btn')).toBeEnabled();
  });

  test('fibonacci computation works', async ({ page }) => {
    // Wait for WASM to be ready
    await expect(page.locator('#status')).toContainText('ready', { timeout: 15000 });

    // Set input and compute
    await page.locator('#fib-n').fill('10');
    await page.locator('#fib-btn').click();

    // Check result - fib(10) = 55
    await expect(page.locator('#fib-result')).toContainText('55');

    // Check sequence is displayed
    await expect(page.locator('#fib-result .sequence')).toBeVisible();
  });

  test('factorial computation works', async ({ page }) => {
    // Wait for WASM to be ready
    await expect(page.locator('#status')).toContainText('ready', { timeout: 15000 });

    // Set input and compute
    await page.locator('#fact-n').fill('5');
    await page.locator('#fact-btn').click();

    // Check result - 5! = 120
    await expect(page.locator('#fact-result')).toContainText('120');
  });

  test('displays timing information', async ({ page }) => {
    // Wait for WASM to be ready
    await expect(page.locator('#status')).toContainText('ready', { timeout: 15000 });

    // Compute fibonacci
    await page.locator('#fib-n').fill('15');
    await page.locator('#fib-btn').click();

    // Check timing is displayed
    await expect(page.locator('#fib-result')).toContainText('ms');
    await expect(page.locator('#fib-result')).toContainText('Avg per call');
  });

  test('shows Bosatsu source code', async ({ page }) => {
    // Check the code block is visible with Bosatsu code
    const codeBlock = page.locator('.code-block');
    await expect(codeBlock).toContainText('def fib');
    await expect(codeBlock).toContainText('def factorial');
    await expect(codeBlock).toContainText('int_loop');
  });
});

test.describe('Landing Page', () => {
  test('has link to WASM demo', async ({ page }) => {
    await page.goto('/');
    const interopLink = page.locator('a[href="demo/interop.html"]');
    await expect(interopLink).toBeVisible();
  });
});
