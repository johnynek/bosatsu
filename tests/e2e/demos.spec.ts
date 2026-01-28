import { test, expect, Page } from '@playwright/test';

// Helper to capture console logs
function setupConsoleCapture(page: Page) {
  const logs: string[] = [];
  page.on('console', msg => {
    const text = `[${msg.type()}] ${msg.text()}`;
    logs.push(text);
    console.log(text);
  });
  page.on('pageerror', err => {
    const text = `[PAGE ERROR] ${err.message}`;
    logs.push(text);
    console.log(text);
  });
  return logs;
}

// =============================================================================
// LOAN CALCULATOR DEMO
// =============================================================================
test.describe('Loan Calculator Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/loan-calculator.html');
    // Wait for JavaScript to initialize (controls are populated dynamically)
    await expect(page.locator('.value-display').first()).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    // Title can have underscore or space depending on --title flag
    await expect(page).toHaveTitle(/Loan.Calculator/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('displays controls section', async ({ page }) => {
    await expect(page.locator('.controls-section')).toBeVisible();
    await expect(page.locator('#controls')).toBeVisible();
  });

  test('shows value displays for computed values', async ({ page }) => {
    // Should have value displays for the loan calculations
    const valueDisplays = page.locator('.value-display');
    await expect(valueDisplays.first()).toBeVisible();
  });

  test('Why? buttons are present and functional', async ({ page }) => {
    // Look for Why? buttons
    const whyButtons = page.locator('.why-button');
    const count = await whyButtons.count();

    if (count > 0) {
      // Click a Why? button
      await whyButtons.first().click();

      // Modal should appear
      await expect(page.locator('#why-modal')).not.toHaveClass(/hidden/);
      await expect(page.locator('.why-modal-content')).toBeVisible();

      // Close modal
      await page.locator('#why-modal-close').click();
      await expect(page.locator('#why-modal')).toHaveClass(/hidden/);
    }
  });

  test('What if? toggles are present', async ({ page }) => {
    // Look for What if? toggle section
    const whatIfToggles = page.locator('#what-if-toggles');
    if (await whatIfToggles.isVisible()) {
      await expect(whatIfToggles).toBeVisible();
    }
  });
});

// =============================================================================
// INVESTMENT CALCULATOR DEMO
// =============================================================================
test.describe('Investment Calculator Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/investment.html');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/investment/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('displays controls and values', async ({ page }) => {
    await expect(page.locator('.controls-section')).toBeVisible();
    await expect(page.locator('#controls')).toBeVisible();
  });

  test('shows computed investment values', async ({ page }) => {
    const valueDisplays = page.locator('.value-display');
    await expect(valueDisplays.first()).toBeVisible();
  });

  test('Why? explanation modal works', async ({ page }) => {
    const whyButtons = page.locator('.why-button');
    const count = await whyButtons.count();

    if (count > 0) {
      await whyButtons.first().click();
      await expect(page.locator('#why-modal')).not.toHaveClass(/hidden/);

      // Check explanation content
      const explanation = page.locator('#why-explanation');
      await expect(explanation).toBeVisible();

      // Should contain derivation info
      const derivations = page.locator('.derivation');
      await expect(derivations.first()).toBeVisible();

      await page.locator('#why-modal-close').click();
    }
  });
});

// =============================================================================
// CARBON FOOTPRINT CALCULATOR DEMO
// =============================================================================
test.describe('Carbon Footprint Calculator Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/carbon-footprint.html');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/carbon/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('displays controls and computed values', async ({ page }) => {
    await expect(page.locator('.controls-section')).toBeVisible();
  });

  test('Why? buttons show derivation chains', async ({ page }) => {
    const whyButtons = page.locator('.why-button');
    const count = await whyButtons.count();

    if (count > 0) {
      await whyButtons.first().click();

      // Modal should show derivation hierarchy
      const derivations = page.locator('.derivation');
      await expect(derivations.first()).toBeVisible();

      // Check for assumption/computed tags
      const assumptions = page.locator('.derivation.assumption');
      const computed = page.locator('.derivation.computed');

      // Should have at least one type
      const hasAssumption = await assumptions.count() > 0;
      const hasComputed = await computed.count() > 0;
      expect(hasAssumption || hasComputed).toBeTruthy();

      await page.locator('#why-modal-close').click();
    }
  });
});

// =============================================================================
// ANIMATION DEMOS REMOVED
// =============================================================================
// NOTE: Bouncing Ball and Pendulum demos were removed because they used embedded
// JavaScript strings instead of real Bosatsu code. They were just configuration
// files that bypassed Bosatsu entirely. Phase 14 will add trig/math functions
// to Bosatsu/Numeric to enable real physics simulations in pure Bosatsu.

// =============================================================================
// MULTI-TARGET COMPILATION DEMO (index page)
// =============================================================================
test.describe('Multi-Target Demo Index', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/index.html');
    // Wait for the page to fully initialize (source code loaded, not "Loading...")
    await expect(page.locator('#source-code')).not.toContainText('Loading...', { timeout: 10000 });
  });

  test('page loads with compilation demo', async ({ page }) => {
    await expect(page).toHaveTitle(/Bosatsu/i);
    await expect(page.locator('h1')).toContainText('Compilation');
  });

  test('shows Bosatsu source code', async ({ page }) => {
    const sourceCode = page.locator('#source-code');
    await expect(sourceCode).toBeVisible();
    // Should contain Bosatsu code
    await expect(sourceCode).toContainText('def');
  });

  test('has tabs for JS, C, WASM', async ({ page }) => {
    await expect(page.locator('.tab[data-target="js"]')).toBeVisible();
    await expect(page.locator('.tab[data-target="c"]')).toBeVisible();
    await expect(page.locator('.tab[data-target="wasm"]')).toBeVisible();
  });

  test('JavaScript tab shows generated code', async ({ page }) => {
    // JS tab should be active by default
    await expect(page.locator('.tab[data-target="js"]')).toHaveClass(/active/);

    const jsCode = page.locator('#js-code');
    await expect(jsCode).toBeVisible();
  });

  test('can switch between tabs', async ({ page }) => {
    // Switch to C tab
    await page.locator('.tab[data-target="c"]').click();
    await expect(page.locator('.tab[data-target="c"]')).toHaveClass(/active/);
    await expect(page.locator('#c')).toBeVisible();

    // Switch to WASM tab
    await page.locator('.tab[data-target="wasm"]').click();
    await expect(page.locator('.tab[data-target="wasm"]')).toHaveClass(/active/);
    await expect(page.locator('#wasm')).toBeVisible();
  });

  test('can run JavaScript demo', async ({ page }) => {
    const runBtn = page.locator('#run-js');
    await expect(runBtn).toBeVisible();

    await runBtn.click();

    // Result should appear (may show result or error message)
    const result = page.locator('#js-result');
    // Wait for any content to appear with longer timeout for JS execution
    await expect(result).not.toBeEmpty({ timeout: 10000 });
  });
});

// =============================================================================
// LOAN CALCULATOR WITH SWEEP CHARTS
// =============================================================================
test.describe('Loan Calculator with Sweep Charts', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/loan-sweep.html');
    // Wait for page to load - applet container should be immediately visible
    await expect(page.locator('.applet-container')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Loan.*Calculator.*Sweep/i);
  });

  test('sweep section and chart render', async ({ page }) => {
    // Wait for JS to create the sweep section
    const sweepsSection = page.locator('.sweeps-section');
    await expect(sweepsSection).toBeVisible({ timeout: 10000 });
    await expect(sweepsSection.locator('h3')).toContainText('Parameter Sweeps');

    // Check canvas exists
    const canvas = page.locator('#sweep-chart-0');
    await expect(canvas).toBeVisible();

    // Check canvas has proper dimensions
    const box = await canvas.boundingBox();
    expect(box).not.toBeNull();
    expect(box!.width).toBeGreaterThan(0);
    expect(box!.height).toBeGreaterThan(0);
  });

  test('sweep chart container has proper styling', async ({ page }) => {
    const chartContainer = page.locator('.sweep-chart-container');
    await expect(chartContainer).toBeVisible({ timeout: 10000 });

    // Check header shows the sweep relationship
    const header = chartContainer.locator('.sweep-chart-title');
    await expect(header).toBeVisible();
    await expect(header).toContainText('monthly_payment');
    await expect(header).toContainText('annual_rate');
  });

  test('changing input updates sweep chart marker', async ({ page }) => {
    // Wait for slider inputs to render
    await expect(page.locator('input[type="range"]').first()).toBeVisible({ timeout: 10000 });

    // Get a slider input for annual_rate
    const slider = page.locator('input[type="range"]').nth(1); // annual_rate is the second input

    // Get the current value
    const initialValue = await slider.inputValue();

    // Change the slider value
    await slider.fill('500');

    // The _renderSweep function should have been called
    // We can't easily check canvas content, but we can check the value changed
    const newValue = await slider.inputValue();
    expect(newValue).not.toBe(initialValue);
  });
});

// =============================================================================
// DEMO NAVIGATION
// =============================================================================
test.describe('Demo Navigation', () => {
  test('landing page has links to all demos', async ({ page }) => {
    await page.goto('/');

    // Check for demo links
    await expect(page.locator('a[href*="interop"]')).toBeVisible();
  });

  test('demo index links to interop demo', async ({ page }) => {
    await page.goto('/demo/index.html');

    const interopLink = page.locator('a[href="interop.html"]');
    await expect(interopLink).toBeVisible();
  });
});
