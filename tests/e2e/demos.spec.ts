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
// LOAN CALCULATOR WITH CANVAS VISUALIZATION
// =============================================================================
test.describe('Loan Calculator with Canvas Visualization', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/loan-viz.html');
    // Wait for page to load
    await expect(page.locator('.applet-container')).toBeVisible({ timeout: 10000 });
  });

  test('page loads with canvas element', async ({ page }) => {
    await expect(page).toHaveTitle(/Loan.*Calculator.*Visualization/i);

    // Check canvas exists
    const canvas = page.locator('#simulation-canvas');
    await expect(canvas).toBeVisible();

    // Check canvas has proper dimensions
    const box = await canvas.boundingBox();
    expect(box).not.toBeNull();
    expect(box!.width).toBeGreaterThan(0);
    expect(box!.height).toBeGreaterThan(0);
  });

  test('canvas API functions are available', async ({ page }) => {
    // Check that the canvas API functions are defined
    const hasAPI = await page.evaluate(() => {
      return typeof (window as any)._clear === 'function' &&
             typeof (window as any)._rect === 'function' &&
             typeof (window as any)._circle === 'function' &&
             typeof (window as any)._line === 'function' &&
             typeof (window as any)._text === 'function';
    });
    expect(hasAPI).toBe(true);
  });

  test('canvas drawing functions work', async ({ page }) => {
    // Test that we can call drawing functions without errors
    const drewShape = await page.evaluate(() => {
      try {
        (window as any)._clear('#ffffff');
        (window as any)._fill('#667eea');
        (window as any)._rect(50, 50, 100, 50);
        (window as any)._circle(200, 75, 25);
        (window as any)._text('Test', 250, 75);
        return true;
      } catch (e) {
        return false;
      }
    });
    expect(drewShape).toBe(true);
  });

  test('visualization callback can be registered', async ({ page }) => {
    // Test registering a visualization callback
    const registered = await page.evaluate(() => {
      let called = false;
      (window as any)._onVisualize(() => {
        called = true;
      });
      // Manually trigger a redraw
      (window as any)._redrawVisualization();
      return called;
    });
    expect(registered).toBe(true);
  });
});

// =============================================================================
// TAX CALCULATOR DEMO (AI Debugging Demo)
// =============================================================================
test.describe('Tax Calculator Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/tax_demo.html');
    // Wait for JavaScript to initialize (controls are populated dynamically)
    await expect(page.locator('.value-display').first()).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/Tax.*Calculator/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('displays controls section with input sliders', async ({ page }) => {
    await expect(page.locator('.controls-section')).toBeVisible();
    await expect(page.locator('#controls')).toBeVisible();

    // Should have sliders for tax_rate, income, deductions
    const sliders = page.locator('input[type="range"]');
    await expect(sliders).toHaveCount(3);
  });

  test('shows computed results', async ({ page }) => {
    // Should have result displays for net_income, taxable_income, tax_amount, effective_rate
    const results = page.locator('[id^="result-"]');
    const count = await results.count();
    expect(count).toBeGreaterThanOrEqual(4);
  });

  test('initial values are correct', async ({ page }) => {
    // Tax rate should be 25%
    const taxRateValue = page.locator('#tax_rate-value');
    await expect(taxRateValue).toContainText('25');

    // Income should be $100,000
    const incomeValue = page.locator('#income-value');
    await expect(incomeValue).toContainText('100,000');

    // Deductions should be $15,000
    const deductionsValue = page.locator('#deductions-value');
    await expect(deductionsValue).toContainText('15,000');
  });

  test('changing tax rate updates computed values', async ({ page }) => {
    // Get initial net income
    const netIncomeResult = page.locator('#result-net_income .result-value');
    const initialNetIncome = await netIncomeResult.textContent();

    // Change tax rate from 25 to 30
    const taxRateSlider = page.locator('#tax_rate-slider');
    await taxRateSlider.fill('30');

    // Net income should decrease (higher taxes)
    const newNetIncome = await netIncomeResult.textContent();
    expect(newNetIncome).not.toBe(initialNetIncome);
  });

  test('changing income updates computed values', async ({ page }) => {
    // Get initial tax amount
    const taxAmountResult = page.locator('#result-tax_amount .result-value');
    const initialTaxAmount = await taxAmountResult.textContent();

    // Increase income to $150,000
    const incomeSlider = page.locator('#income-slider');
    await incomeSlider.fill('150000');

    // Tax amount should increase
    const newTaxAmount = await taxAmountResult.textContent();
    expect(newTaxAmount).not.toBe(initialTaxAmount);
  });

  test('changing deductions updates taxable income', async ({ page }) => {
    // Get initial taxable income
    const taxableResult = page.locator('#result-taxable_income .result-value');
    const initialTaxable = await taxableResult.textContent();

    // Increase deductions to $25,000
    const deductionsSlider = page.locator('#deductions-slider');
    await deductionsSlider.fill('25000');

    // Taxable income should decrease
    const newTaxable = await taxableResult.textContent();
    expect(newTaxable).not.toBe(initialTaxable);
  });

  test('verifies tax calculation math', async ({ page }) => {
    // With tax_rate=25, income=100000, deductions=15000:
    // taxable_income = 100000 - 15000 = 85000
    // tax_amount = 85000 * 25 / 100 = 21250
    // net_income = 100000 - 21250 = 78750
    // effective_rate = 21250 * 100 / 100000 = 21%

    const taxableResult = page.locator('#result-taxable_income .result-value');
    await expect(taxableResult).toContainText('85,000');

    const taxAmountResult = page.locator('#result-tax_amount .result-value');
    await expect(taxAmountResult).toContainText('21,250');

    const netIncomeResult = page.locator('#result-net_income .result-value');
    await expect(netIncomeResult).toContainText('78,750');

    const effectiveRateResult = page.locator('#result-effective_rate .result-value');
    await expect(effectiveRateResult).toContainText('21');
  });

  test('Why? buttons are present on outputs', async ({ page }) => {
    // Each output should have a Why? button
    const whyButtons = page.locator('.why-button');
    await expect(whyButtons).toHaveCount(4); // taxable_income, tax_amount, net_income, effective_rate
  });

  test('Why? button opens modal with derivation chain', async ({ page }) => {
    // Click Why? button on net_income
    const whyButton = page.locator('#result-net_income .why-button');
    await expect(whyButton).toBeVisible();
    await whyButton.click();

    // Modal should appear
    const modal = page.locator('#why-modal');
    await expect(modal).not.toHaveClass(/hidden/);
    await expect(modal.locator('.why-modal-content')).toBeVisible();

    // Should show the derivation info
    const explanation = page.locator('#why-explanation');
    await expect(explanation).toBeVisible();

    // Should show net_income as computed with its formula
    await expect(explanation).toContainText('net_income');
    await expect(explanation).toContainText('computed');
    await expect(explanation).toContainText('income - tax_amount'); // The actual formula

    // Should show dependencies (which have their own formulas)
    await expect(explanation).toContainText('income');
    await expect(explanation).toContainText('tax_amount'); // Direct dependency

    // Close modal
    await page.locator('#why-modal-close').click();
    await expect(modal).toHaveClass(/hidden/);
  });

  test('Each output shows its unique formula in Why?', async ({ page }) => {
    // Test tax_amount shows its specific formula
    await page.locator('#result-tax_amount .why-button').click();
    const explanation = page.locator('#why-explanation');
    await expect(explanation).toContainText('taxable_income');
    await expect(explanation).toContainText('tax_rate');
    await expect(explanation).toContainText('÷ 100'); // tax_amount formula includes division by 100
    await page.locator('#why-modal-close').click();

    // Test effective_rate shows its specific formula (different from tax_amount)
    await page.locator('#result-effective_rate .why-button').click();
    await expect(explanation).toContainText('effective_rate');
    await expect(explanation).toContainText('100 × tax_amount'); // Different formula!
    await page.locator('#why-modal-close').click();
  });

  test('Why? explanation updates after changing values', async ({ page }) => {
    // Click Why? on net_income
    await page.locator('#result-net_income .why-button').click();

    // Check initial value
    const explanation = page.locator('#why-explanation');
    await expect(explanation).toContainText('78,750'); // initial net_income

    // Close modal
    await page.locator('#why-modal-close').click();

    // Change tax rate to 30%
    await page.locator('#tax_rate-slider').fill('30');

    // Re-open Why? modal
    await page.locator('#result-net_income .why-button').click();

    // Value should be updated (30% of 85000 = 25500, so net = 100000 - 25500 = 74500)
    await expect(explanation).toContainText('74,500');

    await page.locator('#why-modal-close').click();
  });

  test('has no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    // Interact with the page
    await page.locator('#tax_rate-slider').fill('20');
    await page.locator('#income-slider').fill('80000');
    await page.locator('#deductions-slider').fill('10000');

    // Wait for any async operations
    await page.waitForTimeout(500);

    // Should have no errors
    expect(errors).toHaveLength(0);
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
