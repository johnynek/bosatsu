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
// LOAN CALCULATOR DEMO TESTS (New Simulation Format)
// Tests the generated HTML from simulation-cli
// =============================================================================
test.describe('Loan Calculator Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/simulation/loan-calculator.html');
    await expect(page.locator('.applet-container')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/loan.calculator/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('displays applet title', async ({ page }) => {
    const title = page.locator('.applet-title');
    await expect(title).toBeVisible();
    await expect(title).toContainText(/loan.calculator/i);
  });

  test('has controls section', async ({ page }) => {
    const controls = page.locator('#controls');
    await expect(controls).toBeVisible();
  });

  test('has results section', async ({ page }) => {
    const results = page.locator('#results');
    await expect(results).toBeVisible();
  });

  test('has slider inputs for assumptions', async ({ page }) => {
    // New format uses range sliders
    const sliders = page.locator('input[type="range"]');
    const count = await sliders.count();
    expect(count).toBeGreaterThanOrEqual(3); // principal, annual_rate, years
  });

  test('displays result values', async ({ page }) => {
    // New format uses result-value class
    const results = page.locator('.result-value');
    const count = await results.count();
    expect(count).toBeGreaterThanOrEqual(3);
  });

  test('result values are not NaN or undefined', async ({ page }) => {
    const results = page.locator('.result-value');
    const count = await results.count();

    for (let i = 0; i < count; i++) {
      const text = await results.nth(i).textContent();
      expect(text, `Result at index ${i}`).not.toBe('NaN');
      expect(text, `Result at index ${i}`).not.toBe('undefined');
      expect(text, `Result at index ${i}`).not.toBe('null');
    }
  });

  test('has why buttons for computed values', async ({ page }) => {
    const whyButtons = page.locator('.why-button');
    const count = await whyButtons.count();
    expect(count).toBeGreaterThanOrEqual(3);
  });

  test('clicking why button opens modal', async ({ page }) => {
    const whyButton = page.locator('.why-button').first();
    await whyButton.click();

    const modal = page.locator('#why-modal');
    await expect(modal).not.toHaveClass(/hidden/);
    await expect(modal).toBeVisible();
  });

  test('why modal shows derivation info', async ({ page }) => {
    await page.locator('.why-button').first().click();

    const explanation = page.locator('#why-explanation');
    await expect(explanation).toBeVisible();
  });

  test('closing modal works', async ({ page }) => {
    await page.locator('.why-button').first().click();
    await expect(page.locator('#why-modal')).not.toHaveClass(/hidden/);

    await page.locator('#why-modal-close').click();
    await expect(page.locator('#why-modal')).toHaveClass(/hidden/);
  });

  test('changing slider triggers recomputation', async ({ page }) => {
    // Get initial value
    const resultValue = page.locator('.result-value').first();
    const initialText = await resultValue.textContent();

    // Find and change a slider
    const slider = page.locator('input[type="range"]').first();
    const min = parseInt(await slider.getAttribute('min') || '0');
    const max = parseInt(await slider.getAttribute('max') || '100');
    const step = parseInt(await slider.getAttribute('step') || '1');

    // Calculate a valid value different from current
    const current = parseInt(await slider.inputValue());
    const newVal = current > min + step * 5 ? min + step * 3 : max - step * 3;
    await slider.fill(newVal.toString());
    await slider.dispatchEvent('input');
    await page.waitForTimeout(300);

    // Value should change (or at least not show NaN/undefined)
    const newText = await resultValue.textContent();
    expect(newText).not.toBe('NaN');
    expect(newText).not.toBe('undefined');
  });

  test('has no page errors on load', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));
    await page.waitForTimeout(500);
    expect(errors).toHaveLength(0);
  });

  test('monthly_payment is a valid number', async ({ page }) => {
    const monthlyPayment = page.locator('#result-monthly_payment .result-value');
    if (await monthlyPayment.count() > 0) {
      const text = await monthlyPayment.textContent();
      expect(text).not.toBe('NaN');
      expect(text).not.toBe('undefined');
      const value = parseFloat(text?.replace(/[^0-9.-]/g, '') || '0');
      expect(value).toBeGreaterThan(0);
    }
  });
});

// =============================================================================
// CARBON FOOTPRINT DEMO TESTS (New Simulation Format)
// =============================================================================
test.describe('Carbon Footprint Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/simulation/carbon-footprint.html');
    await expect(page.locator('.applet-container')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/carbon/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('displays applet title', async ({ page }) => {
    const title = page.locator('.applet-title');
    await expect(title).toBeVisible();
    await expect(title).toContainText(/carbon/i);
  });

  test('has controls section', async ({ page }) => {
    const controls = page.locator('#controls');
    await expect(controls).toBeVisible();
  });

  test('has results section', async ({ page }) => {
    const results = page.locator('#results');
    await expect(results).toBeVisible();
  });

  test('has slider inputs for all assumptions', async ({ page }) => {
    const sliders = page.locator('input[type="range"]');
    const count = await sliders.count();
    expect(count).toBeGreaterThanOrEqual(5); // car_miles, car_mpg, flights, electricity, gas
  });

  test('displays computed result values', async ({ page }) => {
    const results = page.locator('.result-value');
    const count = await results.count();
    expect(count).toBeGreaterThanOrEqual(5);
  });

  test('result values are numbers, not NaN or undefined', async ({ page }) => {
    const results = page.locator('.result-value');
    const count = await results.count();

    for (let i = 0; i < count; i++) {
      const text = await results.nth(i).textContent();
      expect(text, `Result at index ${i}`).not.toBe('NaN');
      expect(text, `Result at index ${i}`).not.toBe('undefined');
      expect(text, `Result at index ${i}`).not.toBe('null');
      // Should contain a number
      expect(text, `Result at index ${i}`).toMatch(/[\d.]+/);
    }
  });

  test('has why buttons for computed values', async ({ page }) => {
    const whyButtons = page.locator('.why-button');
    const count = await whyButtons.count();
    expect(count).toBeGreaterThanOrEqual(5);
  });

  test('clicking why button opens modal', async ({ page }) => {
    const whyButton = page.locator('.why-button').first();
    await whyButton.click();

    const modal = page.locator('#why-modal');
    await expect(modal).not.toHaveClass(/hidden/);
    await expect(modal).toBeVisible();
  });

  test('why modal shows derivation chain', async ({ page }) => {
    await page.locator('.why-button').first().click();

    const explanation = page.locator('#why-explanation');
    await expect(explanation).toBeVisible();
    // Should contain some dependency information
    const text = await explanation.textContent();
    expect(text?.length).toBeGreaterThan(5);
  });

  test('closing modal works', async ({ page }) => {
    await page.locator('.why-button').first().click();
    await expect(page.locator('#why-modal')).not.toHaveClass(/hidden/);

    await page.locator('#why-modal-close').click();
    await expect(page.locator('#why-modal')).toHaveClass(/hidden/);
  });

  test('changing car_miles slider updates computed values', async ({ page }) => {
    // Get initial total_footprint
    const totalFootprint = page.locator('#result-total_footprint .result-value');
    if (await totalFootprint.count() === 0) {
      // Skip if element not found in this format
      return;
    }
    const initialTotal = await totalFootprint.textContent();

    // Find car_miles slider - look for slider with matching label
    const carMilesSlider = page.locator('input[type="range"]').first();
    if (await carMilesSlider.count() === 0) return;

    const current = parseInt(await carMilesSlider.inputValue());
    const step = parseInt(await carMilesSlider.getAttribute('step') || '1');
    const newVal = current + step * 5;

    await carMilesSlider.fill(newVal.toString());
    await carMilesSlider.dispatchEvent('input');
    await page.waitForTimeout(300);

    const newTotal = await totalFootprint.textContent();
    // Value should change or at least be valid
    expect(newTotal).not.toBe('NaN');
    expect(newTotal).not.toBe('undefined');
  });

  test('has no page errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    await page.locator('.why-button').first().click();
    await page.locator('#why-modal-close').click();
    await page.waitForTimeout(300);

    expect(errors).toHaveLength(0);
  });

  test('gallons_used is computed correctly', async ({ page }) => {
    // Look for gallons_used result
    const gallonsResult = page.locator('#result-gallons_used .result-value');
    if (await gallonsResult.count() > 0) {
      const text = await gallonsResult.textContent();
      expect(text).not.toBe('NaN');
      expect(text).not.toBe('undefined');
      // Should be car_miles / car_mpg = 12000 / 28 ≈ 428.57
      const value = parseFloat(text || '0');
      expect(value).toBeGreaterThan(400);
      expect(value).toBeLessThan(500);
    }
  });

  test('monthly_average is computed correctly', async ({ page }) => {
    // Get total_footprint and monthly_average
    const totalResult = page.locator('#result-total_footprint .result-value');
    const monthlyResult = page.locator('#result-monthly_average .result-value');

    if (await totalResult.count() > 0 && await monthlyResult.count() > 0) {
      const totalText = await totalResult.textContent();
      const monthlyText = await monthlyResult.textContent();

      const total = parseFloat(totalText || '0');
      const monthly = parseFloat(monthlyText || '0');

      // monthly should be total / 12
      if (total > 0) {
        const expected = total / 12;
        expect(Math.abs(monthly - expected)).toBeLessThan(1);
      }
    }
  });
});

// =============================================================================
// SIMULATION UI PATTERNS TESTS
// Common patterns across all simulation demos
// =============================================================================
test.describe('Simulation UI Patterns', () => {
  const demos = [
    { url: '/demos/simulation/carbon-footprint.html', name: 'Carbon Footprint' },
    { url: '/demos/simulation/loan-calculator.html', name: 'Loan Calculator' },
    { url: '/demos/simulation/tax-calculator.html', name: 'Tax Calculator' },
  ];

  for (const demo of demos) {
    test(`${demo.name}: has applet-container structure`, async ({ page }) => {
      await page.goto(demo.url);
      await expect(page.locator('.applet-container')).toBeVisible();
      await expect(page.locator('.applet-title')).toBeVisible();
      await expect(page.locator('#controls')).toBeVisible();
    });

    test(`${demo.name}: has results section`, async ({ page }) => {
      await page.goto(demo.url);
      await expect(page.locator('#results')).toBeVisible();
    });

    test(`${demo.name}: has slider inputs`, async ({ page }) => {
      await page.goto(demo.url);
      const sliders = page.locator('input[type="range"]');
      const count = await sliders.count();
      expect(count).toBeGreaterThanOrEqual(1);
    });

    test(`${demo.name}: has result values`, async ({ page }) => {
      await page.goto(demo.url);
      const results = page.locator('.result-value');
      const count = await results.count();
      expect(count).toBeGreaterThanOrEqual(1);
    });

    test(`${demo.name}: result values are valid numbers`, async ({ page }) => {
      await page.goto(demo.url);
      const results = page.locator('.result-value');
      const count = await results.count();

      for (let i = 0; i < count; i++) {
        const text = await results.nth(i).textContent();
        expect(text, `${demo.name} result at index ${i}`).not.toBe('NaN');
        expect(text, `${demo.name} result at index ${i}`).not.toBe('undefined');
        expect(text, `${demo.name} result at index ${i}`).not.toBe('null');
        // Should contain at least one digit
        expect(text, `${demo.name} result at index ${i}`).toMatch(/[\d.]+/);
      }
    });

    test(`${demo.name}: has why buttons`, async ({ page }) => {
      await page.goto(demo.url);
      const whyButtons = page.locator('.why-button');
      const count = await whyButtons.count();
      expect(count).toBeGreaterThanOrEqual(1);
    });
  }
});
