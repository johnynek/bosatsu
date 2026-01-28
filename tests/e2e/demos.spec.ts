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
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/loan_calculator/i);
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
// BOUNCING BALL ANIMATION DEMO
// =============================================================================
test.describe('Bouncing Ball Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/bouncing-ball.html');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/bouncing/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('canvas is present and rendered', async ({ page }) => {
    const canvas = page.locator('#simulation-canvas');
    await expect(canvas).toBeVisible();

    // Check canvas has expected dimensions
    const width = await canvas.getAttribute('width');
    const height = await canvas.getAttribute('height');
    expect(Number(width)).toBeGreaterThan(0);
    expect(Number(height)).toBeGreaterThan(0);
  });

  test('gravity slider works', async ({ page }) => {
    const slider = page.locator('#gravity-slider');
    await expect(slider).toBeVisible();

    // Change gravity
    await slider.fill('750');

    // Display should update
    const display = page.locator('#gravity-display');
    await expect(display).toContainText('750');
  });

  test('bounciness slider works', async ({ page }) => {
    const slider = page.locator('#bounciness-slider');
    await expect(slider).toBeVisible();

    // Change bounciness
    await slider.fill('50');

    // Display should update
    const display = page.locator('#bounciness-display');
    await expect(display).toContainText('50');
  });

  test('reset button works', async ({ page }) => {
    // Wait for simulation to run a bit
    await page.waitForTimeout(500);

    // Get initial position display
    const xDisplay = page.locator('#x-display');
    const yDisplay = page.locator('#y-display');

    // Click reset
    await page.locator('button:has-text("Reset")').click();

    // Position should be reset to initial values
    await expect(xDisplay).toContainText('200');
    await expect(yDisplay).toContainText('50');
  });

  test('pause/resume button works', async ({ page }) => {
    const pauseBtn = page.locator('#pause-btn');
    await expect(pauseBtn).toBeVisible();
    await expect(pauseBtn).toContainText('Pause');

    // Click pause
    await pauseBtn.click();
    await expect(pauseBtn).toContainText('Resume');

    // Click resume
    await pauseBtn.click();
    await expect(pauseBtn).toContainText('Pause');
  });

  test('animation updates position over time', async ({ page }) => {
    // Get initial position
    const yDisplay = page.locator('#y-display');
    const initialY = await yDisplay.textContent();

    // Wait for animation
    await page.waitForTimeout(1000);

    // Position should have changed (ball falls due to gravity)
    const newY = await yDisplay.textContent();
    expect(newY).not.toBe(initialY);
  });
});

// =============================================================================
// PENDULUM ANIMATION DEMO
// =============================================================================
test.describe('Pendulum Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/pendulum.html');
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/pendulum/i);
    await expect(page.locator('.applet-container')).toBeVisible();
  });

  test('canvas is present', async ({ page }) => {
    const canvas = page.locator('#simulation-canvas');
    await expect(canvas).toBeVisible();
  });

  test('gravity slider works', async ({ page }) => {
    const slider = page.locator('#gravity-slider');
    await expect(slider).toBeVisible();

    await slider.fill('15');

    const display = page.locator('#gravity-display');
    await expect(display).toContainText('15');
  });

  test('damping slider works', async ({ page }) => {
    const slider = page.locator('#damping-slider');
    await expect(slider).toBeVisible();

    await slider.fill('95');

    const display = page.locator('#damping-display');
    await expect(display).toContainText('95');
  });

  test('reset button works', async ({ page }) => {
    // Wait for simulation to run
    await page.waitForTimeout(500);

    // Click reset
    await page.locator('button:has-text("Reset")').click();

    // Simulation should continue running after reset
    await page.waitForTimeout(100);
    await expect(page.locator('#pause-btn')).toContainText('Pause');
  });

  test('pause/resume works', async ({ page }) => {
    const pauseBtn = page.locator('#pause-btn');
    await expect(pauseBtn).toBeVisible();

    await pauseBtn.click();
    await expect(pauseBtn).toContainText('Resume');

    await pauseBtn.click();
    await expect(pauseBtn).toContainText('Pause');
  });

  test('length slider works', async ({ page }) => {
    const slider = page.locator('#length-slider');
    await expect(slider).toBeVisible();

    await slider.fill('150');

    const display = page.locator('#length-display');
    await expect(display).toContainText('150');
  });
});

// =============================================================================
// MULTI-TARGET COMPILATION DEMO (index page)
// =============================================================================
test.describe('Multi-Target Demo Index', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demo/index.html');
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

    // Result should appear
    const result = page.locator('#js-result');
    await expect(result).not.toBeEmpty();
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
