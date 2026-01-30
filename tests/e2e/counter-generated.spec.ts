import { test, expect } from '@playwright/test';

/**
 * Tests for the generated BosatsuUI counter demo.
 *
 * This counter is generated from demos/ui/counter.bosatsu via:
 *   bosatsu-sim ui demos/ui/counter.bosatsu -o counter.html
 *
 * It demonstrates the full BosatsuUI pipeline:
 *   .bosatsu source -> TypedExpr -> UIAnalyzer -> binding extraction -> HTML
 */
test.describe('BosatsuUI Counter (Generated)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/ui/counter.html');
    // Wait for the app to render
    await expect(page.locator('#app')).toBeVisible({ timeout: 5000 });
  });

  // ==========================================================================
  // Basic Functionality
  // ==========================================================================

  test('page loads and displays initial state', async ({ page }) => {
    const display = page.locator('#count-display');
    await expect(display).toBeVisible();
    await expect(display).toHaveText('0');
  });

  test('has increment and decrement buttons', async ({ page }) => {
    const incrementBtn = page.locator('button:has-text("+")');
    const decrementBtn = page.locator('button:has-text("-")');

    await expect(incrementBtn).toBeVisible();
    await expect(decrementBtn).toBeVisible();
  });

  test('clicking + increments the counter', async ({ page }) => {
    const display = page.locator('#count-display');
    const incrementBtn = page.locator('button:has-text("+")');

    await expect(display).toHaveText('0');
    await incrementBtn.click();
    await expect(display).toHaveText('1');
    await incrementBtn.click();
    await expect(display).toHaveText('2');
    await incrementBtn.click();
    await expect(display).toHaveText('3');
  });

  test('clicking - decrements the counter', async ({ page }) => {
    const display = page.locator('#count-display');
    const decrementBtn = page.locator('button:has-text("-")');

    await expect(display).toHaveText('0');
    await decrementBtn.click();
    await expect(display).toHaveText('-1');
    await decrementBtn.click();
    await expect(display).toHaveText('-2');
  });

  test('counter supports positive and negative values', async ({ page }) => {
    const display = page.locator('#count-display');
    const incrementBtn = page.locator('button:has-text("+")');
    const decrementBtn = page.locator('button:has-text("-")');

    // Go positive
    await incrementBtn.click();
    await incrementBtn.click();
    await expect(display).toHaveText('2');

    // Go back to zero
    await decrementBtn.click();
    await decrementBtn.click();
    await expect(display).toHaveText('0');

    // Go negative
    await decrementBtn.click();
    await expect(display).toHaveText('-1');
  });

  // ==========================================================================
  // Multiple Operations
  // ==========================================================================

  test('counter handles multiple clicks correctly', async ({ page }) => {
    const display = page.locator('#count-display');
    const incrementBtn = page.locator('button:has-text("+")');
    const decrementBtn = page.locator('button:has-text("-")');

    // Click + 5 times
    for (let i = 0; i < 5; i++) {
      await incrementBtn.click();
    }
    await expect(display).toHaveText('5');

    // Click - 3 times
    for (let i = 0; i < 3; i++) {
      await decrementBtn.click();
    }
    await expect(display).toHaveText('2');
  });

  test('rapid clicking updates correctly', async ({ page }) => {
    const display = page.locator('#count-display');
    const incrementBtn = page.locator('button:has-text("+")');

    // Rapidly click 20 times
    for (let i = 0; i < 20; i++) {
      await incrementBtn.click();
    }

    // Should be exactly 20
    await expect(display).toHaveText('20');
  });

  test('alternating clicks work correctly', async ({ page }) => {
    const display = page.locator('#count-display');
    const incrementBtn = page.locator('button:has-text("+")');
    const decrementBtn = page.locator('button:has-text("-")');

    // Alternate: +, -, +, -, +, +, +
    await incrementBtn.click();
    await expect(display).toHaveText('1');
    await decrementBtn.click();
    await expect(display).toHaveText('0');
    await incrementBtn.click();
    await expect(display).toHaveText('1');
    await decrementBtn.click();
    await expect(display).toHaveText('0');
    await incrementBtn.click();
    await incrementBtn.click();
    await incrementBtn.click();
    await expect(display).toHaveText('3');
  });

  // ==========================================================================
  // Edge Cases
  // ==========================================================================

  test('handles large positive numbers', async ({ page }) => {
    const display = page.locator('#count-display');
    const incrementBtn = page.locator('button:has-text("+")');

    // Click 100 times
    for (let i = 0; i < 100; i++) {
      await incrementBtn.click();
    }
    await expect(display).toHaveText('100');
  });

  test('handles large negative numbers', async ({ page }) => {
    const display = page.locator('#count-display');
    const decrementBtn = page.locator('button:has-text("-")');

    // Click 50 times
    for (let i = 0; i < 50; i++) {
      await decrementBtn.click();
    }
    await expect(display).toHaveText('-50');
  });

  // ==========================================================================
  // DOM and Structure
  // ==========================================================================

  test('has correct HTML structure', async ({ page }) => {
    // Should have an #app container
    await expect(page.locator('#app')).toBeVisible();

    // Should have a card with class
    await expect(page.locator('.card')).toBeVisible();

    // Should have a Counter title
    await expect(page.locator('h1')).toContainText('Counter');

    // Should have the display element
    await expect(page.locator('#count-display')).toBeVisible();

    // Should have two buttons
    const buttons = page.locator('button');
    await expect(buttons).toHaveCount(2);
  });

  test('display element is inside the card', async ({ page }) => {
    const card = page.locator('.card');
    const display = card.locator('#count-display');
    await expect(display).toBeVisible();
  });

  // ==========================================================================
  // No Console Errors
  // ==========================================================================

  test('has no console errors during interaction', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    // Interact with the page
    const incrementBtn = page.locator('button:has-text("+")');
    const decrementBtn = page.locator('button:has-text("-")');

    await incrementBtn.click();
    await incrementBtn.click();
    await decrementBtn.click();

    // Wait a moment for any async errors
    await page.waitForTimeout(100);

    expect(errors).toHaveLength(0);
  });

  // ==========================================================================
  // State Persistence Within Session
  // ==========================================================================

  test('state persists through multiple interactions', async ({ page }) => {
    const display = page.locator('#count-display');
    const incrementBtn = page.locator('button:has-text("+")');
    const decrementBtn = page.locator('button:has-text("-")');

    // Build up a count
    await incrementBtn.click();
    await incrementBtn.click();
    await incrementBtn.click();
    await expect(display).toHaveText('3');

    // Wait a bit (simulating user pause)
    await page.waitForTimeout(500);

    // Continue interacting
    await decrementBtn.click();
    await expect(display).toHaveText('2');

    // Wait again
    await page.waitForTimeout(500);

    // Continue
    await incrementBtn.click();
    await incrementBtn.click();
    await expect(display).toHaveText('4');
  });

  // ==========================================================================
  // Visual/Style Tests
  // ==========================================================================

  test('buttons are clickable and styled', async ({ page }) => {
    const incrementBtn = page.locator('button:has-text("+")');
    const decrementBtn = page.locator('button:has-text("-")');

    // Check buttons are enabled
    await expect(incrementBtn).toBeEnabled();
    await expect(decrementBtn).toBeEnabled();

    // Check buttons have some styling (not unstyled default)
    const incBtnStyles = await incrementBtn.evaluate(el => {
      const styles = window.getComputedStyle(el);
      return {
        cursor: styles.cursor,
        borderRadius: styles.borderRadius,
      };
    });
    expect(incBtnStyles.cursor).toBe('pointer');
  });

  test('display element shows number prominently', async ({ page }) => {
    const display = page.locator('#count-display');

    // Check it has some styling that makes it prominent
    const displayStyles = await display.evaluate(el => {
      const styles = window.getComputedStyle(el);
      return {
        fontSize: parseInt(styles.fontSize),
      };
    });

    // Font size should be larger than default body text
    expect(displayStyles.fontSize).toBeGreaterThanOrEqual(16);
  });
});
