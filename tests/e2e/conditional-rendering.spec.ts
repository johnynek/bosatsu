import { test, expect } from '@playwright/test';

/**
 * Tests for the conditional rendering demo.
 *
 * This demo demonstrates BosatsuUI's ability to handle sum types (enums)
 * with conditional bindings. The key feature is that bindings have a
 * `when` clause that specifies which variant of a sum type they apply to.
 *
 * The demo has a Status enum with three variants:
 *   - Loading
 *   - Error(message: String)
 *   - Success(data: String)
 *
 * Clicking the buttons changes the state to different variants, and
 * only the bindings whose conditions match the current variant are applied.
 */
test.describe('Conditional Rendering Demo', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/ui/conditional.html');
    // Wait for the app to render
    await expect(page.locator('#app')).toBeVisible({ timeout: 5000 });
  });

  // ==========================================================================
  // Initial State
  // ==========================================================================

  test('page loads and displays initial Loading state', async ({ page }) => {
    const loadingContent = page.locator('#loading-content');
    const errorContent = page.locator('#error-content');
    const successContent = page.locator('#success-content');

    // Loading should be visible
    await expect(loadingContent).toBeVisible();
    await expect(loadingContent).toContainText('Loading...');

    // Error and Success should be hidden
    await expect(errorContent).not.toBeVisible();
    await expect(successContent).not.toBeVisible();
  });

  test('has all three state buttons', async ({ page }) => {
    const loadingBtn = page.locator('#loading-btn');
    const errorBtn = page.locator('#error-btn');
    const successBtn = page.locator('#success-btn');

    await expect(loadingBtn).toBeVisible();
    await expect(loadingBtn).toHaveText('Loading');

    await expect(errorBtn).toBeVisible();
    await expect(errorBtn).toHaveText('Error');

    await expect(successBtn).toBeVisible();
    await expect(successBtn).toHaveText('Success');
  });

  // ==========================================================================
  // State Transitions
  // ==========================================================================

  test('clicking Error button shows error content', async ({ page }) => {
    const loadingContent = page.locator('#loading-content');
    const errorContent = page.locator('#error-content');
    const successContent = page.locator('#success-content');
    const errorBtn = page.locator('#error-btn');

    // Click Error button
    await errorBtn.click();

    // Error should be visible
    await expect(errorContent).toBeVisible();
    await expect(errorContent).toContainText('Something went wrong');

    // Loading and Success should be hidden
    await expect(loadingContent).not.toBeVisible();
    await expect(successContent).not.toBeVisible();
  });

  test('clicking Success button shows success content', async ({ page }) => {
    const loadingContent = page.locator('#loading-content');
    const errorContent = page.locator('#error-content');
    const successContent = page.locator('#success-content');
    const successBtn = page.locator('#success-btn');

    // Click Success button
    await successBtn.click();

    // Success should be visible
    await expect(successContent).toBeVisible();
    await expect(successContent).toContainText('Data loaded successfully');

    // Loading and Error should be hidden
    await expect(loadingContent).not.toBeVisible();
    await expect(errorContent).not.toBeVisible();
  });

  test('clicking Loading button returns to loading state', async ({ page }) => {
    const loadingContent = page.locator('#loading-content');
    const errorContent = page.locator('#error-content');
    const successContent = page.locator('#success-content');
    const loadingBtn = page.locator('#loading-btn');
    const successBtn = page.locator('#success-btn');

    // First go to Success
    await successBtn.click();
    await expect(successContent).toBeVisible();

    // Then click Loading
    await loadingBtn.click();

    // Loading should be visible again
    await expect(loadingContent).toBeVisible();
    await expect(loadingContent).toContainText('Loading...');

    // Error and Success should be hidden
    await expect(errorContent).not.toBeVisible();
    await expect(successContent).not.toBeVisible();
  });

  // ==========================================================================
  // State Cycling
  // ==========================================================================

  test('can cycle through all states', async ({ page }) => {
    const loadingContent = page.locator('#loading-content');
    const errorContent = page.locator('#error-content');
    const successContent = page.locator('#success-content');
    const loadingBtn = page.locator('#loading-btn');
    const errorBtn = page.locator('#error-btn');
    const successBtn = page.locator('#success-btn');

    // Start at Loading
    await expect(loadingContent).toBeVisible();

    // Go to Error
    await errorBtn.click();
    await expect(errorContent).toBeVisible();
    await expect(loadingContent).not.toBeVisible();
    await expect(successContent).not.toBeVisible();

    // Go to Success
    await successBtn.click();
    await expect(successContent).toBeVisible();
    await expect(loadingContent).not.toBeVisible();
    await expect(errorContent).not.toBeVisible();

    // Back to Loading
    await loadingBtn.click();
    await expect(loadingContent).toBeVisible();
    await expect(errorContent).not.toBeVisible();
    await expect(successContent).not.toBeVisible();

    // Go to Error again
    await errorBtn.click();
    await expect(errorContent).toBeVisible();
  });

  test('multiple rapid state changes work correctly', async ({ page }) => {
    const loadingContent = page.locator('#loading-content');
    const errorContent = page.locator('#error-content');
    const successContent = page.locator('#success-content');
    const loadingBtn = page.locator('#loading-btn');
    const errorBtn = page.locator('#error-btn');
    const successBtn = page.locator('#success-btn');

    // Rapidly cycle through states
    await successBtn.click();
    await errorBtn.click();
    await loadingBtn.click();
    await successBtn.click();
    await errorBtn.click();

    // Should end at Error
    await expect(errorContent).toBeVisible();
    await expect(loadingContent).not.toBeVisible();
    await expect(successContent).not.toBeVisible();
  });

  // ==========================================================================
  // Error State Details
  // ==========================================================================

  test('error state shows correct message', async ({ page }) => {
    const errorContent = page.locator('#error-content');
    const errorBtn = page.locator('#error-btn');

    await errorBtn.click();

    // Check the exact message
    await expect(errorContent).toHaveText('Something went wrong');
  });

  test('error state has correct styling', async ({ page }) => {
    const errorContent = page.locator('#error-content');
    const errorBtn = page.locator('#error-btn');

    await errorBtn.click();

    // Error should have error class styling
    const styles = await errorContent.evaluate(el => {
      const computed = window.getComputedStyle(el);
      return {
        color: computed.color,
        display: computed.display,
      };
    });

    // Should be visible (not display: none)
    expect(styles.display).not.toBe('none');
  });

  // ==========================================================================
  // Success State Details
  // ==========================================================================

  test('success state shows correct data', async ({ page }) => {
    const successContent = page.locator('#success-content');
    const successBtn = page.locator('#success-btn');

    await successBtn.click();

    // Check the exact message
    await expect(successContent).toHaveText('Data loaded successfully!');
  });

  test('success state has correct styling', async ({ page }) => {
    const successContent = page.locator('#success-content');
    const successBtn = page.locator('#success-btn');

    await successBtn.click();

    // Success should have success class styling
    const styles = await successContent.evaluate(el => {
      const computed = window.getComputedStyle(el);
      return {
        color: computed.color,
        display: computed.display,
      };
    });

    // Should be visible
    expect(styles.display).not.toBe('none');
  });

  // ==========================================================================
  // State Debug Display
  // ==========================================================================

  test('state debug shows current state object', async ({ page }) => {
    const stateDebug = page.locator('#state-debug');
    const loadingBtn = page.locator('#loading-btn');
    const errorBtn = page.locator('#error-btn');
    const successBtn = page.locator('#success-btn');

    // Initial state
    await expect(stateDebug).toContainText('Loading');

    // After Error
    await errorBtn.click();
    await expect(stateDebug).toContainText('Error');

    // After Success
    await successBtn.click();
    await expect(stateDebug).toContainText('Success');

    // Back to Loading
    await loadingBtn.click();
    await expect(stateDebug).toContainText('Loading');
  });

  // ==========================================================================
  // Button Active States
  // ==========================================================================

  test('buttons show active state correctly', async ({ page }) => {
    const loadingBtn = page.locator('#loading-btn');
    const errorBtn = page.locator('#error-btn');
    const successBtn = page.locator('#success-btn');

    // Initial: Loading should be active
    await expect(loadingBtn).toHaveClass(/active/);
    await expect(errorBtn).not.toHaveClass(/active/);
    await expect(successBtn).not.toHaveClass(/active/);

    // After clicking Error
    await errorBtn.click();
    await expect(loadingBtn).not.toHaveClass(/active/);
    await expect(errorBtn).toHaveClass(/active/);
    await expect(successBtn).not.toHaveClass(/active/);

    // After clicking Success
    await successBtn.click();
    await expect(loadingBtn).not.toHaveClass(/active/);
    await expect(errorBtn).not.toHaveClass(/active/);
    await expect(successBtn).toHaveClass(/active/);

    // After clicking Loading
    await loadingBtn.click();
    await expect(loadingBtn).toHaveClass(/active/);
    await expect(errorBtn).not.toHaveClass(/active/);
    await expect(successBtn).not.toHaveClass(/active/);
  });

  // ==========================================================================
  // HTML Structure
  // ==========================================================================

  test('has correct HTML structure', async ({ page }) => {
    // Should have app container
    await expect(page.locator('#app')).toBeVisible();

    // Should have card
    await expect(page.locator('.card')).toBeVisible();

    // Should have title
    await expect(page.locator('h1')).toContainText('Conditional Rendering Demo');

    // Should have buttons container
    await expect(page.locator('.buttons')).toBeVisible();

    // Should have result container
    await expect(page.locator('.result')).toBeVisible();

    // Should have three state buttons
    const buttons = page.locator('.buttons button');
    await expect(buttons).toHaveCount(3);
  });

  // ==========================================================================
  // No Console Errors
  // ==========================================================================

  test('has no console errors during state transitions', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    const loadingBtn = page.locator('#loading-btn');
    const errorBtn = page.locator('#error-btn');
    const successBtn = page.locator('#success-btn');

    // Perform all state transitions
    await errorBtn.click();
    await successBtn.click();
    await loadingBtn.click();
    await errorBtn.click();
    await loadingBtn.click();
    await successBtn.click();

    // Wait for any async errors
    await page.waitForTimeout(100);

    expect(errors).toHaveLength(0);
  });

  // ==========================================================================
  // Only One State Visible At A Time
  // ==========================================================================

  test('exactly one state content is visible at any time', async ({ page }) => {
    const loadingContent = page.locator('#loading-content');
    const errorContent = page.locator('#error-content');
    const successContent = page.locator('#success-content');
    const loadingBtn = page.locator('#loading-btn');
    const errorBtn = page.locator('#error-btn');
    const successBtn = page.locator('#success-btn');

    // Helper to count visible elements
    const countVisible = async () => {
      const results = await Promise.all([
        loadingContent.isVisible(),
        errorContent.isVisible(),
        successContent.isVisible(),
      ]);
      return results.filter(v => v).length;
    };

    // Initial state
    expect(await countVisible()).toBe(1);

    // After Error
    await errorBtn.click();
    expect(await countVisible()).toBe(1);

    // After Success
    await successBtn.click();
    expect(await countVisible()).toBe(1);

    // After Loading
    await loadingBtn.click();
    expect(await countVisible()).toBe(1);
  });
});
