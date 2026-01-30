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
// BOSATSU UI TODO LIST DEMO (Generated from .bosatsu source)
// Tests the generated todo-list which demonstrates per-item O(1) DOM updates
// =============================================================================
test.describe('BosatsuUI Todo List Demo (Generated)', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/ui/todo-list_generated.html');
    // Wait for the todo list to be visible
    await expect(page.locator('.todo-list')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    // Multiple .card elements exist, use .first()
    await expect(page.locator('.card').first()).toBeVisible();
    await expect(page.locator('h1')).toHaveText('BosatsuUI Todo List');
  });

  test('displays three todo items', async ({ page }) => {
    const todoItems = page.locator('.todo-item');
    await expect(todoItems).toHaveCount(3);
  });

  test('todo 1 starts unchecked', async ({ page }) => {
    const checkbox = page.locator('#todo-1-checkbox');
    await expect(checkbox).not.toHaveClass(/checked/);
  });

  test('todo 2 starts unchecked', async ({ page }) => {
    const checkbox = page.locator('#todo-2-checkbox');
    await expect(checkbox).not.toHaveClass(/checked/);
  });

  test('todo 3 starts checked (completed)', async ({ page }) => {
    const checkbox = page.locator('#todo-3-checkbox');
    await expect(checkbox).toHaveClass(/checked/);
  });

  test('clicking checkbox toggles completion state', async ({ page }) => {
    const checkbox1 = page.locator('#todo-1-checkbox');

    // Initially unchecked
    await expect(checkbox1).not.toHaveClass(/checked/);

    // Click to check
    await checkbox1.click();
    await expect(checkbox1).toHaveClass(/checked/);

    // Click to uncheck
    await checkbox1.click();
    await expect(checkbox1).not.toHaveClass(/checked/);
  });

  test('toggling one todo does not affect others', async ({ page }) => {
    const checkbox1 = page.locator('#todo-1-checkbox');
    const checkbox2 = page.locator('#todo-2-checkbox');
    const checkbox3 = page.locator('#todo-3-checkbox');

    // Initial states
    await expect(checkbox1).not.toHaveClass(/checked/);
    await expect(checkbox2).not.toHaveClass(/checked/);
    await expect(checkbox3).toHaveClass(/checked/);

    // Toggle todo 1
    await checkbox1.click();
    await expect(checkbox1).toHaveClass(/checked/);
    await expect(checkbox2).not.toHaveClass(/checked/); // unchanged
    await expect(checkbox3).toHaveClass(/checked/); // unchanged

    // Toggle todo 2
    await checkbox2.click();
    await expect(checkbox1).toHaveClass(/checked/); // unchanged
    await expect(checkbox2).toHaveClass(/checked/);
    await expect(checkbox3).toHaveClass(/checked/); // unchanged
  });

  test('multiple toggles work correctly', async ({ page }) => {
    const checkbox3 = page.locator('#todo-3-checkbox');

    // Initially checked
    await expect(checkbox3).toHaveClass(/checked/);

    // Toggle several times
    await checkbox3.click();
    await expect(checkbox3).not.toHaveClass(/checked/);

    await checkbox3.click();
    await expect(checkbox3).toHaveClass(/checked/);

    await checkbox3.click();
    await expect(checkbox3).not.toHaveClass(/checked/);

    await checkbox3.click();
    await expect(checkbox3).toHaveClass(/checked/);
  });

  test('displays binding information', async ({ page }) => {
    const bindingInfo = page.locator('.binding-info');
    await expect(bindingInfo).toBeVisible();
    await expect(bindingInfo).toContainText('O(1) updates');
  });

  test('no JavaScript errors during interactions', async ({ page }) => {
    const logs = setupConsoleCapture(page);

    // Perform several interactions
    await page.locator('#todo-1-checkbox').click();
    await page.locator('#todo-2-checkbox').click();
    await page.locator('#todo-3-checkbox').click();

    // Check no page errors
    const errors = logs.filter(log => log.includes('[PAGE ERROR]'));
    expect(errors).toHaveLength(0);
  });
});
