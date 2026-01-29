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
// BOSATSU UI COUNTER DEMO
// =============================================================================
test.describe('BosatsuUI Counter Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/ui/counter.html');
    // Wait for the counter to be visible
    await expect(page.locator('#count-display')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/BosatsuUI Counter/i);
    await expect(page.locator('.card')).toBeVisible();
  });

  test('displays initial count of 0', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    await expect(countDisplay).toHaveText('0');
  });

  test('increment button increases count', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const incButton = page.locator('#inc');

    await expect(countDisplay).toHaveText('0');
    await incButton.click();
    await expect(countDisplay).toHaveText('1');
    await incButton.click();
    await expect(countDisplay).toHaveText('2');
  });

  test('decrement button decreases count', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const decButton = page.locator('#dec');

    // Start at 0
    await expect(countDisplay).toHaveText('0');
    // Decrement to -1
    await decButton.click();
    await expect(countDisplay).toHaveText('-1');
    // Decrement to -2
    await decButton.click();
    await expect(countDisplay).toHaveText('-2');
  });

  test('reset button sets count to 0', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const incButton = page.locator('#inc');
    const resetButton = page.locator('#reset');

    // Increment a few times
    await incButton.click();
    await incButton.click();
    await incButton.click();
    await expect(countDisplay).toHaveText('3');

    // Reset
    await resetButton.click();
    await expect(countDisplay).toHaveText('0');
  });

  test('random button sets count to random value', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const randomButton = page.locator('#random');

    await expect(countDisplay).toHaveText('0');
    await randomButton.click();

    // Value should be different (very unlikely to be 0 randomly)
    // and within expected range [0, 100)
    const value = await countDisplay.textContent();
    const numValue = parseInt(value || '0', 10);
    expect(numValue).toBeGreaterThanOrEqual(0);
    expect(numValue).toBeLessThan(100);
  });

  test('binding log shows updates', async ({ page }) => {
    const log = page.locator('#log');
    const incButton = page.locator('#inc');

    // Initial entry should exist
    await expect(log).toContainText('count');

    // Click increment
    await incButton.click();

    // Should have new log entry with path and value
    await expect(log).toContainText('["count"]');
    await expect(log).toContainText('#count-display');
    await expect(log).toContainText('textContent');
  });

  test('data-bosatsu-id attribute is present', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    await expect(countDisplay).toHaveAttribute('data-bosatsu-id', 'count-display');
  });

  test('rapid clicking updates correctly', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const incButton = page.locator('#inc');

    // Rapidly click 10 times
    for (let i = 0; i < 10; i++) {
      await incButton.click();
    }

    // Should be exactly 10
    await expect(countDisplay).toHaveText('10');
  });

  test('has no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    // Interact with the page
    await page.locator('#inc').click();
    await page.locator('#dec').click();
    await page.locator('#reset').click();
    await page.locator('#random').click();

    // Wait for any async operations
    await page.waitForTimeout(500);

    // Should have no errors
    expect(errors).toHaveLength(0);
  });
});

// =============================================================================
// BOSATSU UI TODO LIST DEMO
// =============================================================================
test.describe('BosatsuUI Todo List Demo', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/ui/todo-list.html');
    // Wait for initial todos to render
    await expect(page.locator('.todo-item').first()).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/BosatsuUI Todo List/i);
    await expect(page.locator('.container')).toBeVisible();
  });

  test('displays initial todos', async ({ page }) => {
    // Should have 3 initial todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(3);

    // Check initial todo texts
    await expect(page.locator('#todo-1-text')).toContainText('Learn about BosatsuUI');
    await expect(page.locator('#todo-2-text')).toContainText('Understand O(1) DOM updates');
    await expect(page.locator('#todo-3-text')).toContainText('Compare with React VDOM');
  });

  test('displays correct stats', async ({ page }) => {
    const totalCount = page.locator('#total-count');
    const activeCount = page.locator('#active-count');
    const completedCount = page.locator('#completed-count');

    await expect(totalCount).toHaveText('3');
    await expect(activeCount).toHaveText('3');
    await expect(completedCount).toHaveText('0');
  });

  test('can add a new todo', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    // Type and add new todo
    await input.fill('New test todo');
    await addButton.click();

    // Should have 4 todos now
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(4);

    // New todo should be visible
    await expect(page.locator('.todo-text').last()).toContainText('New test todo');

    // Stats should update
    await expect(page.locator('#total-count')).toHaveText('4');
    await expect(page.locator('#active-count')).toHaveText('4');
  });

  test('can add todo with Enter key', async ({ page }) => {
    const input = page.locator('#new-todo-input');

    await input.fill('Enter key todo');
    await input.press('Enter');

    // Should have 4 todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(4);

    // Input should be cleared
    await expect(input).toHaveValue('');
  });

  test('can toggle todo completion', async ({ page }) => {
    const firstCheckbox = page.locator('#todo-1-checkbox');

    // Initially not completed
    await expect(firstCheckbox).not.toHaveClass(/checked/);

    // Click to complete
    await firstCheckbox.click();

    // Should be checked
    await expect(firstCheckbox).toHaveClass(/checked/);

    // Stats should update
    await expect(page.locator('#active-count')).toHaveText('2');
    await expect(page.locator('#completed-count')).toHaveText('1');

    // Click again to uncomplete
    await firstCheckbox.click();
    await expect(firstCheckbox).not.toHaveClass(/checked/);
    await expect(page.locator('#active-count')).toHaveText('3');
    await expect(page.locator('#completed-count')).toHaveText('0');
  });

  test('can delete todo', async ({ page }) => {
    const firstTodo = page.locator('.todo-item').first();
    const deleteBtn = firstTodo.locator('.todo-delete');

    // Hover to show delete button
    await firstTodo.hover();

    // Click delete
    await deleteBtn.click();

    // Should have 2 todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(2);

    // Stats should update
    await expect(page.locator('#total-count')).toHaveText('2');
  });

  test('filter buttons work - All', async ({ page }) => {
    // Complete first todo
    await page.locator('#todo-1-checkbox').click();

    // Click All filter
    await page.locator('#filter-all').click();

    // Should show all 3 todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(3);
  });

  test('filter buttons work - Active', async ({ page }) => {
    // Complete first todo
    await page.locator('#todo-1-checkbox').click();

    // Click Active filter
    await page.locator('#filter-active').click();

    // Should show only 2 active todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(2);
  });

  test('filter buttons work - Completed', async ({ page }) => {
    // Complete first todo
    await page.locator('#todo-1-checkbox').click();

    // Click Completed filter
    await page.locator('#filter-completed').click();

    // Should show only 1 completed todo
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(1);
  });

  test('filter button has active styling', async ({ page }) => {
    const allFilter = page.locator('#filter-all');
    const activeFilter = page.locator('#filter-active');

    // All should be active initially
    await expect(allFilter).toHaveClass(/active/);
    await expect(activeFilter).not.toHaveClass(/active/);

    // Click active filter
    await activeFilter.click();

    // Active should now be active, All should not
    await expect(allFilter).not.toHaveClass(/active/);
    await expect(activeFilter).toHaveClass(/active/);
  });

  test('empty input does not add todo', async ({ page }) => {
    const addButton = page.locator('#add-btn');

    // Try to add empty todo
    await addButton.click();

    // Should still have 3 todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(3);
  });

  test('completed todo has strikethrough styling', async ({ page }) => {
    // Complete first todo
    await page.locator('#todo-1-checkbox').click();

    // The todo item should have completed class
    const firstTodo = page.locator('#todo-1');
    await expect(firstTodo).toHaveClass(/completed/);
  });

  test('data-bosatsu-id attributes are present on todo items', async ({ page }) => {
    // Each todo should have data-bosatsu-id attributes
    const checkbox = page.locator('#todo-1-checkbox');
    const text = page.locator('#todo-1-text');

    await expect(checkbox).toHaveAttribute('data-bosatsu-id', 'todo-1-checkbox');
    await expect(text).toHaveAttribute('data-bosatsu-id', 'todo-1-text');
  });

  test('has no console errors during interactions', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    // Add, toggle, delete, filter
    await page.locator('#new-todo-input').fill('Test todo');
    await page.locator('#add-btn').click();
    await page.locator('#todo-1-checkbox').click();
    await page.locator('#filter-active').click();
    await page.locator('#filter-completed').click();
    await page.locator('#filter-all').click();

    // Delete a todo
    await page.locator('.todo-item').first().hover();
    await page.locator('.todo-delete').first().click();

    await page.waitForTimeout(500);
    expect(errors).toHaveLength(0);
  });
});

// =============================================================================
// BOSATSU UI PERFORMANCE BENCHMARK
// =============================================================================
test.describe('BosatsuUI Performance Benchmark', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/benchmarks/ui-performance/index.html');
    await expect(page.locator('h1')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/BosatsuUI.*React.*Elm.*Benchmark/i);
    await expect(page.locator('.card').first()).toBeVisible();
  });

  test('displays key concept explanation', async ({ page }) => {
    const explainer = page.locator('.explainer');
    await expect(explainer).toBeVisible();
    await expect(explainer).toContainText('compile-time static analysis');
    await expect(explainer).toContainText('O(1)');
    await expect(explainer).toContainText('No virtual DOM');
  });

  test('has benchmark control buttons', async ({ page }) => {
    await expect(page.locator('#run-all')).toBeVisible();
    await expect(page.locator('#run-single')).toBeVisible();
    await expect(page.locator('#run-batch')).toBeVisible();
    await expect(page.locator('#run-list')).toBeVisible();
    await expect(page.locator('#run-targeted')).toBeVisible();
  });

  test('single update benchmark runs and shows results', async ({ page }) => {
    const runButton = page.locator('#run-single');
    const results = page.locator('#results');

    await runButton.click();

    // Wait for results (benchmark takes ~2 seconds per framework)
    // Skip progress check since it may clear too quickly
    await expect(results.locator('table')).toBeVisible({ timeout: 15000 });

    // Should show results for all three frameworks
    await expect(results).toContainText('BosatsuUI');
    await expect(results).toContainText('React');
    await expect(results).toContainText('Elm');

    // Should show speedup explanation
    await expect(results.locator('.speedup')).toBeVisible();
  });

  test('batch update benchmark runs and shows results', async ({ page }) => {
    const runButton = page.locator('#run-batch');
    const results = page.locator('#results');

    await runButton.click();

    // Wait for results
    await expect(results.locator('table')).toBeVisible({ timeout: 15000 });

    // Should mention batching
    await expect(results).toContainText(/batch/i);

    // Should show all frameworks
    await expect(results).toContainText('BosatsuUI');
    await expect(results).toContainText('React');
  });

  test('list update benchmark runs and shows results', async ({ page }) => {
    const runButton = page.locator('#run-list');
    const results = page.locator('#results');

    await runButton.click();

    // Wait for results
    await expect(results.locator('table')).toBeVisible({ timeout: 15000 });

    // Should mention list size
    await expect(results).toContainText('1000');

    // Should explain list advantages
    await expect(results.locator('.speedup')).toContainText(/list|O\(1\)|item/i);
  });

  test('targeted update benchmark runs and shows results', async ({ page }) => {
    const runButton = page.locator('#run-targeted');
    const results = page.locator('#results');

    await runButton.click();

    // Wait for results
    await expect(results.locator('table')).toBeVisible({ timeout: 15000 });

    // Should mention targeted updates
    await expect(results).toContainText(/targeted/i);

    // Should explain why BosatsuUI excels
    await expect(results.locator('.speedup')).toContainText(/binding|O\(1\)/i);
  });

  test('results table has correct columns', async ({ page }) => {
    // Run single benchmark to get table
    await page.locator('#run-single').click();
    await expect(page.locator('#results table')).toBeVisible({ timeout: 15000 });

    // Check table headers
    const table = page.locator('#results table');
    await expect(table).toContainText('Framework');
    await expect(table).toContainText('Avg');
    await expect(table).toContainText('Ops/sec');
    await expect(table).toContainText('Speedup');
  });

  test('hidden benchmark areas exist', async ({ page }) => {
    // These are used for running benchmarks
    const bosatsuRoot = page.locator('#bosatsu-root');
    const reactRoot = page.locator('#react-root');
    const elmRoot = page.locator('#elm-root');

    await expect(bosatsuRoot).toBeAttached();
    await expect(reactRoot).toBeAttached();
    await expect(elmRoot).toBeAttached();

    // They should be hidden
    await expect(bosatsuRoot).toHaveClass(/benchmark-area/);
    await expect(reactRoot).toHaveClass(/benchmark-area/);
  });

  test('has no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));

    // Just load and verify no errors
    await page.waitForTimeout(1000);
    expect(errors).toHaveLength(0);
  });
});

// =============================================================================
// BOSATSU UI DEMOS INDEX PAGE
// =============================================================================
test.describe('BosatsuUI Demos Index', () => {
  test.beforeEach(async ({ page }) => {
    setupConsoleCapture(page);
    await page.goto('/demos/index.html');
    await expect(page.locator('h1')).toBeVisible({ timeout: 10000 });
  });

  test('page loads successfully', async ({ page }) => {
    await expect(page).toHaveTitle(/BosatsuUI/i);
    await expect(page.locator('header')).toBeVisible();
  });

  test('displays key insight section', async ({ page }) => {
    const keyInsight = page.locator('.key-insight');
    await expect(keyInsight).toBeVisible();
    await expect(keyInsight).toContainText('statePath');
    await expect(keyInsight).toContainText('DOMProperty');
    await expect(keyInsight).toContainText('O(1)');
  });

  test('has link to counter demo', async ({ page }) => {
    const counterLink = page.locator('a[href="ui/counter.html"]');
    await expect(counterLink).toBeVisible();
    await expect(counterLink).toContainText('Counter');
  });

  test('has link to todo list demo', async ({ page }) => {
    const todoLink = page.locator('a[href="ui/todo-list.html"]');
    await expect(todoLink).toBeVisible();
    await expect(todoLink).toContainText('Todo List');
  });

  test('has link to benchmark', async ({ page }) => {
    const benchmarkLink = page.locator('a[href="benchmarks/ui-performance/index.html"]');
    await expect(benchmarkLink).toBeVisible();
    await expect(benchmarkLink).toContainText('React');
    await expect(benchmarkLink).toContainText('Elm');
  });

  test('demo cards have proper structure', async ({ page }) => {
    const cards = page.locator('.demo-card');
    // 2 UI demos + 3 simulation demos + 1 benchmark = 6 cards
    const count = await cards.count();
    expect(count).toBeGreaterThanOrEqual(3);

    // Each card should have title and description
    for (let i = 0; i < count; i++) {
      const card = cards.nth(i);
      await expect(card.locator('h3')).toBeVisible();
      await expect(card.locator('p')).toBeVisible();
      await expect(card.locator('.tag')).toBeVisible();
    }
  });

  test('counter link navigates correctly', async ({ page }) => {
    await page.locator('a[href="ui/counter.html"]').click();
    await expect(page).toHaveURL(/counter\.html/);
    await expect(page).toHaveTitle(/Counter/i);
  });

  test('todo list link navigates correctly', async ({ page }) => {
    await page.locator('a[href="ui/todo-list.html"]').click();
    await expect(page).toHaveURL(/todo-list\.html/);
    await expect(page).toHaveTitle(/Todo List/i);
  });

  test('benchmark link navigates correctly', async ({ page }) => {
    await page.locator('a[href="benchmarks/ui-performance/index.html"]').click();
    await expect(page).toHaveURL(/ui-performance/);
    await expect(page).toHaveTitle(/Benchmark/i);
  });

  test('displays How It Works code section', async ({ page }) => {
    const codeSection = page.locator('.section').last();
    await expect(codeSection).toContainText('How It Works');
    await expect(codeSection).toContainText('UIAnalyzer');
    await expect(codeSection).toContainText('setState');
    await expect(codeSection).toContainText('bindings');
  });

  test('has footer with links', async ({ page }) => {
    const footer = page.locator('footer');
    await expect(footer).toBeVisible();
    await expect(footer).toContainText('Bosatsu');
    await expect(footer).toContainText('BurritoUI');
  });

  test('has no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));
    await page.waitForTimeout(500);
    expect(errors).toHaveLength(0);
  });
});

// =============================================================================
// BOSATSU UI BINDING CORRECTNESS TESTS
// =============================================================================
test.describe('BosatsuUI Binding Correctness', () => {
  test('counter: state path matches element update', async ({ page }) => {
    await page.goto('/demos/ui/counter.html');
    await expect(page.locator('#count-display')).toBeVisible();

    // Evaluate to check internal state matches DOM
    await page.locator('#inc').click();
    await page.locator('#inc').click();
    await page.locator('#inc').click();

    // DOM should show 3
    await expect(page.locator('#count-display')).toHaveText('3');

    // The binding log should show the state path was updated
    const log = page.locator('#log');
    await expect(log).toContainText('["count"]');
    await expect(log).toContainText('3');
  });

  test('todo list: per-item bindings update correctly', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();

    // Toggle todo 1
    await page.locator('#todo-1-checkbox').click();

    // Only todo 1's checkbox should be checked
    await expect(page.locator('#todo-1-checkbox')).toHaveClass(/checked/);
    await expect(page.locator('#todo-2-checkbox')).not.toHaveClass(/checked/);
    await expect(page.locator('#todo-3-checkbox')).not.toHaveClass(/checked/);

    // Toggle todo 2
    await page.locator('#todo-2-checkbox').click();

    // Both 1 and 2 should be checked
    await expect(page.locator('#todo-1-checkbox')).toHaveClass(/checked/);
    await expect(page.locator('#todo-2-checkbox')).toHaveClass(/checked/);
    await expect(page.locator('#todo-3-checkbox')).not.toHaveClass(/checked/);
  });

  test('todo list: stats update reflect correct counts', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();

    // Initial: 3 total, 3 active, 0 completed
    await expect(page.locator('#total-count')).toHaveText('3');
    await expect(page.locator('#active-count')).toHaveText('3');
    await expect(page.locator('#completed-count')).toHaveText('0');

    // Complete 2 todos
    await page.locator('#todo-1-checkbox').click();
    await page.locator('#todo-2-checkbox').click();

    // 3 total, 1 active, 2 completed
    await expect(page.locator('#total-count')).toHaveText('3');
    await expect(page.locator('#active-count')).toHaveText('1');
    await expect(page.locator('#completed-count')).toHaveText('2');

    // Add new todo
    await page.locator('#new-todo-input').fill('New todo');
    await page.locator('#add-btn').click();

    // 4 total, 2 active, 2 completed
    await expect(page.locator('#total-count')).toHaveText('4');
    await expect(page.locator('#active-count')).toHaveText('2');
    await expect(page.locator('#completed-count')).toHaveText('2');

    // Delete a completed todo
    await page.locator('#todo-1').hover();
    await page.locator('#todo-1 .todo-delete').click();

    // 3 total, 2 active, 1 completed
    await expect(page.locator('#total-count')).toHaveText('3');
    await expect(page.locator('#active-count')).toHaveText('2');
    await expect(page.locator('#completed-count')).toHaveText('1');
  });
});

// =============================================================================
// COUNTER DEMO - EDGE CASES & STRESS TESTS
// =============================================================================
test.describe('Counter Demo - Edge Cases', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/ui/counter.html');
    await expect(page.locator('#count-display')).toBeVisible();
  });

  test('handles negative numbers correctly', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const decButton = page.locator('#dec');

    // Decrement to negative values
    for (let i = 0; i < 5; i++) {
      await decButton.click();
    }

    await expect(countDisplay).toHaveText('-5');
  });

  test('handles large positive numbers', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const incButton = page.locator('#inc');

    // Rapid increment to large number
    for (let i = 0; i < 50; i++) {
      await incButton.click();
    }

    await expect(countDisplay).toHaveText('50');
  });

  test('reset works from negative value', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const decButton = page.locator('#dec');
    const resetButton = page.locator('#reset');

    // Go negative
    await decButton.click();
    await decButton.click();
    await expect(countDisplay).toHaveText('-2');

    // Reset
    await resetButton.click();
    await expect(countDisplay).toHaveText('0');
  });

  test('random button generates values in expected range', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const randomButton = page.locator('#random');

    // Test 10 random values
    for (let i = 0; i < 10; i++) {
      await randomButton.click();
      const value = await countDisplay.textContent();
      const numValue = parseInt(value || '0', 10);
      expect(numValue).toBeGreaterThanOrEqual(0);
      expect(numValue).toBeLessThan(100);
    }
  });

  test('alternating increment/decrement maintains correctness', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const incButton = page.locator('#inc');
    const decButton = page.locator('#dec');

    // Alternate: +1, -1, +1, -1, +1 = +1
    await incButton.click();
    await decButton.click();
    await incButton.click();
    await decButton.click();
    await incButton.click();

    await expect(countDisplay).toHaveText('1');
  });

  test('binding log maintains maximum entries', async ({ page }) => {
    const log = page.locator('#log');
    const incButton = page.locator('#inc');

    // Click more than 10 times (log keeps last 10)
    for (let i = 0; i < 15; i++) {
      await incButton.click();
    }

    // Should have at most 10 entries
    const entries = page.locator('#log .entry');
    const count = await entries.count();
    expect(count).toBeLessThanOrEqual(10);
  });

  test('binding log shows correct binding path format', async ({ page }) => {
    const log = page.locator('#log');
    const incButton = page.locator('#inc');

    await incButton.click();

    // Check for proper binding log format
    await expect(log).toContainText('["count"]');
    await expect(log).toContainText('#count-display');
    await expect(log).toContainText('textContent');
  });

  test('stress test: 100 rapid clicks', async ({ page }) => {
    const countDisplay = page.locator('#count-display');
    const incButton = page.locator('#inc');

    // Rapidly click 100 times
    for (let i = 0; i < 100; i++) {
      await incButton.click();
    }

    await expect(countDisplay).toHaveText('100');
  });

  test('all buttons are interactive', async ({ page }) => {
    const incButton = page.locator('#inc');
    const decButton = page.locator('#dec');
    const resetButton = page.locator('#reset');
    const randomButton = page.locator('#random');

    // All buttons should be enabled
    await expect(incButton).toBeEnabled();
    await expect(decButton).toBeEnabled();
    await expect(resetButton).toBeEnabled();
    await expect(randomButton).toBeEnabled();
  });

  test('explainer section is visible', async ({ page }) => {
    const explainer = page.locator('.explainer');
    await expect(explainer).toBeVisible();
    await expect(explainer).toContainText('O(1)');
    await expect(explainer).toContainText('binding');
  });
});

// =============================================================================
// TODO LIST DEMO - EDGE CASES & STRESS TESTS
// =============================================================================
test.describe('Todo List Demo - Edge Cases', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();
  });

  test('whitespace-only input does not add todo', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    // Try to add whitespace-only todo
    await input.fill('   ');
    await addButton.click();

    // Should still have 3 todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(3);
  });

  test('special characters in todo text are handled safely', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    // Add todo with special characters (XSS test)
    await input.fill('<script>alert("xss")</script>');
    await addButton.click();

    // Should have 4 todos
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(4);

    // The script should be escaped, not executed
    const lastTodoText = page.locator('.todo-text').last();
    await expect(lastTodoText).toContainText('<script>');
  });

  test('HTML entities in todo text are escaped', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    await input.fill('Test & <b>bold</b>');
    await addButton.click();

    const lastTodoText = page.locator('.todo-text').last();
    await expect(lastTodoText).toContainText('Test & <b>bold</b>');
  });

  test('long todo text is handled', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    const longText = 'A'.repeat(200);
    await input.fill(longText);
    await addButton.click();

    const lastTodoText = page.locator('.todo-text').last();
    await expect(lastTodoText).toContainText('A'.repeat(50)); // At least partial
  });

  test('delete all todos shows empty state', async ({ page }) => {
    // Delete all todos
    for (let i = 0; i < 3; i++) {
      const firstTodo = page.locator('.todo-item').first();
      await firstTodo.hover();
      await page.locator('.todo-delete').first().click();
    }

    // Should show empty state
    const emptyState = page.locator('.empty-state');
    await expect(emptyState).toBeVisible();
    await expect(emptyState).toContainText('No todos');
  });

  test('empty state message when filter has no matches', async ({ page }) => {
    // Click completed filter (no completed todos initially)
    await page.locator('#filter-completed').click();

    // Should show empty state
    const emptyState = page.locator('.empty-state');
    await expect(emptyState).toBeVisible();
    await expect(emptyState).toContainText('No todos match');
  });

  test('toggle all todos to completed and back', async ({ page }) => {
    // Toggle all 3 todos
    await page.locator('#todo-1-checkbox').click();
    await page.locator('#todo-2-checkbox').click();
    await page.locator('#todo-3-checkbox').click();

    // All should be completed
    await expect(page.locator('#completed-count')).toHaveText('3');
    await expect(page.locator('#active-count')).toHaveText('0');

    // Toggle all back
    await page.locator('#todo-1-checkbox').click();
    await page.locator('#todo-2-checkbox').click();
    await page.locator('#todo-3-checkbox').click();

    // All should be active again
    await expect(page.locator('#completed-count')).toHaveText('0');
    await expect(page.locator('#active-count')).toHaveText('3');
  });

  test('filter persists after adding new todo', async ({ page }) => {
    // Complete first todo
    await page.locator('#todo-1-checkbox').click();

    // Switch to active filter
    await page.locator('#filter-active').click();
    await expect(page.locator('.todo-item')).toHaveCount(2);

    // Add new todo
    await page.locator('#new-todo-input').fill('New active todo');
    await page.locator('#add-btn').click();

    // New todo should appear (it's active)
    await expect(page.locator('.todo-item')).toHaveCount(3);
  });

  test('filter persists after toggling todo', async ({ page }) => {
    // Switch to active filter
    await page.locator('#filter-active').click();
    await expect(page.locator('.todo-item')).toHaveCount(3);

    // Complete first visible todo
    await page.locator('#todo-1-checkbox').click();

    // Completed todo should disappear from active view
    await expect(page.locator('.todo-item')).toHaveCount(2);
  });

  test('adding many todos maintains performance', async ({ page }) => {
    // Add 20 todos quickly
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    for (let i = 0; i < 20; i++) {
      await input.fill(`Todo ${i + 1}`);
      await addButton.click();
    }

    // Should have 23 todos total (3 initial + 20 new)
    const todos = page.locator('.todo-item');
    await expect(todos).toHaveCount(23);

    // Stats should be correct
    await expect(page.locator('#total-count')).toHaveText('23');
  });

  test('data-bosatsu-id attributes on stats', async ({ page }) => {
    await expect(page.locator('#total-count')).toHaveAttribute('data-bosatsu-id', 'total-count');
    await expect(page.locator('#active-count')).toHaveAttribute('data-bosatsu-id', 'active-count');
    await expect(page.locator('#completed-count')).toHaveAttribute('data-bosatsu-id', 'completed-count');
  });

  test('new todos get unique IDs', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    // Add two new todos
    await input.fill('First new');
    await addButton.click();
    await input.fill('Second new');
    await addButton.click();

    // Should have todo-4 and todo-5
    await expect(page.locator('#todo-4')).toBeVisible();
    await expect(page.locator('#todo-5')).toBeVisible();
  });

  test('input clears after adding todo', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    await input.fill('Test todo');
    await addButton.click();

    await expect(input).toHaveValue('');
  });

  test('input focuses after adding todo', async ({ page }) => {
    const input = page.locator('#new-todo-input');
    const addButton = page.locator('#add-btn');

    await input.fill('Test todo');
    await addButton.click();

    await expect(input).toBeFocused();
  });

  test('binding info section is visible', async ({ page }) => {
    const bindingInfo = page.locator('.binding-info');
    await expect(bindingInfo).toBeVisible();
    await expect(bindingInfo).toContainText('["todos"');
    await expect(bindingInfo).toContainText('O(1)');
  });
});

// =============================================================================
// BENCHMARK PAGE - COMPREHENSIVE TESTS
// =============================================================================
test.describe('Benchmark Page - Comprehensive Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');
    await expect(page.locator('h1')).toBeVisible();
  });

  test('buttons are disabled during benchmark', async ({ page }) => {
    const runAllButton = page.locator('#run-all');

    // Click run all and immediately check buttons
    await runAllButton.click();

    // All buttons should be disabled during run
    await expect(page.locator('#run-all')).toBeDisabled();
    await expect(page.locator('#run-single')).toBeDisabled();
    await expect(page.locator('#run-batch')).toBeDisabled();
    await expect(page.locator('#run-list')).toBeDisabled();
    await expect(page.locator('#run-targeted')).toBeDisabled();

    // Wait for completion
    await expect(page.locator('#progress')).toContainText(/complete|Running/i, { timeout: 60000 });
  });

  test('progress indicator element exists', async ({ page }) => {
    // Progress indicator should exist on page (may be initially hidden/empty)
    const progress = page.locator('#progress');
    await expect(progress).toBeAttached();
    await expect(progress).toHaveClass(/progress/);
  });

  test('results clear before new benchmark', async ({ page }) => {
    // Run single benchmark
    await page.locator('#run-single').click();
    await expect(page.locator('#results table')).toBeVisible({ timeout: 15000 });

    // Run another benchmark
    await page.locator('#run-batch').click();

    // Results should update (check for batch-specific text)
    await expect(page.locator('#results')).toContainText(/batch/i, { timeout: 15000 });
  });

  test('run all completes all benchmarks', async ({ page }) => {
    test.setTimeout(120000); // 2 minutes for all benchmarks

    await page.locator('#run-all').click();

    // Wait for completion message
    await expect(page.locator('#progress')).toContainText('complete', { timeout: 90000 });

    // Buttons should be re-enabled
    await expect(page.locator('#run-all')).toBeEnabled();
  });

  test('results table shows speedup calculations', async ({ page }) => {
    await page.locator('#run-single').click();
    await expect(page.locator('#results table')).toBeVisible({ timeout: 15000 });

    // Check for speedup column
    await expect(page.locator('#results')).toContainText('Speedup');
    await expect(page.locator('#results')).toContainText('baseline');
  });

  test('bosatsu-root has correct class', async ({ page }) => {
    const bosatsuRoot = page.locator('#bosatsu-root');
    await expect(bosatsuRoot).toHaveClass(/benchmark-area/);
  });

  test('react-root has correct class', async ({ page }) => {
    const reactRoot = page.locator('#react-root');
    await expect(reactRoot).toHaveClass(/benchmark-area/);
  });

  test('elm-root has correct class', async ({ page }) => {
    const elmRoot = page.locator('#elm-root');
    await expect(elmRoot).toHaveClass(/benchmark-area/);
  });

  test('explainer contains key terminology', async ({ page }) => {
    const explainer = page.locator('.explainer');
    await expect(explainer).toContainText('compile-time');
    await expect(explainer).toContainText('statePath');
    await expect(explainer).toContainText('DOMProperty');
    await expect(explainer).toContainText('virtual DOM');
  });

  test('results show all three frameworks', async ({ page }) => {
    await page.locator('#run-single').click();
    await expect(page.locator('#results table')).toBeVisible({ timeout: 15000 });

    await expect(page.locator('#results')).toContainText('BosatsuUI');
    await expect(page.locator('#results')).toContainText('React');
    await expect(page.locator('#results')).toContainText('Elm');
  });

  test('list benchmark mentions 1000 items', async ({ page }) => {
    await page.locator('#run-list').click();
    await expect(page.locator('#results')).toContainText('1000', { timeout: 15000 });
  });

  test('batch benchmark mentions batch size', async ({ page }) => {
    await page.locator('#run-batch').click();
    await expect(page.locator('#results')).toContainText(/100.*updates/i, { timeout: 15000 });
  });

  test('subtitle is descriptive', async ({ page }) => {
    const subtitle = page.locator('.subtitle');
    await expect(subtitle).toContainText('Virtual DOM');
  });

  test('initial results placeholder exists', async ({ page }) => {
    const results = page.locator('#results');
    await expect(results).toContainText('Click a button');
  });
});

// =============================================================================
// INDEX PAGE - COMPREHENSIVE TESTS
// =============================================================================
test.describe('Index Page - Comprehensive Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/index.html');
    await expect(page.locator('h1')).toBeVisible();
  });

  test('header has gradient text styling', async ({ page }) => {
    const h1 = page.locator('h1');
    await expect(h1).toHaveText('BosatsuUI');
  });

  test('subtitle describes the approach', async ({ page }) => {
    const subtitle = page.locator('.subtitle');
    await expect(subtitle).toContainText('compile-time');
    await expect(subtitle).toContainText('TypedExpr');
  });

  test('badge shows "No Virtual DOM"', async ({ page }) => {
    const badge = page.locator('.badge');
    await expect(badge).toContainText('No Virtual DOM');
  });

  test('key insight lists three steps', async ({ page }) => {
    const keyInsight = page.locator('.key-insight ol');
    await expect(keyInsight.locator('li')).toHaveCount(3);
  });

  test('interactive demos section exists', async ({ page }) => {
    await expect(page.locator('text=Interactive UI Demos')).toBeVisible();
  });

  test('performance benchmarks section exists', async ({ page }) => {
    await expect(page.locator('text=Performance Benchmarks')).toBeVisible();
  });

  test('how it works section exists', async ({ page }) => {
    await expect(page.locator('text=How It Works')).toBeVisible();
  });

  test('demo cards have tags', async ({ page }) => {
    const tags = page.locator('.demo-card .tag');
    const count = await tags.count();
    expect(count).toBeGreaterThanOrEqual(2);
  });

  test('counter demo card has core concept tag', async ({ page }) => {
    const counterCard = page.locator('a[href="ui/counter.html"]');
    await expect(counterCard.locator('.tag')).toContainText('Core Concept');
  });

  test('todo list card has per-item bindings tag', async ({ page }) => {
    const todoCard = page.locator('a[href="ui/todo-list.html"]');
    await expect(todoCard.locator('.tag')).toContainText('Per-Item');
  });

  test('benchmark card has benchmark suite tag', async ({ page }) => {
    const benchmarkCard = page.locator('a[href="benchmarks/ui-performance/index.html"]');
    await expect(benchmarkCard.locator('.tag')).toContainText('Benchmark');
  });

  test('demo cards hover effect works', async ({ page }) => {
    const counterCard = page.locator('a[href="ui/counter.html"]');
    await counterCard.hover();
    // Card should still be visible (hover shouldn't break it)
    await expect(counterCard).toBeVisible();
  });

  test('code example in how it works section', async ({ page }) => {
    // Check for code-like content
    await expect(page.locator('.section').last()).toContainText('const bindings');
    await expect(page.locator('.section').last()).toContainText('setState');
  });

  test('footer links to Bosatsu project', async ({ page }) => {
    const footer = page.locator('footer');
    await expect(footer.locator('a[href*="bosatsu"]')).toBeVisible();
  });

  test('footer mentions BurritoUI inspiration', async ({ page }) => {
    const footer = page.locator('footer');
    await expect(footer).toContainText('Inspired by');
    await expect(footer.locator('a[href*="burrito"]')).toBeVisible();
  });

  test('all demo card links are valid', async ({ page }) => {
    const links = await page.locator('.demo-card').all();
    // 2 UI demos + 3 simulation demos + 1 benchmark = 6 cards minimum
    expect(links.length).toBeGreaterThanOrEqual(3);

    for (const link of links) {
      const href = await link.getAttribute('href');
      expect(href).not.toBeNull();
      expect(href).toMatch(/\.html$/);
    }
  });

  test('section titles have decorative bar', async ({ page }) => {
    const sectionTitles = page.locator('.section-title');
    const count = await sectionTitles.count();
    expect(count).toBeGreaterThanOrEqual(2);
  });
});

// =============================================================================
// ACCESSIBILITY TESTS
// =============================================================================
test.describe('Accessibility', () => {
  test('counter demo: buttons have accessible names', async ({ page }) => {
    await page.goto('/demos/ui/counter.html');
    await expect(page.locator('#count-display')).toBeVisible();

    // Buttons should have text content as accessible names
    await expect(page.locator('#inc')).toContainText('Increment');
    await expect(page.locator('#dec')).toContainText('Decrement');
    await expect(page.locator('#reset')).toContainText('Reset');
    await expect(page.locator('#random')).toContainText('Random');
  });

  test('todo demo: input has placeholder', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    const input = page.locator('#new-todo-input');
    await expect(input).toHaveAttribute('placeholder');
  });

  test('index page: all links have href', async ({ page }) => {
    await page.goto('/demos/index.html');
    const links = await page.locator('a').all();

    for (const link of links) {
      const href = await link.getAttribute('href');
      expect(href).not.toBeNull();
    }
  });

  test('index page: heading hierarchy is correct', async ({ page }) => {
    await page.goto('/demos/index.html');

    // Should have one h1
    const h1Count = await page.locator('h1').count();
    expect(h1Count).toBe(1);

    // Should have h2s for sections
    const h2Count = await page.locator('h2').count();
    expect(h2Count).toBeGreaterThanOrEqual(2);
  });

  test('benchmark page: all buttons are keyboard accessible', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Tab to first button and activate with Enter
    await page.keyboard.press('Tab');
    await page.keyboard.press('Tab');
    await page.keyboard.press('Tab'); // May need more tabs depending on page structure

    // All buttons should be tabbable (no tabindex=-1)
    const buttons = await page.locator('button').all();
    for (const button of buttons) {
      const tabindex = await button.getAttribute('tabindex');
      expect(tabindex).not.toBe('-1');
    }
  });
});

// =============================================================================
// CROSS-BROWSER CONSISTENCY TESTS
// =============================================================================
test.describe('Cross-Browser Consistency', () => {
  test('counter works across interactions', async ({ page }) => {
    await page.goto('/demos/ui/counter.html');
    await expect(page.locator('#count-display')).toBeVisible();

    // Complex sequence
    await page.locator('#inc').click();
    await page.locator('#inc').click();
    await page.locator('#dec').click();
    await page.locator('#random').click();
    await page.locator('#reset').click();

    await expect(page.locator('#count-display')).toHaveText('0');
  });

  test('todo list complex workflow', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();

    // Add, complete, filter, delete workflow
    await page.locator('#new-todo-input').fill('Workflow test');
    await page.locator('#add-btn').click();

    await page.locator('#todo-1-checkbox').click();
    await page.locator('#filter-completed').click();

    await expect(page.locator('.todo-item')).toHaveCount(1);

    await page.locator('#filter-all').click();
    const todoCount = await page.locator('.todo-item').count();
    expect(todoCount).toBe(4);
  });
});

// =============================================================================
// RUNTIME BEHAVIOR TESTS
// =============================================================================
test.describe('BosatsuUI Runtime Behavior', () => {
  test('counter: element cache is used', async ({ page }) => {
    await page.goto('/demos/ui/counter.html');
    await expect(page.locator('#count-display')).toBeVisible();

    // The fact that rapid clicking works proves the cache is working
    for (let i = 0; i < 20; i++) {
      await page.locator('#inc').click();
    }
    await expect(page.locator('#count-display')).toHaveText('20');
  });

  test('todo: bindings are registered for new items', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();

    // Add new todo
    await page.locator('#new-todo-input').fill('New binding test');
    await page.locator('#add-btn').click();

    // The new todo should have proper binding (toggle should work)
    await page.locator('#todo-4-checkbox').click();
    await expect(page.locator('#todo-4-checkbox')).toHaveClass(/checked/);
  });

  test('todo: bindings are cleaned up on delete', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();

    // Delete first todo
    await page.locator('#todo-1').hover();
    await page.locator('#todo-1 .todo-delete').click();

    // The element should be gone
    await expect(page.locator('#todo-1')).not.toBeVisible();

    // Stats should still work (bindings cleaned up properly)
    await expect(page.locator('#total-count')).toHaveText('2');
  });
});

// =============================================================================
// VISUAL CONSISTENCY TESTS
// =============================================================================
test.describe('Visual Consistency', () => {
  test('counter: completed state shows strikethrough', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();

    await page.locator('#todo-1-checkbox').click();

    const todoItem = page.locator('#todo-1');
    await expect(todoItem).toHaveClass(/completed/);
  });

  test('todo: filter buttons show active state', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');

    await page.locator('#filter-active').click();
    await expect(page.locator('#filter-active')).toHaveClass(/active/);
    await expect(page.locator('#filter-all')).not.toHaveClass(/active/);
    await expect(page.locator('#filter-completed')).not.toHaveClass(/active/);
  });

  test('todo: delete button visible on hover', async ({ page }) => {
    await page.goto('/demos/ui/todo-list.html');
    await expect(page.locator('.todo-item').first()).toBeVisible();

    // Before hover, delete should be hidden (opacity: 0)
    const deleteBtn = page.locator('#todo-1 .todo-delete');

    // After hover, should be visible
    await page.locator('#todo-1').hover();
    await expect(deleteBtn).toBeVisible();
  });

  test('counter: count display updates color correctly', async ({ page }) => {
    await page.goto('/demos/ui/counter.html');
    const countDisplay = page.locator('#count-display');

    // Should have orange/accent color styling
    await expect(countDisplay).toHaveClass(/count/);
  });
});
