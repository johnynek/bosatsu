import { test, expect } from '@playwright/test';

/**
 * IO Behavior Family Tests
 *
 * These tests verify the complete IO pipeline:
 *   JsGen IO compilation → runtime _runIO execution → DOM update
 *
 * Each test corresponds to a .bosatsu fixture in demos/test/ that exercises
 * a specific IO pattern. The fixtures are compiled via:
 *   bosatsu-sim ui demos/test/<fixture>.bosatsu -o web_deploy/demos/test/<fixture>.html
 */

test.describe('IO Behavior: Single Write', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/test/io_single_write.html');
    await expect(page.locator('#app')).toBeVisible({ timeout: 5000 });
  });

  test('initial state displays 0', async ({ page }) => {
    await expect(page.locator('#display')).toHaveText('0');
  });

  test('click updates state and DOM', async ({ page }) => {
    await expect(page.locator('#display')).toHaveText('0');
    await page.click('#btn');
    await expect(page.locator('#display')).toHaveText('1');
  });

  test('multiple clicks accumulate', async ({ page }) => {
    const btn = page.locator('#btn');
    await btn.click();
    await btn.click();
    await btn.click();
    await expect(page.locator('#display')).toHaveText('3');
  });

  test('no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));
    await page.click('#btn');
    await page.waitForTimeout(100);
    expect(errors).toHaveLength(0);
  });
});

test.describe('IO Behavior: flatMap Sequencing', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/test/io_flatmap_sequence.html');
    await expect(page.locator('#app')).toBeVisible({ timeout: 5000 });
  });

  test('initial states display underscore', async ({ page }) => {
    await expect(page.locator('#display-a')).toHaveText('_');
    await expect(page.locator('#display-b')).toHaveText('_');
  });

  test('both writes execute in order', async ({ page }) => {
    await page.click('#btn');
    await expect(page.locator('#display-a')).toHaveText('X');
    await expect(page.locator('#display-b')).toHaveText('Y');
  });

  test('no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));
    await page.click('#btn');
    await page.waitForTimeout(100);
    expect(errors).toHaveLength(0);
  });
});

test.describe('IO Behavior: Event Handler (on_input)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/test/io_event_handler.html');
    await expect(page.locator('#app')).toBeVisible({ timeout: 5000 });
  });

  test('initial display is empty', async ({ page }) => {
    await expect(page.locator('#display')).toHaveText('');
  });

  test('typing in input updates display via IO', async ({ page }) => {
    await page.fill('#input', 'hello');
    await expect(page.locator('#display')).toHaveText('hello');
  });

  test('clearing input clears display', async ({ page }) => {
    await page.fill('#input', 'test');
    await expect(page.locator('#display')).toHaveText('test');
    await page.fill('#input', '');
    await expect(page.locator('#display')).toHaveText('');
  });

  test('no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));
    await page.fill('#input', 'test');
    await page.waitForTimeout(100);
    expect(errors).toHaveLength(0);
  });
});

test.describe('IO Behavior: Frame Callback', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/test/io_frame_callback.html');
    await expect(page.locator('#app')).toBeVisible({ timeout: 5000 });
  });

  test('frame callback increments counter over time', async ({ page }) => {
    // Wait for several animation frames
    await page.waitForTimeout(500);
    const text = await page.locator('#counter').textContent();
    const count = parseInt(text || '0', 10);
    // After 500ms at ~60fps, should have at least a few frames
    expect(count).toBeGreaterThan(0);
  });

  test('counter keeps incrementing', async ({ page }) => {
    await page.waitForTimeout(200);
    const text1 = await page.locator('#counter').textContent();
    const count1 = parseInt(text1 || '0', 10);

    await page.waitForTimeout(200);
    const text2 = await page.locator('#counter').textContent();
    const count2 = parseInt(text2 || '0', 10);

    expect(count2).toBeGreaterThan(count1);
  });

  test('no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));
    await page.waitForTimeout(300);
    expect(errors).toHaveLength(0);
  });
});

test.describe('IO Behavior: Read Then Write (flatMap state dependency)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/demos/test/io_read_then_write.html');
    await expect(page.locator('#app')).toBeVisible({ timeout: 5000 });
  });

  test('initial states display 0', async ({ page }) => {
    await expect(page.locator('#counter-display')).toHaveText('0');
    await expect(page.locator('#doubled-display')).toHaveText('0');
  });

  test('click writes counter then reads updated counter for doubled', async ({ page }) => {
    await page.click('#btn');
    // counter: 0 → 1, doubled: read(counter)+read(counter) = 1+1 = 2
    await expect(page.locator('#counter-display')).toHaveText('1');
    await expect(page.locator('#doubled-display')).toHaveText('2');
  });

  test('second click sees first clicks state', async ({ page }) => {
    await page.click('#btn');
    await page.click('#btn');
    // counter: 1 → 2, doubled: 2+2 = 4
    await expect(page.locator('#counter-display')).toHaveText('2');
    await expect(page.locator('#doubled-display')).toHaveText('4');
  });

  test('no console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('pageerror', err => errors.push(err.message));
    await page.click('#btn');
    await page.click('#btn');
    await page.waitForTimeout(100);
    expect(errors).toHaveLength(0);
  });
});
