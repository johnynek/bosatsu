import { test, expect, Page } from '@playwright/test';

test.describe('Drag Performance Benchmark - Reordering', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('file://' + process.cwd() + '/demos/benchmarks/drag-performance/index.html');
    // Wait for React to render
    await page.waitForTimeout(500);
  });

  // Helper to get item titles in order from a container
  async function getItemTitles(page: Page, containerSelector: string): Promise<string[]> {
    return page.$$eval(
      `${containerSelector} .list-item`,
      (items) => items.map((item) => {
        const titleEl = item.querySelector('div[style*="font-weight: bold"]');
        return titleEl?.textContent?.trim() || '';
      })
    );
  }

  // Helper to drag an item to a new position
  async function dragItem(
    page: Page,
    containerSelector: string,
    fromIndex: number,
    toIndex: number
  ) {
    const container = page.locator(containerSelector);
    const items = container.locator('.list-item');

    const fromItem = items.nth(fromIndex);
    const toItem = items.nth(toIndex);

    const fromBox = await fromItem.boundingBox();
    const toBox = await toItem.boundingBox();

    if (!fromBox || !toBox) {
      throw new Error(`Could not get bounding boxes for items ${fromIndex} -> ${toIndex}`);
    }

    // Start drag from center of source item
    const startX = fromBox.x + fromBox.width / 2;
    const startY = fromBox.y + fromBox.height / 2;

    // End drag at center of target item
    const endX = toBox.x + toBox.width / 2;
    const endY = toBox.y + toBox.height / 2;

    await page.mouse.move(startX, startY);
    await page.mouse.down();
    await page.mouse.move(endX, endY, { steps: 10 });
    await page.mouse.up();

    // Wait for any animations/updates
    await page.waitForTimeout(100);
  }

  test.describe('Naive React', () => {
    const container = '#react-naive-container';

    test('should reorder item when dragged down', async ({ page }) => {
      const titlesBefore = await getItemTitles(page, container);
      expect(titlesBefore[0]).toContain('Item 1');
      expect(titlesBefore[1]).toContain('Item 2');
      expect(titlesBefore[2]).toContain('Item 3');

      // Drag item 0 to position 2
      await dragItem(page, container, 0, 2);

      const titlesAfter = await getItemTitles(page, container);
      // Item 1 should now be at index 2, and items 2,3 should shift up
      expect(titlesAfter[0]).toContain('Item 2');
      expect(titlesAfter[1]).toContain('Item 3');
      expect(titlesAfter[2]).toContain('Item 1');
    });

    test('should reorder item when dragged up', async ({ page }) => {
      const titlesBefore = await getItemTitles(page, container);
      expect(titlesBefore[2]).toContain('Item 3');

      // Drag item 2 to position 0
      await dragItem(page, container, 2, 0);

      const titlesAfter = await getItemTitles(page, container);
      // Item 3 should now be at index 0
      expect(titlesAfter[0]).toContain('Item 3');
      expect(titlesAfter[1]).toContain('Item 1');
      expect(titlesAfter[2]).toContain('Item 2');
    });
  });

  test.describe('Optimized React', () => {
    const container = '#react-optimized-container';

    test('should reorder item when dragged down', async ({ page }) => {
      const titlesBefore = await getItemTitles(page, container);
      expect(titlesBefore[0]).toContain('Item 1');
      expect(titlesBefore[1]).toContain('Item 2');
      expect(titlesBefore[2]).toContain('Item 3');

      // Drag item 0 to position 2
      await dragItem(page, container, 0, 2);

      const titlesAfter = await getItemTitles(page, container);
      expect(titlesAfter[0]).toContain('Item 2');
      expect(titlesAfter[1]).toContain('Item 3');
      expect(titlesAfter[2]).toContain('Item 1');
    });

    test('should reorder item when dragged up', async ({ page }) => {
      // Drag item 2 to position 0
      await dragItem(page, container, 2, 0);

      const titlesAfter = await getItemTitles(page, container);
      expect(titlesAfter[0]).toContain('Item 3');
      expect(titlesAfter[1]).toContain('Item 1');
      expect(titlesAfter[2]).toContain('Item 2');
    });
  });

  test.describe('BosatsuUI (Vanilla)', () => {
    const container = '#bosatsu-container';

    test('should reorder item when dragged down', async ({ page }) => {
      const titlesBefore = await getItemTitles(page, container);
      expect(titlesBefore[0]).toContain('Item 1');
      expect(titlesBefore[1]).toContain('Item 2');
      expect(titlesBefore[2]).toContain('Item 3');

      // Drag item 0 to position 2
      await dragItem(page, container, 0, 2);

      const titlesAfter = await getItemTitles(page, container);
      expect(titlesAfter[0]).toContain('Item 2');
      expect(titlesAfter[1]).toContain('Item 3');
      expect(titlesAfter[2]).toContain('Item 1');
    });

    test('should reorder item when dragged up', async ({ page }) => {
      // Drag item 2 to position 0
      await dragItem(page, container, 2, 0);

      const titlesAfter = await getItemTitles(page, container);
      expect(titlesAfter[0]).toContain('Item 3');
      expect(titlesAfter[1]).toContain('Item 1');
      expect(titlesAfter[2]).toContain('Item 2');
    });

    test('should maintain correct order after multiple drags', async ({ page }) => {
      // Drag item 0 to position 4
      await dragItem(page, container, 0, 4);

      let titles = await getItemTitles(page, container);
      expect(titles[0]).toContain('Item 2');
      expect(titles[4]).toContain('Item 1');

      // Now drag item 4 (which is "Item 1") back to position 0
      await dragItem(page, container, 4, 0);

      titles = await getItemTitles(page, container);
      expect(titles[0]).toContain('Item 1');
      expect(titles[1]).toContain('Item 2');
    });
  });

  test.describe('All implementations should produce same results', () => {
    test('dragging first item to third position should be consistent', async ({ page }) => {
      // Perform the same drag on all three implementations
      await dragItem(page, '#react-naive-container', 0, 2);
      await dragItem(page, '#react-optimized-container', 0, 2);
      await dragItem(page, '#bosatsu-container', 0, 2);

      const naiveTitles = await getItemTitles(page, '#react-naive-container');
      const optimizedTitles = await getItemTitles(page, '#react-optimized-container');
      const bosatsuTitles = await getItemTitles(page, '#bosatsu-container');

      // All should have the same order
      expect(naiveTitles.slice(0, 5)).toEqual(optimizedTitles.slice(0, 5));
      expect(naiveTitles.slice(0, 5)).toEqual(bosatsuTitles.slice(0, 5));
    });
  });
});
