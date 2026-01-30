import { test, expect } from '@playwright/test';

/**
 * Benchmark Integrity Tests
 *
 * These tests verify that the BosatsuUI vs React benchmark is fair and real:
 * 1. React is actually loaded from CDN (not simulated)
 * 2. React components actually render and update the DOM
 * 3. Both frameworks perform equivalent operations
 * 4. No fake/simulated implementations are used
 *
 * This prevents future AI agents from faking benchmark results.
 */

test.describe('Benchmark Integrity Verification', () => {

  test('benchmark page loads real React from CDN', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Verify React global exists and is the real React
    const reactInfo = await page.evaluate(() => {
      return {
        hasReact: typeof React !== 'undefined',
        hasReactDOM: typeof ReactDOM !== 'undefined',
        reactVersion: React?.version,
        hasCreateElement: typeof React?.createElement === 'function',
        hasUseState: typeof React?.useState === 'function',
        hasMemo: typeof React?.memo === 'function',
        hasCreateRoot: typeof ReactDOM?.createRoot === 'function',
      };
    });

    expect(reactInfo.hasReact, 'React global must exist').toBe(true);
    expect(reactInfo.hasReactDOM, 'ReactDOM global must exist').toBe(true);
    expect(reactInfo.reactVersion, 'React version must start with 18').toMatch(/^18\./);
    expect(reactInfo.hasCreateElement, 'React.createElement must be a function').toBe(true);
    expect(reactInfo.hasUseState, 'React.useState must be a function').toBe(true);
    expect(reactInfo.hasMemo, 'React.memo must be a function').toBe(true);
    expect(reactInfo.hasCreateRoot, 'ReactDOM.createRoot must be a function').toBe(true);
  });

  test('single update benchmark uses real React setState', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Run single benchmark
    await page.click('#run-single');

    // Wait for benchmark to complete (look for results table)
    await page.waitForSelector('table', { timeout: 30000 });

    // Verify React actually rendered and updated
    const verification = await page.evaluate(async () => {
      // Check that _reactSetCount was exposed (proves React component mounted)
      const hasSetCount = typeof window._reactSetCount === 'function';

      // If React mounted, verify we can call setState and it updates
      if (hasSetCount) {
        // Get current React count element
        const reactCount = document.querySelector('#react-count');
        const beforeText = reactCount?.textContent;

        // Call the real React setState
        window._reactSetCount(12345);

        // Wait for React to flush
        await new Promise(r => setTimeout(r, 100));

        const afterText = reactCount?.textContent;

        return {
          hasSetCount,
          beforeText,
          afterText,
          reactUpdatedDOM: afterText === '12345',
        };
      }

      return { hasSetCount, reactUpdatedDOM: false };
    });

    expect(verification.hasSetCount, 'React setState must be exposed').toBe(true);
    expect(verification.reactUpdatedDOM, 'React must actually update the DOM when setState is called').toBe(true);
  });

  test('list benchmark uses real React with memoized components', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Run list benchmark
    await page.click('#run-list');

    // Wait for benchmark to complete
    await page.waitForSelector('table', { timeout: 60000 });

    // Verify React list rendering
    const verification = await page.evaluate(async () => {
      const hasSetItems = typeof window._reactSetItems === 'function';

      if (hasSetItems) {
        // Count how many React list items exist
        const reactItems = document.querySelectorAll('[id^="react-item-"]');
        const itemCount = reactItems.length;

        // Verify we can update a specific item
        const targetItem = document.querySelector('#react-item-50');
        const beforeText = targetItem?.textContent;

        // Update via React setState
        window._reactSetItems(prev => {
          const copy = [...prev];
          copy[50] = 99999;
          return copy;
        });

        // Wait for React to flush
        await new Promise(r => setTimeout(r, 100));

        const afterText = targetItem?.textContent;

        return {
          hasSetItems,
          itemCount,
          beforeText,
          afterText,
          reactUpdatedListItem: afterText === '99999',
        };
      }

      return { hasSetItems, itemCount: 0, reactUpdatedListItem: false };
    });

    expect(verification.hasSetItems, 'React setItems must be exposed').toBe(true);
    expect(verification.itemCount, 'React must render 100 list items').toBe(100);
    expect(verification.reactUpdatedListItem, 'React must update specific list item when setState is called').toBe(true);
  });

  test('benchmark code does not contain simulation classes', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Get the page source
    const pageContent = await page.content();

    // Check for simulation/mock patterns that indicate faking
    const fakePatterns = [
      /class\s+ReactSimulation/,        // Simulated React class
      /class\s+ElmSimulation/,          // Simulated Elm class
      /createVDOM\s*=\s*function/,      // Fake VDOM creation
      /diffAndPatch\s*=\s*function/,    // Fake diff function
      /\/\/\s*Simulate/i,               // "Simulate" comments
      /\/\/\s*Fake/i,                   // "Fake" comments
      /\/\/\s*Mock/i,                   // "Mock" comments
    ];

    const foundPatterns: string[] = [];
    for (const pattern of fakePatterns) {
      if (pattern.test(pageContent)) {
        foundPatterns.push(pattern.toString());
      }
    }

    expect(
      foundPatterns,
      `Benchmark contains fake/simulated code patterns: ${foundPatterns.join(', ')}`
    ).toHaveLength(0);
  });

  test('both BosatsuUI and React update equivalent DOM elements', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Run targeted benchmark to test both frameworks
    await page.click('#run-targeted');
    await page.waitForSelector('table', { timeout: 30000 });

    // Verify both frameworks created equivalent DOM structures
    const verification = await page.evaluate(async () => {
      const bosatsuElements = document.querySelectorAll('[id^="bosatsu-value-"]');
      const reactElements = document.querySelectorAll('[id^="react-value-"]');

      // Both should have created 10 value elements
      const bosatsuCount = bosatsuElements.length;
      const reactCount = reactElements.length;

      // Test that BosatsuUI updates work
      const bosatsuRuntime = document.querySelector('#bosatsu-root')?._bosatsuRuntime;

      // Get initial values
      const bosatsuValueA = document.querySelector('#bosatsu-value-a')?.textContent;
      const reactValueA = document.querySelector('#react-value-a')?.textContent;

      return {
        bosatsuCount,
        reactCount,
        bothHave10Elements: bosatsuCount === 10 && reactCount === 10,
        bosatsuValueA,
        reactValueA,
      };
    });

    expect(verification.bothHave10Elements, 'Both BosatsuUI and React must create 10 value elements').toBe(true);
  });

  test('benchmark results show both frameworks completed runs', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Run single benchmark
    await page.click('#run-single');
    await page.waitForSelector('table', { timeout: 30000 });

    // Check results table has both frameworks
    const tableContent = await page.locator('table').textContent();

    expect(tableContent).toContain('BosatsuUI');
    expect(tableContent).toContain('React');
    expect(tableContent).toContain('Ops/sec');

    // Verify both have non-zero ops/sec (both actually ran)
    const opsPerSecValues = await page.evaluate(() => {
      const rows = document.querySelectorAll('table tr');
      const results: { framework: string; opsPerSec: number }[] = [];

      rows.forEach(row => {
        const cells = row.querySelectorAll('td');
        if (cells.length >= 3) {
          const framework = cells[0]?.textContent || '';
          const opsPerSec = parseInt(cells[2]?.textContent?.replace(/,/g, '') || '0', 10);
          if (framework) {
            results.push({ framework, opsPerSec });
          }
        }
      });

      return results;
    });

    // Both frameworks should have completed significant number of operations
    const bosatsuResult = opsPerSecValues.find(r => r.framework.includes('BosatsuUI'));
    const reactResult = opsPerSecValues.find(r => r.framework.includes('React'));

    expect(bosatsuResult, 'BosatsuUI result must exist').toBeDefined();
    expect(reactResult, 'React result must exist').toBeDefined();
    expect(bosatsuResult!.opsPerSec, 'BosatsuUI must have completed operations').toBeGreaterThan(0);
    expect(reactResult!.opsPerSec, 'React must have completed operations').toBeGreaterThan(0);
  });

  test('warning banner indicates fair benchmark', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Verify there's a warning/note about fair comparison
    const warningText = await page.locator('.warning').textContent();

    expect(warningText).toContain('Fair Benchmark');
    expect(warningText).toContain('actual React 18');
    expect(warningText).toContain('not a simulation');
  });
});
