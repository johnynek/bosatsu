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

  test('single update benchmark completes with real results', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Run single benchmark
    await page.click('#run-single');

    // Wait for benchmark to complete (look for results table)
    await page.waitForSelector('table', { timeout: 30000 });

    // Verify both frameworks produced results
    const results = await page.evaluate(() => {
      const rows = document.querySelectorAll('table tr');
      const data: { framework: string; avgMs: number; opsPerSec: number }[] = [];

      rows.forEach(row => {
        const cells = row.querySelectorAll('td');
        if (cells.length >= 3) {
          const framework = cells[0]?.textContent || '';
          const avgMs = parseFloat(cells[1]?.textContent || '0');
          const opsPerSec = parseInt(cells[2]?.textContent?.replace(/,/g, '') || '0', 10);
          if (framework) {
            data.push({ framework, avgMs, opsPerSec });
          }
        }
      });

      return data;
    });

    // Verify both BosatsuUI and React have results
    const bosatsuResult = results.find(r => r.framework.includes('BosatsuUI'));
    const reactResult = results.find(r => r.framework.includes('React'));

    expect(bosatsuResult, 'BosatsuUI result must exist').toBeDefined();
    expect(reactResult, 'React result must exist').toBeDefined();
    expect(bosatsuResult!.opsPerSec, 'BosatsuUI must have completed operations').toBeGreaterThan(1000);
    expect(reactResult!.opsPerSec, 'React must have completed operations').toBeGreaterThan(1000);
    expect(bosatsuResult!.avgMs, 'BosatsuUI avg time must be positive').toBeGreaterThan(0);
    expect(reactResult!.avgMs, 'React avg time must be positive').toBeGreaterThan(0);
  });

  test('list benchmark completes with real results', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Run list benchmark
    await page.click('#run-list');

    // Wait for benchmark to complete
    await page.waitForSelector('table', { timeout: 60000 });

    // Verify results table mentions the list size
    const tableHtml = await page.locator('table').first().innerHTML();
    expect(tableHtml).toContain('100');

    // Verify both frameworks produced results
    const results = await page.evaluate(() => {
      const rows = document.querySelectorAll('table tr');
      const data: { framework: string; opsPerSec: number }[] = [];

      rows.forEach(row => {
        const cells = row.querySelectorAll('td');
        if (cells.length >= 3) {
          const framework = cells[0]?.textContent || '';
          const opsPerSec = parseInt(cells[2]?.textContent?.replace(/,/g, '') || '0', 10);
          if (framework) {
            data.push({ framework, opsPerSec });
          }
        }
      });

      return data;
    });

    expect(results.length, 'Should have results for both frameworks').toBe(2);
    results.forEach(r => {
      expect(r.opsPerSec, `${r.framework} must have completed operations`).toBeGreaterThan(1000);
    });
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

  test('targeted update benchmark completes with real results', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Run targeted benchmark
    await page.click('#run-targeted');
    await page.waitForSelector('table', { timeout: 30000 });

    // Verify both frameworks produced results
    const results = await page.evaluate(() => {
      const rows = document.querySelectorAll('table tr');
      const data: { framework: string; opsPerSec: number }[] = [];

      rows.forEach(row => {
        const cells = row.querySelectorAll('td');
        if (cells.length >= 3) {
          const framework = cells[0]?.textContent || '';
          const opsPerSec = parseInt(cells[2]?.textContent?.replace(/,/g, '') || '0', 10);
          if (framework) {
            data.push({ framework, opsPerSec });
          }
        }
      });

      return data;
    });

    expect(results.length, 'Should have results for both frameworks').toBe(2);
    results.forEach(r => {
      expect(r.opsPerSec, `${r.framework} must have completed operations`).toBeGreaterThan(1000);
    });
  });

  test('warning banner indicates fair benchmark', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Verify there's a warning/note about fair comparison
    const warningText = await page.locator('.warning').textContent();

    expect(warningText).toContain('Fair Benchmark');
    expect(warningText).toContain('actual React 18');
    expect(warningText).toContain('not a simulation');
  });

  test('benchmark uses ReactDOM.createRoot (React 18 API)', async ({ page }) => {
    await page.goto('/demos/benchmarks/ui-performance/index.html');

    // Get page source and verify React 18 API usage
    const pageContent = await page.content();

    // Must use React 18's createRoot
    expect(pageContent).toContain('ReactDOM.createRoot');

    // Must use React hooks
    expect(pageContent).toContain('useState');

    // Must use memo for fair list comparison
    expect(pageContent).toContain('memo');
  });
});
