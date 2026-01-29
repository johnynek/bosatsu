import { test, expect } from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

// =============================================================================
// META-TEST: Every GitHub Pages demo must have Playwright tests
// =============================================================================

// Parse the demos/index.html to find all demo links
async function extractDemoLinksFromIndex(): Promise<string[]> {
  const indexPath = path.join(__dirname, '../../demos/index.html');
  const content = fs.readFileSync(indexPath, 'utf-8');

  // Find all href attributes in anchor tags that link to demos
  const hrefRegex = /href="([^"]+\.html)"/g;
  const links: string[] = [];
  let match;

  while ((match = hrefRegex.exec(content)) !== null) {
    const href = match[1];
    // Skip external links and the index itself
    if (!href.startsWith('http') && !href.startsWith('#') && href !== 'index.html') {
      links.push(href);
    }
  }

  return links;
}

// Get all spec files and extract which demos they test
async function extractTestedDemos(): Promise<Set<string>> {
  const testsDir = path.join(__dirname);
  const testedDemos = new Set<string>();

  // Read all .spec.ts files
  const specFiles = fs.readdirSync(testsDir).filter(f => f.endsWith('.spec.ts'));

  for (const specFile of specFiles) {
    const content = fs.readFileSync(path.join(testsDir, specFile), 'utf-8');

    // Find all page.goto() calls
    const gotoRegex = /page\.goto\(['"](\/[^'"]+\.html)['"]\)/g;
    let match;

    while ((match = gotoRegex.exec(content)) !== null) {
      testedDemos.add(match[1]);
    }

    // Also look for URLs in test.describe or comments
    const urlRegex = /url:\s*['"]([^'"]+\.html)['"]/g;
    while ((match = urlRegex.exec(content)) !== null) {
      // Normalize to absolute path
      let url = match[1];
      if (!url.startsWith('/')) {
        url = '/' + url;
      }
      testedDemos.add(url);
    }
  }

  return testedDemos;
}

// Normalize demo path to match between index.html and test URLs
function normalizePath(href: string): string {
  // Remove leading ./
  let normalized = href.replace(/^\.\//, '');
  // Add leading /demos/ if it's a relative path
  if (!normalized.startsWith('/')) {
    normalized = '/demos/' + normalized;
  }
  // Ensure leading slash
  if (!normalized.startsWith('/')) {
    normalized = '/' + normalized;
  }
  return normalized;
}

test.describe('Meta: Demo Coverage', () => {
  test('every demo linked from index.html has Playwright tests', async () => {
    const indexLinks = await extractDemoLinksFromIndex();
    const testedDemos = await extractTestedDemos();

    console.log('Demos found in index.html:');
    indexLinks.forEach(l => console.log(`  - ${l}`));

    console.log('\nDemos with Playwright tests:');
    testedDemos.forEach(d => console.log(`  - ${d}`));

    const untestedDemos: string[] = [];

    for (const link of indexLinks) {
      const normalizedLink = normalizePath(link);

      // Check if any test covers this demo
      const isTested = Array.from(testedDemos).some(tested =>
        tested.includes(normalizedLink) ||
        normalizedLink.includes(tested.replace('/demos/', '')) ||
        tested.endsWith(link.replace('./', ''))
      );

      if (!isTested) {
        untestedDemos.push(link);
      }
    }

    if (untestedDemos.length > 0) {
      console.error('\nUntested demos:');
      untestedDemos.forEach(d => console.error(`  - ${d}`));
    }

    // This test fails if any demos are not covered by tests
    expect(
      untestedDemos,
      `The following demos are not covered by Playwright tests: ${untestedDemos.join(', ')}`
    ).toHaveLength(0);
  });

  test('every demo page loads without errors', async ({ page }) => {
    const indexLinks = await extractDemoLinksFromIndex();
    const errors: { demo: string; error: string }[] = [];

    for (const link of indexLinks) {
      const url = `/demos/${link.replace('./', '')}`;

      // Capture any page errors
      const pageErrors: string[] = [];
      page.on('pageerror', err => pageErrors.push(err.message));

      try {
        await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 10000 });

        if (pageErrors.length > 0) {
          errors.push({ demo: link, error: pageErrors.join('; ') });
        }
      } catch (err) {
        errors.push({ demo: link, error: String(err) });
      }
    }

    if (errors.length > 0) {
      console.error('Demos with errors:');
      errors.forEach(e => console.error(`  - ${e.demo}: ${e.error}`));
    }

    expect(errors, 'Some demos have JavaScript errors').toHaveLength(0);
  });

  test('every demo page shows no NaN or undefined values', async ({ page }) => {
    const indexLinks = await extractDemoLinksFromIndex();
    const problemDemos: { demo: string; issue: string }[] = [];

    for (const link of indexLinks) {
      const url = `/demos/${link.replace('./', '')}`;

      try {
        await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 10000 });

        // Check visible result/value elements, not raw body text (which includes JS source)
        const resultValues = page.locator('.result-value, .value-display, [id^="val-"]');
        const count = await resultValues.count();

        for (let i = 0; i < count; i++) {
          const text = await resultValues.nth(i).textContent();
          if (text === 'NaN' || text?.trim() === 'NaN') {
            problemDemos.push({ demo: link, issue: `Contains NaN value: "${text}"` });
          }
          if (text === 'undefined' || text?.trim() === 'undefined') {
            problemDemos.push({ demo: link, issue: `Contains undefined value: "${text}"` });
          }
        }
      } catch (err) {
        problemDemos.push({ demo: link, issue: `Load error: ${String(err)}` });
      }
    }

    if (problemDemos.length > 0) {
      console.error('Demos with NaN/undefined issues:');
      problemDemos.forEach(p => console.error(`  - ${p.demo}: ${p.issue}`));
    }

    expect(problemDemos, 'Some demos show NaN or undefined values').toHaveLength(0);
  });
});

// =============================================================================
// REGISTRY: All demos that should be tested
// Add new demos here when created
// =============================================================================
const DEMO_REGISTRY = {
  // BosatsuUI demos (Phase 6)
  'ui': [
    { path: '/demos/ui/counter.html', name: 'Counter', testFile: 'bosatsu-ui.spec.ts' },
    { path: '/demos/ui/todo-list.html', name: 'Todo List', testFile: 'bosatsu-ui.spec.ts' },
  ],
  // Simulation demos (Generated from .bosatsu)
  'simulation': [
    { path: '/demos/simulation/loan-calculator.html', name: 'Loan Calculator', testFile: 'simulation.spec.ts' },
    { path: '/demos/simulation/carbon-footprint.html', name: 'Carbon Footprint', testFile: 'simulation.spec.ts' },
    { path: '/demos/simulation/tax-calculator.html', name: 'Tax Calculator', testFile: 'simulation.spec.ts' },
  ],
  // Benchmark demos
  'benchmarks': [
    { path: '/demos/benchmarks/ui-performance/index.html', name: 'UI Performance Benchmark', testFile: 'bosatsu-ui.spec.ts' },
  ],
};

test.describe('Demo Registry Verification', () => {
  for (const [category, demos] of Object.entries(DEMO_REGISTRY)) {
    for (const demo of demos) {
      test(`${category}/${demo.name}: loads successfully`, async ({ page }) => {
        const { errors } = setupCapture(page);
        await page.goto(demo.path);
        await page.waitForLoadState('domcontentloaded');

        expect(errors, `${demo.name} has JavaScript errors`).toHaveLength(0);
      });

      test(`${category}/${demo.name}: no NaN or undefined`, async ({ page }) => {
        await page.goto(demo.path);
        await page.waitForLoadState('domcontentloaded');

        // Check visible result/value elements, not raw body text (which includes JS source)
        const resultValues = page.locator('.result-value, .value-display, [id^="val-"]');
        const count = await resultValues.count();

        for (let i = 0; i < count; i++) {
          const text = await resultValues.nth(i).textContent();
          expect(text, `Value at index ${i} should not be NaN`).not.toBe('NaN');
          expect(text, `Value at index ${i} should not be undefined`).not.toBe('undefined');
        }
      });
    }
  }
});

function setupCapture(page: import('@playwright/test').Page) {
  const errors: string[] = [];
  page.on('pageerror', err => errors.push(err.message));
  return { errors };
}
