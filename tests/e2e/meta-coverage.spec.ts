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
    { path: '/demos/benchmarks/ui-performance/index.html', name: 'UI Performance Benchmark', testFile: 'benchmark-integrity.spec.ts' },
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

// =============================================================================
// REALNESS CHECK: Demos must be generated from actual .bosatsu source
// =============================================================================

// Patterns that indicate fake/embedded JS instead of real Bosatsu compilation
const FAKE_DEMO_PATTERNS = [
  // Physics simulations with embedded formulas (not from Bosatsu)
  /function\s+updatePhysics\s*\(/,
  /position\s*\+=\s*velocity/,
  /velocity\s*\+=\s*gravity/,
  /Math\.(sin|cos|sqrt)\s*\(/,
  // Inline business logic (should come from compiled Bosatsu)
  /\/\/\s*Fake\s+calculation/i,
  /\/\/\s*Placeholder\s+logic/i,
  /\/\/\s*TODO:\s*replace\s+with\s+real/i,
  // Animation frame loops without Bosatsu
  /requestAnimationFrame\s*\(\s*function\s+animate/,
];

// Map of demo paths to their expected .bosatsu source files
const DEMO_SOURCE_MAP: Record<string, string[]> = {
  'ui/counter.html': ['demos/ui/counter.bosatsu'],
  'ui/todo-list.html': ['demos/ui/todo-list.bosatsu'],
  'simulation/loan-calculator.html': [
    'demo/loan_calculator_numeric_func.bosatsu',
    'demo/loan_calculator_numeric_func.sim.bosatsu'
  ],
  'simulation/carbon-footprint.html': [
    'demo/carbon_numeric.bosatsu',
    'demo/carbon_numeric.sim.bosatsu'
  ],
  'simulation/tax-calculator.html': [
    'demo/tax_numeric.bosatsu',
    'demo/tax_numeric.sim.bosatsu'
  ],
};

test.describe('Demo Realness Verification', () => {
  test('every demo has corresponding .bosatsu source files', async () => {
    const missingSource: { demo: string; missing: string[] }[] = [];

    for (const [demoPath, sourceFiles] of Object.entries(DEMO_SOURCE_MAP)) {
      const missing: string[] = [];

      for (const sourceFile of sourceFiles) {
        const fullPath = path.join(__dirname, '../..', sourceFile);
        if (!fs.existsSync(fullPath)) {
          missing.push(sourceFile);
        }
      }

      if (missing.length > 0) {
        missingSource.push({ demo: demoPath, missing });
      }
    }

    if (missingSource.length > 0) {
      console.error('Demos missing source files:');
      missingSource.forEach(d => {
        console.error(`  - ${d.demo}: missing ${d.missing.join(', ')}`);
      });
    }

    expect(
      missingSource,
      'Some demos are missing their .bosatsu source files - they may be fake!'
    ).toHaveLength(0);
  });

  test('no demo contains fake/embedded JS patterns', async () => {
    const webDeployDir = path.join(__dirname, '../../web_deploy/demos');
    const fakePatterns: { file: string; patterns: string[] }[] = [];

    // Only run if web_deploy exists (skip during local dev if not built)
    if (!fs.existsSync(webDeployDir)) {
      console.log('web_deploy/demos not found, skipping fake pattern check (run after build)');
      return;
    }

    function checkFile(filePath: string) {
      const content = fs.readFileSync(filePath, 'utf-8');
      const found: string[] = [];

      for (const pattern of FAKE_DEMO_PATTERNS) {
        if (pattern.test(content)) {
          found.push(pattern.toString());
        }
      }

      if (found.length > 0) {
        fakePatterns.push({ file: filePath, patterns: found });
      }
    }

    function walkDir(dir: string) {
      if (!fs.existsSync(dir)) return;

      const files = fs.readdirSync(dir);
      for (const file of files) {
        const fullPath = path.join(dir, file);
        const stat = fs.statSync(fullPath);

        if (stat.isDirectory()) {
          walkDir(fullPath);
        } else if (file.endsWith('.html')) {
          checkFile(fullPath);
        }
      }
    }

    walkDir(webDeployDir);

    if (fakePatterns.length > 0) {
      console.error('Demos with fake/embedded JS patterns:');
      fakePatterns.forEach(f => {
        console.error(`  - ${f.file}:`);
        f.patterns.forEach(p => console.error(`      ${p}`));
      });
    }

    expect(
      fakePatterns,
      'Some demos contain fake/embedded JS - they should be generated from Bosatsu!'
    ).toHaveLength(0);
  });

  test('landing page only links to real demos', async () => {
    const landingPagePath = path.join(__dirname, '../../web/index.html');
    const content = fs.readFileSync(landingPagePath, 'utf-8');

    // Find all demo links
    const hrefRegex = /href="demo\/([^"]+\.html)"/g;
    const demoLinks: string[] = [];
    let match;

    while ((match = hrefRegex.exec(content)) !== null) {
      demoLinks.push(match[1]);
    }

    // Check for known fake demos that should NOT be in landing page
    const KNOWN_FAKE_DEMOS = [
      'bouncing-ball.html',
      'pendulum.html',
      'investment.html', // Old non-Numeric version
    ];

    const fakeLinksFound = demoLinks.filter(link =>
      KNOWN_FAKE_DEMOS.some(fake => link.includes(fake))
    );

    if (fakeLinksFound.length > 0) {
      console.error('Landing page links to known fake demos:');
      fakeLinksFound.forEach(link => console.error(`  - ${link}`));
    }

    expect(
      fakeLinksFound,
      'Landing page should not link to fake demos'
    ).toHaveLength(0);
  });

  test('all .bosatsu source files use Bosatsu/Numeric for math', async () => {
    const numericDemos = [
      'demo/loan_calculator_numeric_func.bosatsu',
      'demo/carbon_numeric.bosatsu',
      'demo/tax_numeric.bosatsu',
    ];

    const missingNumeric: string[] = [];

    for (const file of numericDemos) {
      const fullPath = path.join(__dirname, '../..', file);
      if (fs.existsSync(fullPath)) {
        const content = fs.readFileSync(fullPath, 'utf-8');
        if (!content.includes('Bosatsu/Numeric')) {
          missingNumeric.push(file);
        }
      }
    }

    if (missingNumeric.length > 0) {
      console.error('Numeric demos not importing Bosatsu/Numeric:');
      missingNumeric.forEach(f => console.error(`  - ${f}`));
    }

    expect(
      missingNumeric,
      'Numeric demos must import from Bosatsu/Numeric'
    ).toHaveLength(0);
  });
});
