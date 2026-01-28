# Phase 6: Simulation Applets Full - PR Guide

## Overview

Phase 6 completes the simulation applet infrastructure with full provenance tracking UI:
- "Why?" buttons with derivation chain display
- "What if?" assumption toggling UI
- Parameter sweep sliders
- Self-contained HTML embed generation
- Theme support (light/dark)

## Files Changed

### Core UI Components

| File | Description |
|------|-------------|
| `core/src/main/scala/dev/bosatsu/ui/WhyExplainer.scala` | "Why?" button and modal UI generation |
| `core/src/main/scala/dev/bosatsu/ui/WhatIfToggle.scala` | "What if?" assumption toggle UI |
| `core/src/main/scala/dev/bosatsu/ui/ParameterSweep.scala` | Parameter sweep slider UI |
| `core/src/main/scala/dev/bosatsu/ui/EmbedGenerator.scala` | Self-contained HTML generation |
| `core/src/main/scala/dev/bosatsu/ui/SimulationApplet.scala` | Derivation types and simulation state |

### Simulation CLI

| File | Description |
|------|-------------|
| `simulation-cli/src/main/scala/dev/bosatsu/simulation/Main.scala` | CLI entry point |
| `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala` | Argument parsing |
| `simulation-cli/src/main/scala/dev/bosatsu/simulation/DerivationAnalyzer.scala` | IR analysis for derivation extraction |
| `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationGen.scala` | HTML generation with tracking |

### Tests

| File | Description |
|------|-------------|
| `core/src/test/scala/dev/bosatsu/ui/SimulationAppletTest.scala` | Tests for WhyExplainer, WhatIfToggle, ParameterSweep |
| `core/src/test/scala/dev/bosatsu/ui/SimulationAppletGen.scala` | ScalaCheck generators |
| `core/src/test/scala/dev/bosatsu/ui/UIComponentsTest.scala` | Comprehensive UI component tests |
| `simulation-cli/src/test/scala/dev/bosatsu/simulation/DerivationAnalyzerTest.scala` | IR analysis tests |
| `simulation-cli/src/test/scala/dev/bosatsu/simulation/SimulationGenTest.scala` | HTML generation tests |
| `simulation-cli/src/test/scala/dev/bosatsu/simulation/MainTest.scala` | CLI integration tests |
| `simulation-cli/src/test/scala/dev/bosatsu/simulation/SimulationCommandTest.scala` | Argument parsing tests |

### Demo Pages

| File | Description |
|------|-------------|
| `web_deploy/demo/loan-calculator.html` | Loan calculator with provenance |
| `web_deploy/demo/investment.html` | Investment calculator with provenance |
| `web_deploy/demo/carbon-footprint.html` | Carbon footprint calculator with provenance |

## Key Features

### "Why?" Buttons

Shows derivation chain explaining how a value was computed:

```
net_income = income - total_tax = 100000 - 21250 = 78750
  └── income = 100000 [assumption]
  └── total_tax = taxable * tax_rate = 85000 * 0.25 = 21250
      └── taxable = income - deductions = 100000 - 15000 = 85000
      └── tax_rate = 0.25 [assumption]
```

### "What if?" Toggles

Interactive controls to explore alternative assumptions:
- Number inputs for numeric assumptions
- Boolean toggles for true/false assumptions
- Text inputs for string assumptions

### Parameter Sweeps

Range sliders to explore sensitivity:
- Shows current value
- Recomputes all derived values on change
- Min/max bounds displayed

### Theme Support

- Light theme (default)
- Dark theme via `--theme dark` flag

### Bundle Size

- Self-contained HTML: ~6KB gzipped (target: <20KB)
- Includes all JS, CSS, and computation code

## Usage

```bash
# Generate simulation HTML
bosatsu-sim input.bosatsu -o output.html

# With options
bosatsu-sim tax.bosatsu \
  --output tax-demo.html \
  --title "Tax Calculator" \
  --theme dark \
  --sweeps
```

## Test Results

- Core UI tests: 148 passing
- Simulation CLI tests: 155 passing
- Total: 303 tests

## Security

- HTML escaping added to prevent XSS in WhatIfToggle and ParameterSweep
- User-provided names are escaped before interpolation into innerHTML

## Verification

1. Build: `nix-shell --run "sbt simulationCli/compile"`
2. Test: `nix-shell --run "sbt simulationCli/test"`
3. Run: `nix-shell --run "sbt 'simulationCli/run --help'"`
4. Generate demo: `nix-shell --run "sbt 'simulationCli/run demo.bosatsu -o test.html'"`
5. Open in browser and verify "Why?" and "What if?" functionality
