#!/usr/bin/env bash
set -euo pipefail

# Regenerate all demos from .bosatsu source files
# Run this after modifying .bosatsu files

echo "=============================================="
echo "Regenerating UI demos..."
echo "=============================================="

# UI Demos - each .bosatsu file generates a .html file
for bosatsu_file in demos/ui/*.bosatsu; do
  base=$(basename "$bosatsu_file" .bosatsu)
  html_file="demos/ui/${base}.html"
  echo "Generating: $html_file"
  nix-shell --run "sbt 'simulationCli/run ui $bosatsu_file -o $html_file'" 2>&1 | tail -3
done

echo ""
echo "=============================================="
echo "Regenerating simulation demos..."
echo "=============================================="

# Loan Calculator
nix-shell --run 'sbt "simulationCli/run demo/loan_calculator_numeric_func.bosatsu demo/loan_calculator_numeric_func.sim.bosatsu -o demos/simulation/loan-calculator.html --title \"Loan Calculator\""'

# Carbon Footprint Calculator
nix-shell --run 'sbt "simulationCli/run demo/carbon_numeric.bosatsu demo/carbon_numeric.sim.bosatsu -o demos/simulation/carbon-footprint.html --title \"Carbon Footprint Calculator\""'

# Tax Calculator
nix-shell --run 'sbt "simulationCli/run demo/tax_numeric.bosatsu demo/tax_numeric.sim.bosatsu -o demos/simulation/tax-calculator.html --title \"Tax Calculator\""'

# Copy to web_deploy for local testing
echo ""
echo "Copying to web_deploy..."
mkdir -p web_deploy/demos/simulation web_deploy/demos/ui
cp demos/simulation/*.html web_deploy/demos/simulation/
cp demos/ui/*.html web_deploy/demos/ui/
cp demos/index.html web_deploy/demos/index.html

echo ""
echo "Done! Generated files:"
echo "UI demos:"
ls -la demos/ui/*.html
echo ""
echo "Simulation demos:"
ls -la demos/simulation/*.html
