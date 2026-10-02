#!/bin/bash
# setup_env.sh
# Run this before install_packages.R to configure the HiPerGator environment
# Usage: source setup_env.sh

# ── Modules ──────────────────────────────────────────────────────────────────
ml geos/3.6.2 proj/4.8.0 udunits/2.2.17
ml R

# ── Runtime library paths ─────────────────────────────────────────────────────
export LD_LIBRARY_PATH=/apps/udunits/2.2.17/lib:/apps/gdal/3.7.0/lib:/apps/lib/proj/4.8.0/lib:/apps/geos/3.6.2/lib:$LD_LIBRARY_PATH

# ── PROJ database ─────────────────────────────────────────────────────────────
export PROJ_DATA=/apps/gdal/3.7.0/share/proj

# ── Confirm environment ───────────────────────────────────────────────────────
echo "✓ Modules loaded:"
ml list

echo ""
echo "✓ LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
echo "✓ PROJ_DATA: $PROJ_DATA"
echo ""
echo "Ready to run: Rscript install_packages.R"
