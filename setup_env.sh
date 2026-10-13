#!/bin/bash
# setup_env.sh
# Usage: source setup_env.sh

# ── Modules ──────────────────────────────────────────────────────────────────
ml geos/3.6.2 proj/4.8.0 udunits/2.2.17
ml R

# ── Strip GDAL 3.7.0 from LD_LIBRARY_PATH entirely, then rebuild cleanly ─────
export LD_LIBRARY_PATH=$(echo $LD_LIBRARY_PATH | tr ':' '\n' | grep -v '/apps/gdal/3.7.0' | tr '\n' ':' | sed 's/:$//')
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/apps/udunits/2.2.17/lib:/apps/lib/proj/4.8.0/lib:/apps/geos/3.6.2/lib:/apps/gdal/3.7.0/lib

# ── PROJ database ─────────────────────────────────────────────────────────────
export PROJ_DATA=/apps/gdal/3.7.0/share/proj

# ── Wrapper to run Rscript with GDAL stripped from library path ───────────────
rscript_clean() {
  LD_LIBRARY_PATH=$(echo $LD_LIBRARY_PATH | tr ':' '\n' | grep -v '/apps/gdal/3.7.0' | tr '\n' ':' | sed 's/:$//') \
  Rscript "$@"
}

# ── Confirm environment ───────────────────────────────────────────────────────
echo "✓ Modules loaded:"
ml list
echo ""
echo "✓ LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
echo "✓ PROJ_DATA: $PROJ_DATA"
echo ""
echo "Ready to run: rscript_clean your_script.R"