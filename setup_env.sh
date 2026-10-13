#!/bin/bash
# setup_env.sh
# Usage: source setup_env.sh

#
# HiPerGator environment setup for this project.
# Usage from Bash: source setup_hipergator.sh
#
# This script is intentionally HiPerGator-specific.
# Do not source it on a laptop.

if [[ ! -d /apps ]] || ! command -v ml >/dev/null 2>&1; then
  echo "ERROR: This setup script is for HiPerGator only." >&2
  echo "On a laptop, use the normal R environment and library paths." >&2
  return 1 2>/dev/null || exit 1
fi

# Load the cluster modules used by this project.
ml geos/3.6.2 proj/4.8.0 udunits/2.2.17 || {
  echo "ERROR: Could not load required spatial modules." >&2
  return 1
}
ml R || {
  echo "ERROR: Could not load the R module." >&2
  return 1
}

# Project's personal R library on HiPerGator.
R_LIBS_USER="/blue/ewhite/alexanderblochel/R/libs"
if [[ ! -d "$R_LIBS_USER" ]]; then
  echo "ERROR: HiPerGator R library not found: $R_LIBS_USER" >&2
  return 1
fi
export R_LIBS_USER

# These paths must correspond to the HiPerGator setup tested for this project.
libstdcxx="/apps/ntsynt/1.0.2/lib/gcc/x86_64-conda-linux-gnu/14.2.0/libstdc++.so.6"
if [[ ! -f "$libstdcxx" ]]; then
  echo "ERROR: Expected libstdc++ library not found: $libstdcxx" >&2
  return 1
fi
export LD_PRELOAD="${libstdcxx}${LD_PRELOAD:+:$LD_PRELOAD}"

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}/apps/udunits/2.2.17/lib:/apps/lib/proj/4.8.0/lib:/apps/geos/3.6.2/lib:/apps/gdal/3.7.0/lib"
export PROJ_DATA="/apps/gdal/3.7.0/share/proj"

echo "HiPerGator environment configured."
echo "R library: $R_LIBS_USER"
echo "LD_PRELOAD: $LD_PRELOAD"
echo "PROJ_DATA: $PROJ_DATA"
echo
echo "Loaded modules:"
ml list
echo
echo "Next: run the project's package installation/setup instructions."