#!/usr/bin/env bash
# HiPerGator setup for this project.
# Usage: source ./setup_env.sh
if [[ ! -d /apps ]] || ! command -v ml >/dev/null 2>&1; then
  echo "ERROR: This setup script is for HiPerGator only." >&2
  return 1 2>/dev/null || exit 1
fi
ml R/4.6 || {
  echo "ERROR: Could not load R/4.6." >&2
  return 1
}
ml gdal/3.7.0 || {
  echo "ERROR: Could not load gdal/3.7.0." >&2
  return 1
}
export R_LIBS_USER=/blue/ewhite/alexanderblochel/R/libs
mkdir -p "$R_LIBS_USER"
echo "R package library: $R_LIBS_USER"
echo "R version: $(R --version | head -n 1)"