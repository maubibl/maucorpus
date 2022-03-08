#!/bin/bash
set -e
cat pkgs-cran | grep -v "#" | xargs install2.r --error --skipinstalled
cat pkgs-github | grep -v "#" | xargs installGithub.r --deps TRUE
