#!/bin/bash
set -e
cat pkgs-cran | grep -v "#" | xargs install2.r --error --ncpus -1 --deps TRUE
cat pkgs-github | grep -v "#" | xargs installGithub.r --deps TRUE
