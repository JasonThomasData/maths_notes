#!/usr/bin/env bash

# Sometimes Jupyter notebooks won't compile to PDF via Latex, since Jupyter notebook only recogises $ for starting math, which is a tex command (Tex is not LaTeX, apparently).
# Requires python install of pyppeteer

notebook_file=$1

jupyter nbconvert --to webpdf $notebook_file --allow-chromium-download

