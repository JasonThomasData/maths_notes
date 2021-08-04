#!/usr/bin/env bash

echo "CREATE, ACTIVATE VENV"
virtualenv venv -p python3
source venv/bin/activate

echo "PYTHON INSTALLS"
pip install jupyter

echo "R INSTALLS"
sudo apt install r-base
echo "r = getOption('repos'); r['CRAN'] = 'https://cran.csiro.au/'; options(repos = r); install.packages('IRkernel')" | R --vanilla
echo "IRkernel::installspec()" | R --vanilla

