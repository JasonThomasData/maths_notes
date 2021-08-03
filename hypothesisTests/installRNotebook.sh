#!/usr/bin/env bash

echo "REMOVE VENV"
rm -r venv

echo "CREATE VENV"
virtualenv venv -p python3
source venv/bin/activate

echo "PYTHON INSTALLS"
pip install jupyter
pip install -r requirements.txt 

echo "R INSTALLS"
sudo apt install r-base

echo "r = getOption('repos'); r['CRAN'] = 'http://cran.us.r-project.org'; options(repos = r); install.packages('IRkernel')" | R --vanilla
echo "IRkernel::installspec()" | R --vanilla
