#!/bin/bash
./XGBeast.py -path NoDM/Female/SBP -name nodmfemalesbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path NoDM/Female/DBP -name nodmfemaledbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path NoDM/Male/SBP -name nodmmalesbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path NoDM/Male/DBP -name nodmmaledbp -x reg -n 200 -param metabolite_grid.json
