#!/bin/bash
./XGBeast.py -path FemY_DBP -name femy_dbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path FemY_SBP -name femy_sbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path MaleY_DBP -name maley_dbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path MaleY_SBP -name maley_sbp -x reg -n 200 -param metabolite_grid.json

./XGBeast.py -path FemO_DBP -name femo_dbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path FemO_SBP -name femo_sbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path MaleO_DBP -name maleo_dbp -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path MaleO_SBP -name maleo_sbp -x reg -n 200 -param metabolite_grid.json

./XGBeast.py -path AgeStrata/FemY_SDNN -name SDNN_femY -x reg -n 200 -param metabolite_grid.json
./XGBeast.py -path AgeStrata/FemO_SDNN -name SDNN_femO -x reg -n 200 -param metabolite_grid.json
