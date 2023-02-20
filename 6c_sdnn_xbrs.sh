#!/bin/bash
#./XGBeast.py -path SDNN/Male -name SDNN_male -x reg -n 200 -param metabolite_grid.json
#./XGBeast.py -path SDNN/Female -name SDNN_female -x reg -n 200 -param metabolite_grid.json
#./XGBeast.py -path BRS/Male -name BRS_male -x reg -n 200 -param metabolite_grid.json
#./XGBeast.py -path BRS/Female -name BRS_female -x reg -n 200 -param metabolite_grid.json

./XGBeast.py -path SDNN/Male -name SDNN_male -x reg -n 200 -param metabolite_grid.json -permute
./XGBeast.py -path SDNN/Female -name SDNN_female -x reg -n 200 -param metabolite_grid.json -permute
./XGBeast.py -path BRS/Male -name BRS_male -x reg -n 200 -param metabolite_grid.json -permute
./XGBeast.py -path BRS/Female -name BRS_female -x reg -n 200 -param metabolite_grid.json -permute
