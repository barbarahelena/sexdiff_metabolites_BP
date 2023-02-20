#!/bin/bash
./XGBeast.py -path MaleFemale -name MaleFemale -x class -n 200 -param metabolite_grid.json
./XGBeast.py -path MaleFemale -name MaleFemale -x class -n 200 -param metabolite_grid.json - permute
