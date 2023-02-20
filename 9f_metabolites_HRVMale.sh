#!/bin/bash
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_1 -name Nacetylneuraminate -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_2 -name isobutyrylcarnitine -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_3 -name gentisate -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_4 -name cholestenoate -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_5 -name cysteine -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_6 -name hydroxylaurate -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_7 -name phenylacetate -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_8 -name sphingomyelin -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_9 -name 1-oleoyl-GPI -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/HRV_metabolites/SDNN_10 -name glycerate -x reg -n 200 -param microbiota_grid.json
