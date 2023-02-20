#!/bin/bash
#!/bin/bash
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_1 -name Nacetylcitrulline -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_2 -name etiocholanoloneglucuronide -x reg -n 200 -param microbiota_grid.json
# metabolite that was ranked #3 for DBP also appears in SBP list
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_4 -name androsteronesulfate -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_5 -name serotonin -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_6 -name cortoloneglucuronide -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_7 -name pregnanediol3glucuronide -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_8 -name vanillactate -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_9 -name 5alphaandrostan -x reg -n 200 -param microbiota_grid.json
./XGBeast.py -path Metabolites/DBP_metabolites/Fem_10 -name epiandrosteronesulfate -x reg -n 200 -param microbiota_grid.json
