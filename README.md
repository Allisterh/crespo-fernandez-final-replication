# Replication files for Cuaresma, J. C., & Fernández, O. (2024). Explaining long-term bond yields synchronization dynamics in Europe. Economic Modelling, 106684.


This readme file is intended to replicate the results of the paper titled “Explaining Long-Term Bond Yields Synchronization Dynamics in Europe”, written by Jesus Crespo Cuaresma and Oscar Fernandez, and published in the journal *Economic modelling* ([link to the paper](https://www.sciencedirect.com/science/article/pii/S0264999324000403)). All codes contain documentation which should help the researcher to replicate our results in a user-friendly format. 


**IMPORTANT NOTE 1:** to fully reproduce the results with the same seed, the researcher needs to use an old version of the package BMS, which is the one we used for the paper. More specifically, the researcher needs to download in R the version 0.3.4 of the BMS package here: https://cran.r-project.org/src/contrib/Archive/BMS/. This will make sure that the seed and the corresponding results are always the same, which in our case is set to "set.seed(14091998)". The update to the version 0.3.5 made the setting of the seed a little more complicated, which is why we preferred to stick to the version 0.3.4. 

**IMPORTANT NOTE 2:** all the codes were run in a Macbook Air with an Apple M1 chip, 16 GB of memory, Sonoma 14.1.2. The R version used is 4.2.2. It is advised to use the same R version to avoid issues with the BMS package version 0.3.4. To download the R version 4.2.2, the researcher can follow this link: [https://cran.r-project.org/bin/windows/base/old/](https://cran.r-project.org/bin/windows/base/old/)

The zipped folder attached contains two folders with all the necessary data files and codes to reproduce the results of the paper:

1. **data**: this folder contains all the necessary data to reproduce all the results of the paper: 
	1. synch_levels.xlsx: this file contains the data on the synchronization rate in the long-term government bond yield and the independent variables in levels.
	2. synch_synch.xlsx: this file contains the data on the synchronization rate in the long-term government bond yield and the synchronization rates of the independent variables.
	3. spread_levels: this file contains the data on the long-term government bond yield spreads (against the German yield) and the independent variables in levels.
	4. s_t.xlsx: this file contains the data on the standard deviation in long-term government bond yields
    	5. yields data.xlsx: this file contains data on the yield spread (against the German yield) for the GIIPS countries

2. **scripts**: this folder contains all the necessary code files to reproduce the results of the paper:
	1. synch_levels.R: this code is intended to import the file synch levels.xlsx to reproduce the results were we regress the yield synchronization rates on the levels of the independent variables. The code produces the following output from the paper: Table A.3, Figure 3, Table A.1, Table A.2, Figure A.1, Figure A.3, Figure A.4, Table 1.

	2. synch_synch.R: this code is intended to import the file synch synch.xlsx to reproduce the results were we regress the yield synchronization rates on the synchronization rates of the independent variables. The code produces the following output from the paper: Figure A.5, Figure A.6, Figure A.2, Table 2.

	3. spread_levels.R: this code is intended to import the file spread levels.xlsx to reproduce the results were we regress the yield spreads on the levels of the independent variables. The code produces the following output from the paper: Table A.4.

	4. forecast_code.R: this code is intended to import the files synch levels.xlsx and synch synch.xlsx with the purpose of obtaining the results of the forecasts ONLY for the following models: Pooled AR(1), Country-specific AR(1), BMA synch-levels, BMA synch-synch. The code computes the rolling forecasts of the synchronization variable together with multiple forecasting indicators (RMSE, Theil's U, Accuracy, Hit Rate, False Alarm, Kuiper score). The code reproduces a part of the following output from the paper: Table 3, Table A.5.

	5. pip_50_synch_levels.R: this code is intended to import the file synch levels.xlsx with the purpose of obtaining the forecasts results ONLY for the following models: BMA synch-levels (only those variables with PIP>50\%). We separate the codes for the sake of readability. The code computes the rolling forecasts of the synchronization variable together with multiple forecasting indicators (RMSE, Theil's U, Accuracy, Hit Rate, False Alarm, Kuiper score). The code reproduces a part of the following output from the paper: Table 3, Table A.5.   

	6. figure1_figure2.m: this code is intended to import the files s_t.xlsx and yields_data.xlsx to reproduce the following figures found in the paper: Figure 1, Figure 2.
 	7. ~funcs.R: this file contains all the necessary functions to reproduce the results.
	8. ~packages.R: this file loads all the necessary packages to reproduce the results. 
 	9. ~specs.R: this file contains all the necessary specifications (number of burn-ins, data paths, and etcetera) to reproduce the results. 


	6. pip_50_synch_synch.R: this code is intended to import the file synch synch.xlsx with the purpose of obtaining the forecasts results ONLY for the following models: BMA synch-synch (only those variables with PIP>50\%). We separate the codes for the sake of readability. The code computes the rolling forecasts of the synchronization variable together with multiple forecasting indicators (RMSE, Theil's U, Accuracy, Hit Rate, False Alarm, Kuiper score). The code reproduces a part of the following output from the paper: Table 3, Table A.5.

