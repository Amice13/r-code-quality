TITLE: 	GodefroidtLanger_TerrorTrust_RandomIntercepts
DATA:    	File = TerrorTrust_Data.csv;
VARIABLE:	NAMES = resp ccode trust tfear
sex age educat unemp polint reli_m nnews tvnews 
weight1 weight2
lifexp gdp ln_gdp gini polity comm mon
gti att d_att;
CATEGORICAL = trust tfear;
USEVARIABLES ARE ccode trust tfear
sex age educat unemp polint nnews tvnews  
ln_gdp gini polity gti;
MISSING ARE ALL (-99, -5, -4, -3, -2, -1) ;
CLUSTER = ccode;
WITHIN = sex age educat unemp polint nnews tvnews;	
BETWEEN = gti ln_gdp gini polity;
ANALYSIS: TYPE = TWOLEVEL;
          ESTIMATOR = MLR;
          ALGORITHM = INTEGRATION;
          INTEGRATION = MONTECARLO;
MODEL: 
       %WITHIN%
       tfear ON nnews tvnews 		                                      ! mediator
                     sex age educat unemp polint;
       trust ON tfear nnews tvnews  	                             	      ! DV
                     sex age educat unemp polint;
       %BETWEEN%
       tfear ON gti
	  	 ln_gdp gini polity;
       trust ON gti
	  	 ln_gdp gini polity;
OUTPUT: SAMPSTAT; 


