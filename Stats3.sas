libname statdata 'U:\ECST131'; 
libname library 'U:\ECST131';

/***************EXPLORATORY DATA ANALYIS *************************/

proc sgscatter data = statdata.ameshousing3;
plot saleprice * Gr_Liv_Area / reg;
title "Association of Above grade living area with sales price";
run;

%let interval = GR_liv_Area Basement_Area Garage_Area Deck_Porch_Area Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom;

options nolabel;

proc sgscatter data = statdata.ameshousing3;
plot SalePrice * (&interval) / reg;
title "Associations of Interval Variables with Sale Price";
run;

proc corr data = statdata.fitness rank plots = matrix;
var runtime age weight run_pulse Rest_Pulse Maximum_Pulse Performance;
with Oxygen_Consumption;
Title "Correlation with Oxygen Consumption"; 
run;

proc corr data = statdata.fitness rank 
plots(only) = scatter(nvar=all ellipse=none); /*display the one you want*/
var runtime age weight run_pulse Rest_Pulse Maximum_Pulse Performance;
with Oxygen_Consumption;
Title "Correlation with Oxygen Consumption"; 
run;

ods graphics / imagemap = on;
/* correlation matrix and scattered plot matrix */
proc corr data = statdata.fitness nosimple
plots = matrix (nvar=all histogram); /*display the one you want*/
var runtime age weight run_pulse Rest_Pulse Maximum_Pulse Performance;
id name;
Title "Correlation with Oxygen Consumption"; 
run;

ods select histogram probplot;
proc univariate data = statdata.BodyFat;

var PctBodyFat2 Age Weight Height Neck Chest Abdomen Hip Thigh Knee Ankle Biceps Forearm Wrist;
probplot / normal (mu=est sigma=est);
inset skewness kurtosis;
title "Prediction of % body fat";
run;
title;

data statdata.bodyfat2;
   set statdata.bodyfat;
   if Height=29.5 then Height=69.5;
run;

proc corr data=statdata.bodyfat2 rank
          plots(only)=scatter(nvar=all ellipse=none);
   var Age Weight Height;
   with PctBodyFat2;
   title "Correlations and Scatter Plots with Body Fat %";
run;

proc corr data=statdata.bodyfat2 rank
     plots(only)=scatter(nvar=all ellipse=none);
   var Neck Chest Abdomen Hip Thigh
       Knee Ankle Biceps Forearm Wrist;
   with PctBodyFat2;
   title "Correlations and Scatter Plots with Body Fat %";
run;

title;

proc corr data=statdata.bodyfat2 nosimple
     plots=matrix(histogram);
   var Age Weight Height;
   title "Correlations and Scatter Plot Matrix of Basic Measures";
run;

proc corr data=statdata.bodyfat2 nosimple;
   var Neck Chest Abdomen Hip Thigh
       Knee Ankle Biceps Forearm Wrist;
   title "Correlations of Circumferences";
run;

proc corr data=statdata.bodyfat2 nosimple
     plots=matrix;
   var Neck Chest Abdomen Hip Thigh
       Knee Ankle Biceps Forearm Wrist;
   with Age Weight Height;
   title "Correlations and Scatter Plot Matrix between";
   title2 "Basic Measures and Circumferences";
run;
title;

/***************REGRESSION*************************/
ods graphics / width=700;
proc reg data = statdata.fitness;
model oxygen_consumption = runtime / clm cli;
id Name Runtime;
title 'Predicting Oxygen consumption from runtime';
run;
quit;
title;

data need_predictions;
input Runtime @@;
datalines;
9 10 11 12 13
;
run;

data predoxy;
set need_predictions statdata.fitness;
run;

proc reg data = predoxy;
model oxygen_consumption = Runtime / p;
id Runtime;
title 'Oxygen_Consumption = Runtime with predicted values';
run;
quit;
title;
run;

/*score*/

proc reg data=statdata.fitness noprint outest=estimates; 
   model Oxygen_Consumption=RunTime;
run;
quit;
 
proc print data=estimates;
   title "OUTEST= Data Set from PROC REG";
run;
title;

proc score data=need_predictions score=estimates
     out=scored type=parms; 
   var RunTime; 
run;
 
proc print data=Scored;
   title "Scored New Observations";
run;

title;

proc reg data = statdata.bodyfat2 noprint outest=estimates; 
model  PCTbodyFat2 = Weight /clm cli;
title "Regression of % Body Fat on Weight";
run;
quit;


proc reg data=statdata.bodyfat2 outest=estimates;
   model PctBodyFat2=Weight;
   title "Regression of % Body Fat on Weight";
run;

data toscore;
   input Weight @@;
   datalines;
125 150 175 200 225
;
run;

proc score data=toscore score=estimates
     out=scored type=parms;
   var Weight;
run;

proc print data=scored;
   title "Predicted % Body Fat from Weight 125 150 175 200 225";
run;
title;

/************MULTIPLE LINEAR REGRESSION************/

ods graphics on;

proc reg data=statdata.ameshousing3 ;
    model SalePrice=Basement_Area Lot_Area;
    title "Model with Basement Area and Lot Area";
run;
quit;

proc glm data=statdata.ameshousing3 
         plots(only)=(contourfit);
    model SalePrice=Basement_Area Lot_Area;
    store out=multiple;
    title "Model with Basement Area and Gross Living Area";
run;
quit;

proc plm restore=multiple plots=all;
    effectplot contour (y=Basement_Area x=Lot_Area);
    effectplot slicefit(x=Lot_Area sliceby=Basement_Area=250 to 1000 by 250);
run; 

title;

proc reg data=statdata.bodyfat2;
   model PctBodyFat2 = Age Weight Height
         Neck Chest Abdomen Hip Thigh
         Knee Ankle Biceps Forearm Wrist;
   title 'Regression of PctBodyFat2 on All Predictors';
run;
quit;
title;

proc reg data=statdata.bodyfat2;
   model PctBodyFat2 = Age Weight Height
         Neck Chest Abdomen Hip Thigh
         Ankle Biceps Forearm Wrist;
   title 'Remove Knee';
run;
quit;
title;

proc reg data=statdata.bodyfat2;
   model PctBodyFat2 = Age Weight Height
         Neck Abdomen Hip Thigh
         Ankle Biceps Forearm Wrist;
   title 'Remove Knee and Chest';
run;
quit;
title;
ods graphics / imagemap = on;

/* CP PLOT */
proc reg data = statdata.fitness plots(only) = (cp);
ALLREG: model oxygen_consumption = Performance Runtime Age Weight Run_Pulse Rest_Pulse Maximum_Pulse
 / selection = cp rsquare adjrsq best = 20;
 title "Best models using All - Regression option";
 run;
 quit;

proc reg data=statdata.fitness;
   PREDICT: model Oxygen_Consumption 
                  = RunTime Age Run_Pulse Maximum_Pulse; 
   EXPLAIN: model Oxygen_Consumption 
                  = RunTime Age Weight Run_Pulse Maximum_Pulse; 
   title 'Check "Best" Two Candidate Models';
run;
quit;
title;

ods graphics / imagemap=on;
proc reg data=statdata.bodyfat2 plots(only)=(cp);
   model PctBodyFat2 = Age Weight Height
         Neck Chest Abdomen Hip Thigh
         Knee Ankle Biceps Forearm Wrist
         / selection=cp best=60;
   title "Using Mallows' Cp for Model Selection";
run;
quit;
title;

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
              Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom ;

ods graphics on;

proc glmselect data=statdata.ameshousing3 plots=all;
FORWARD: model SalePrice = &interval / selection=forward select=SL slentry=0.1;
run;
/* STEPWISE DOESN'T TAKE COLINEARITY IN ACCOUNT */
proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISE: model SalePrice=&interval / selection=stepwise 
                   details=steps select=SL slstay=0.05 slentry=0.05;
   title "Stepwise Model Selection for SalePrice - SL 0.05";
run;
title;

/*Optional code that will execute forward and backward selection, each with slentry and slstay = 0.05.*/
proc glmselect data=statdata.ameshousing3 plots=all;
   FORWARD: model SalePrice=&interval / selection=forward details=steps select=SL slentry=0.05;
   title "Forward Model Selection for SalePrice - SL 0.05";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   BACKWARD: model SalePrice=&interval / selection=backward details=steps select=SL slstay=0.05;
   title "Backward Model Selection for SalePrice - SL 0.05";
run;
title;

ods graphics on;
proc glmselect data=statdata.bodyfat2 plots=all;
   STEPWISESL: model PctBodyFat2=Age Weight Height
                     Neck Chest Abdomen Hip Thigh
                     Knee Ankle Biceps Forearm Wrist
                     / SELECTION=STEPWISE SELECT=SL;
   title 'SL STEPWISE Selection with PctBodyFat2';
run;
quit;
title;

proc glmselect data=statdata.bodyfat2 plots=all;
   FORWARDSL: model PctBodyFat2=Age Weight Height
                    Neck Chest Abdomen Hip Thigh
                    Knee Ankle Biceps Forearm Wrist
                    / SELECTION=FORWARD SELECT=SL;
   title 'SL FORWARD Selection with PctBodyFat2';
run;
quit;
title;

proc glmselect data=statdata.bodyfat2 plots=all;
   FORWARDSL: model PctBodyFat2=Age Weight Height
                    Neck Chest Abdomen Hip Thigh
                    Knee Ankle Biceps Forearm Wrist
                    / SELECTION=FORWARD SELECT=SL
                    SLENTRY=0.05;
   title 'SL FORWARD (0.05) Selection with PctBodyFat2';
run;
quit;
title;

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
              Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom ;

ods graphics on;
proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISEAIC: model SalePrice = &interval / selection=stepwise details=steps select=AIC;
   title "Stepwise Model Selection for SalePrice - AIC";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISEBIC: model SalePrice = &interval / selection=stepwise details=steps select=BIC;
   title "Stepwise Model Selection for SalePrice - BIC";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISEAICC: model SalePrice = &interval / selection=stepwise details=steps select=AICC;
   title "Stepwise Model Selection for SalePrice - AICC";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISESBC: model SalePrice = &interval / selection=stepwise details=steps select=SBC;
   title "Stepwise Model Selection for SalePrice - SBC";
run;

title;

ods graphics on;
proc glmselect data=statdata.bodyfat2 plots=all;
   STEPWISESBC: model PctBodyFat2=Age Weight Height Neck Chest
                      Abdomen Hip Thigh Knee Ankle Biceps Forearm
                      Wrist / SELECTION=STEPWISE SELECT=SBC;
   title 'SBC STEPWISE Selection with PctBodyFat2';
run;
quit;
title;

proc glmselect data=statdata.bodyfat2 plots=all;
   STEPWISEAIC: model PctBodyFat2=Age Weight Height
                      Neck Chest Abdomen Hip Thigh
                      Knee Ankle Biceps Forearm Wrist
                      / SELECTION=STEPWISE SELECT=AIC;
   title 'AIC STEPWISE Selection with PctBodyFat2';
run;
quit;
title;
