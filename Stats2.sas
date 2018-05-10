libname statdata 'U:\ECST131'; 
libname library 'U:\ECST131';

proc sgplot data = statdata.ameshousing3;
vbox saleprice / category = Central_Air connect=mean;
title "Sales Price difference across Central Air";
run;

proc sgplot data = statdata.ameshousing3;
vbox salePrice / category = Fireplaces connect=median;
title "Sales price difference across Fireplaces";
run;

proc sgplot data = statdata.ameshousing3;
vbox salePrice / category = Heating_QC connect=q1;
title "Sales price difference across Heating Quality";
run;

proc ttest data = statdata.testscores plot(shownull) = interval;
class Gender;
var SATSCore;
title 'Two sample test';
run;

proc ttest data = statdata.testscores plots (only shownull) = interval h0=0 sides = u;
class gender;
var SATScore;
title 'One side T test comparing the girls with boys';
run;
title;

proc ttest data=statdata.german plots(shownull)=interval h0=0 sides=L;
class Group;
var Change;
title 'German Training, Comparing Treatment to Control';   
title2 'One-Sided t-Test';
run;
title;

/* Analysis of variance - atleast one response variable and 1 categorical variable */

proc means data = statdata.mggarlic printalltypes maxdec = 3;
var bulbwt;
class Fertilizer;
title "Descriptive Statistics of Garlic Weight";
run;

proc print data = statdata.mggarlic (obs=10);
title "Partial Listing of Garlic Data";
run;

/* for sg plot use ods graphics on */
ods graphics on / width = 700;
proc sgplot data = statdata.mggarlic;

vbox BulbWt / category = Fertilizer datalabel = BedID; /*data label for outliers */
format BedID 5.;
title "Box Plot of Garlic Weight";

run;
title;

/* PROC GLM for Anova, sum of squares */

proc glm = statdata.mggarlic plots(only) = diagnostic(unpack); /*unpack = different pages for each graph */
class Fertilizer;
model BulbWt = Fertilizer;
means Fertilizer / hovtest; /*Test for homogeneity of variances - Leven's test */
run;
quit;

proc means data=statdata.ads printalltypes n mean 
           std skewness kurtosis;
   var Sales;
   class Ad;
   title 'Descriptive Statistics of Sales by Ad Type';
run;

proc sgplot data=statdata.ads;
   vbox Sales / category=Ad datalabel=Sales;
   title 'Box Plots of Sales by Ad Type';
run;
title;

proc means data=statdata.ads printalltypes n mean 
           std skewness kurtosis;
   var Sales;
   class Ad;
   title 'Descriptive Statistics of Sales by Ad Type';
run;

proc sgplot data=statdata.ads;
   vbox Sales / category=Ad datalabel=Sales;
   title 'Box Plots of Sales by Ad Type';
run;
title;

proc glm data=statdata.ads plots(only)=diagnostics(unpack);
   class Ad;
   model Sales=Ad;
   means Ad / hovtest;
   title 'Testing for Equality of Ad Type on Sales';
run;
quit;
title;

proc print data = statdata.mggarlic_block (obs=10);
run;

proc glm data = statdata.mggarlic_block plots (only) = diagnostic(unpack);
class Fertilizer Sector;
model Bulbwt = Fertilizer Sector;
title "Anova for Random Block design";
run;
quit;

proc glm data=statdata.ads1 plots(only)=diagnostics(unpack);
   class Ad Area;
   model Sales=Ad Area;
   title 'ANOVA for Randomized Block Design';
run;
quit;
title;

ods graphics on / width = 700;
ods trace off;

ods select LSMeans Diff MeanPlot DiffPlot ControlPlot;

proc glm data = statdata.mggarlic_block;
class Fertilizer Sector;
model Bulbwt = Fertilizer Sector;
lsmeans Fertilizer / pdiff = all adjust=tukey;
lsmeans Fertilizer / pdiff = controlu('4') adjust=dunnett;
lsmeans Fertilizer / pdiff = all adjust =t;
title "Garlic Data - Multiple Comparisons";
run;
quit;
title;

ods select LSMeans Diff MeanPlot DiffPlot ControlPlot;
proc glm data=statdata.ads1;
   class Ad Area;
   model Sales=Ad Area;
   lsmeans Ad / pdiff=all adjust=tukey;
   lsmeans Ad / pdiff=controlu('display') adjust=dunnett;
   title 'Pairwise Differences for Ad Types on Sales';
run;
quit;
title;

proc means data = statdata.drug mean var std printalltypes;
class Disease Drugdose;
var BloodP;
output out = means mean =BloodP_Mean;
/*format Drugdose dosef.;*/
title "Selected Decriptive Statistics for Drug Dataset";
run;
title; 

ods graphics on / width = 700;
proc sgplot data = means;
where _TYPE_ = 3;
scatter x=DrugDose y=BloodP_Mean / group= Disease markerattrs = (size=10);
series x=DrugDose y=BloodP_Mean / group=Disease lineattrs=(thickness=2);
   xaxis integer;
   /*format DrugDose dosef.;*/  
   title "Plot of Stratified Means in Drug Data Set";
run;
title; 

ods graphics on / width = 800;
proc glm data=statdata.drug;
   class DrugDose Disease;
   model Bloodp=DrugDose Disease DrugDose*Disease;
   /*format DrugDose dosef.;*/  
   title "Analyze the Effects of DrugDose and Disease";
   title2 "Including Interaction";
run;
quit;
title;

ods graphics on / width=800;
ods select meanplot lsmeans slicedanova;  

proc glm data=statdata.drug;
   class DrugDose Disease;
   model Bloodp=DrugDose Disease DrugDose*Disease;
   lsmeans DrugDose*Disease / slice=Disease; 
   /*format DrugDose dosef.;*/
   title "Analyze the Effects of DrugDose";
   title2 "at Each Level of Disease";
run;
quit;
title;

/*exercise */

proc means data=statdata.concrete mean var std printalltypes;
   class Brand Additive;
   var Strength;
   output out=means mean=Strength_Mean;
   title 'Selected Descriptive Statistics for Concrete Data Set';
run;

proc sgplot data=means;
   where _TYPE_=3;
   scatter x=Additive y=Strength_Mean / group=Brand 
           markerattrs=(size=10);
   xaxis integer;
   title 'Plot of Stratified Means in Concrete Data Set';
run;
title;

proc glm data=statdata.concrete;
   class Additive Brand;
   model Strength=Additive Brand Additive*Brand;
   title 'Analyze the Effects of Additive and Brand';
   title2 'on Concrete Strength';
run;
quit;
title;

proc glm data=statdata.concrete;
   class Additive Brand;
   model Strength=Additive Brand;
   lsmeans Additive;
   title 'Analyze the Effects of Additive and Brand';
   title2 'on Concrete Strength without Interaction';
run;
quit;
title;

/* Store */

ods graphics on;

proc glm data=statdata.ameshousing3 
         order=internal 
         plots(only)=intplot;
   class Season_Sold Heating_QC;
   model SalePrice=Heating_QC Season_Sold Heating_QC*Season_Sold;
   lsmeans Heating_QC*Season_Sold / diff slice=Heating_QC;
   format Season_Sold Season.;
   store out=interact;
   title "Model with Heating Quality and Season as Interacting Predictors";
run;
title;

proc plm restore=interact plots=all;
   slice Heating_QC*Season_Sold / sliceby=Heating_QC adjust=tukey;
   effectplot interaction(sliceby=Heating_QC) / clm;
run; 

proc sgplot data=statdata.drug;
   vline DrugDose / group=Disease 
                    stat=mean 
                    response=BloodP 
                    markers;
   format DrugDose dosefmt.;
run;

ods graphics on;

proc glm data=statdata.drug plots(only)=intplot;
   class DrugDose Disease;
   model BloodP=DrugDose|Disease;
   lsmeans DrugDose*Disease / slice=Disease;
run;
quit;
