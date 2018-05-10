libname statdata 'U:\ECST131'; 
libname library 'U:\ECST131';

proc means data = statdata.testscores maxdec = 2 fw=10 printalltypes 
n mean median std var q1 q3;
class Gender; /* group the data */
var SATScore; /* descriptive statistics */
title ' Descriptive Statistics using PROC MEANS ';
run;

proc means data = statdata.Normtemp maxdec=2 fw=10 printalltypes mean std q1 q3;
class Gender;
var BodyTemp;
title 'Selected Descriptive Statistics for Body Temp';
run;

/*Create histograms and probability plots */

proc univariate data = statdata.testscores;
var SATScore;
id IDNumber;
histogram SATScore / normal (mu=est sigma=est);
inset skewness kurtosis / position = ne;
probplot SATScore / normal(mu=est sigma=est);
inset skewness kurtosis;
title 'Descriptive statistics using Proc Univariate';
run;
title;

/* Create boxplots */
proc sgplot data=statdata.testscores;
refline 1200 / axis=y lineattrs=(color=blue);
vbox SATScore / datalabel = IDNumber;
format IDNumber 8.;
title "Box plots of SAT Score";
run;
title;

proc univariate data = statdata.Normtemp noprint;
var BodyTemp HeartRate;
histogram BodyTemp HeartRate / normal(mu = est sigma=est);
inset min max skewness Kurtosis / position = ne;
probplot BodyTemp HeartRate / normal(mu=est sigma=est);
inset min max skewness kurtosis;
title "Descriptive statistics using PROC Univariate"; 
run;
title;
proc sgplot data=statdata.normtemp;
   refline 98.6 / axis=y lineattrs=(color=blue);
   vbox BodyTemp / datalabel=ID;
   format ID 3.;
   title "Box Plots of Body Temps";
run;
proc sgplot data=statdata.normtemp;
   vbox HeartRate / datalabel=ID;
   format ID 3.;
   title "Box Plots of Heart Rate";
run;
title;

/* 95% confidence interval for the mean */

proc means data = statdata.testscores maxdec=4 n mean stderr clm alpha=.01;
var satscore;
title '95% CI for SAT';
run;
title;

proc means data=statdata.normtemp maxdec=2 n mean stderr clm;
 var BodyTemp;
 title '95% Confidence Interval for Body Temp';
run;
title;

ods select testsforlocation;
proc univariate data=statdata.testscores mu0=1200;
   var SATScore;
   title 'Testing Whether the Mean of SAT Scores=1200';
run;

ods select testsforlocation;
proc univariate data=statdata.normtemp mu0=98.6;
    var BodyTemp;
    title 'Testing Whether the Mean Body Temperature = 98.6';
run;
title;

