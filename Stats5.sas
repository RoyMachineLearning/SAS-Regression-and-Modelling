libname statdata 'U:\ECST131'; 
libname library 'U:\ECST131';

proc freq data = statdata.sales;
tables Purchase Gender Income Gender*Income; /*purchase variable */
run;

proc freq data = statdata.sales;
tables Purchase*Gender*Income; /*purchase variable */
run;

ods graphics on;
proc freq data = statdata.sales;
tables Purchase Gender Income 
Gender*Purchase Income*Purchase / plots = (freqplot);/*purchase variable */
run;

ods graphics / width=500;

proc freq data=statdata.sales;
   tables Purchase Gender Income
          Gender*Purchase
          Income*Purchase  /
          plots=(freqplot); 
   format Purchase purfmt.;
   title1 'Frequency Tables for Sales Data';
run;

ods select histogram probplot;  

proc univariate data=statdata.sales;
   var Age;
   histogram Age / normal (mu=est
                   sigma=est); 
   probplot Age / normal (mu=est
                  sigma=est);
   title1 'Distribution of Age'; 
run;

data statdata.sales_inc;
   set statdata.sales;
   if Income='Low' then IncLevel=1;
   else If Income='Medium' then IncLevel=2;
   else If Income='High' then IncLevel=3;
run;

ods graphics / width=500;

proc freq data=statdata.sales_inc;
   tables IncLevel*Purchase / plots=freq;
   format IncLevel incfmt. Purchase purfmt.;
   title1 'Create variable IncLevel to correct Income';
run;

title;

proc freq data=statdata.safety;
   tables Unsafe Type Region;
   title 'Safety Data Frequencies';
run;
title;

title;

/*o = p / 1 - p*/

/* test of association - pearson chisquare test*/

proc freq data=statdata.sales_inc;
   tables Gender*Purchase /
          chisq expected cellchi2 nocol nopercent 
          relrisk;
   format Purchase purfmt.;
   title1  'Association between Gender and Purchase';
run;

title;

proc freq data=statdata.sales_inc;
   tables IncLevel*Purchase / chisq measures cl;
   format IncLevel incfmt. Purchase purfmt.;
   title1 'Ordinal Association between IncLevel and Purchase?';
run;

title;

/*practice */
proc freq data=statdata.safety;
   tables Region*Unsafe / chisq relrisk expected;
   format Unsafe safefmt.;
   title "Association between Unsafe and Region";
run;

title;
/* logistic regression - binary, ordinal and nominal */

proc logistic data = statdata.sales_inc
plots(only) = (effect);

class gender (param = ref ref = 'Male');
model Purchase (event='1') = Gender;
run;

ods graphics / width=700;

proc logistic data=statdata.sales_inc
              plots(only)=(effect);
   class Gender (param=ref ref='Male'); 
   model Purchase(event='1')=Gender; 
   title1 'LOGISTIC MODEL (1):Purchase=Gender';
run;

title;

proc logistic data=statdata.titanic2;
class Age (param=ref ref='child'); 
model Survived(event='yes')=Age;
run;

proc logistic data=statdata.safety plots(only)=(effect);
   class Region (param=ref ref='Asia');
   model Unsafe(event='1')=Region;
   title1 'LOGISTIC MODEL (1):Unsafe=Region';
run;

title;

/* multiple logistic regression - units for odds ratio */

ods graphics / width=700;

proc logistic data=statdata.sales_inc 
              plots(only)=(effect oddsratio);
   class Gender (param=ref ref='Male')
         IncLevel (param=ref ref='1');
   units Age=10;
   model Purchase(event='1')=Gender Age IncLevel / 
         selection=backward clodds=pl; /* pl = profile likelyhood */
   title1 'LOGISTIC MODEL (2):Purchase=Gender Age IncLevel';
run;

title;

proc logistic data=statdata.titanic2;
      class Age (param=ref ref='child')
              Gender (param=ref ref='female'); 
      model Survived(event='yes')=Age Gender / selection=backward;
run;

proc logistic data=statdata.titanic2;
   class Age (param=ref ref='child')
                  Gender (param=ref ref='female')
    class (param=ref ref='crew');
	model Survived(event='yes')=Age Gender | Class
      / selection=backward;
run;

/* two factor interactions along with the main effect*/

ods graphics / width=700;

proc logistic data=statdata.sales_inc 
              plots(only)=(effect oddsratio);
   class Gender (param=ref ref='Male')
         IncLevel (param=ref ref='1');
   units Age=10;
   model Purchase(event='1')=Gender | Age | IncLevel @2/ 
         selection=backward clodds=pl ;  /*or WALD. also pl = profile likelyhood */
   title1 'LOGISTIC MODEL (3): Main Effects and 2-Way Interactions';
   title2 '/ sel=backward';
run;

title;

/*With odds ratio plot.*/

ods graphics / width=700;
ods select OddsRatiosPL ORPlot;

proc logistic data=statdata.sales_inc 
              plots(only)=(oddsratio);
   class Gender (param=ref ref='Male')
         IncLevel (param=ref ref='1');
   units Age=10;
   model Purchase(event='1')=Gender | IncLevel Age;
   oddsratio Age / cl=pl;
   oddsratio Gender / diff=ref at (IncLevel=all) cl=pl;
   oddsratio IncLevel / diff=ref at (Gender=all) cl=pl;
   title1 'LOGISTIC MODEL (3a): Significant Terms and All Odds Ratios';
   title2 '/ sel=backward';
run;

title;

/* interaction plot */


proc means data=statdata.sales_inc noprint nway;
   class IncLevel Gender;
   var Purchase;
   output out=bins sum(Purchase)=Purchase n(Purchase)=BinSize;
run;

data bins;
   set bins;
      Logit=log((Purchase+1)/(BinSize-Purchase+1));
run;

proc sgscatter data=bins;
   plot Logit*IncLevel /group=Gender markerattrs=(size=15)
                        join;
   format IncLevel incfmt.;
   label IncLevel='Income Level';
   title;
run;
quit;

proc logistic data=car_poll;
           class Gender (param=ref ref='Female') 
                 Status (param=ref ref='Married');
           model Type(event='sport')=Age Gender Status;
           oddsratio Age;
		   oddsratio Gender / diff=ref at (Status=all);
run;

/* Generate Predictions */

ods select none;
proc logistic data=statdata.ameshousing3;
   class Fireplaces (ref='0') Lot_Shape_2 (ref='Regular') / param=ref;
   model Bonus(event='1')=Basement_Area|Lot_Shape_2 Fireplaces;
   units Basement_Area=100;
   store out=isbonus;
run;
ods select all;

data newhouses;
   length Lot_Shape_2 $9;
   input Fireplaces Lot_Shape_2 $ Basement_Area;
   datalines;
   0 Regular 1060
   2 Regular 775
   2 Irregular 1100
   1 Irregular 975
   1 Regular 800
   ;
run;

proc plm restore=isbonus;
   score data=newhouses out=scored_houses / ILINK;
   title 'Predictions using PROC PLM';
run;

proc print data=scored_houses;
run;
