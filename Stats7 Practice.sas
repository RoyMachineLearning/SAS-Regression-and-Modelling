%global PMLRfolder;
%let PMLRfolder= U:\ECPMLR41; 
libname pmlr "&PMLRfolder";

/* chapter 7 */

data pmlr.pva(drop=control_number 
                   MONTHS_SINCE_LAST_PROM_RESP 
                   FILE_AVG_GIFT 
                   FILE_CARD_GIFT);
   set pmlr.pva_raw_data;
   STATUS_FL=RECENCY_STATUS_96NK in("F","L");
   STATUS_ES=RECENCY_STATUS_96NK in("E","S");
   home01=(HOME_OWNER="H");
   nses1=(SES="1");
   nses3=(SES="3");
   nses4=(SES="4");
   nses_=(SES="?");
   nurbr=(URBANICITY="R");
   nurbu=(URBANICITY="U");
   nurbs=(URBANICITY="S");
   nurbt=(URBANICITY="T");
   nurb_=(URBANICITY="?");
run;

proc contents data=pmlr.pva;
run;

proc means data=pmlr.pva mean nmiss max min;
   var _numeric_;
run;

proc freq data=pmlr.pva nlevels;
   tables _character_;
run;

/* create training data */

proc sort data=pmlr.pva out=work.pva_sort;
   by target_b;
run;

proc surveyselect noprint data=work.pva_sort 
                  samprate=0.5 out=pva_sample seed=27513 
                  outall stratumseed=restore;
   strata target_b;
run;

data pmlr.pva_train(drop=selected SelectionProb SamplingWeight)
     pmlr.pva_valid(drop=selected SelectionProb SamplingWeight);
   set work.pva_sample;
   if selected then output pmlr.pva_train;
   else output pmlr.pva_valid;
run;
/* chapter 8 */


%let pi1 = 0.02;

proc sql noprint;
   select mean(ins) into :rho1 from work.train;
quit;

data work.train;
   set work.train;
   sampwt=((1-&pi1)/(1-&rho1))*(ins=0)+(&pi1/&rho1)*(ins=1);
run;

proc logistic data=work.train des;
   weight sampwt;
   model ins = dda ddabal dep depamt cashbk checks / stb;
   score data=pmlr.new out=scored;
run;

title1 "Logistic Regression Model of the Veterans' Organization Data";
proc logistic data=pmlr.pva_train plots(only)=
              (effect(clband x=(pep_star recent_avg_gift_amt
              frequency_status_97nk)) oddsratio (type=horizontalstat));
   class pep_star (param=ref ref='0');
   model target_b(event='1')=pep_star recent_avg_gift_amt
                  frequency_status_97nk / clodds=pl;
   effectplot slicefit(sliceby=pep_star x=recent_avg_gift_amt) / noobs; 
   effectplot slicefit(sliceby=pep_star x=frequency_status_97nk) / noobs; 
   score data=pmlr.pva_train out=work.scopva_train priorevent=&ex_pi1;
run;

title1 "Adjusted Predicted Probabilities of the Veteran's Organization Data";
proc print data=work.scopva_train(obs=10);
   var p_1 pep_star recent_avg_gift_amt frequency_status_97nk;
run;
title;

data work.pva(drop=CONTROL_NUMBER MONTHS_SINCE_LAST_PROM_RESP 
              FILE_AVG_GIFT FILE_CARD_GIFT);
   set pmlr.pva_raw_data;
run;

data work.pva;
   set work.pva;
   STATUS_FL=RECENCY_STATUS_96NK in("F","L");
   STATUS_ES=RECENCY_STATUS_96NK in("E","S");
run;

proc logistic data=work.pva plots(only)=(effect(clband
              x=(lifetime_card_prom recent_response_prop
                 months_since_last_gift       
                 recent_avg_gift_amt status_es)) 
                 oddsratio (type=horizontalstat)) 
                 namelen=25;
   model target_b(event='1')= lifetime_card_prom
         recent_response_prop months_since_last_gift
         recent_avg_gift_amt status_es / clodds=pl stb;
   units lifetime_card_prom=10 months_since_last_gift=6
         recent_avg_gift_amt=25 / default=1;
run;

/* chapter 3 */

data pmlr.pva_train_mi(drop=i);
   set pmlr.pva_train;
   /* name the missing indicator variables */
   array mi{*} mi_DONOR_AGE mi_INCOME_GROUP 
               mi_WEALTH_RATING;
   /* select variables with missing values */
   array x{*} DONOR_AGE INCOME_GROUP WEALTH_RATING;
   do i=1 to dim(mi);
      mi{i}=(x{i}=.);
      nummiss+mi{i};
   end;
run;

proc rank data=pmlr.pva_train_mi out=work.pva_train_rank groups=3;
   var recent_response_prop recent_avg_gift_amt;
   ranks grp_resp grp_amt;
run;

proc sort data=work.pva_train_rank out=work.pva_train_rank_sort;
   by grp_resp grp_amt;
run;

proc stdize data=work.pva_train_rank_sort method=median
            reponly out=pmlr.pva_train_imputed;
   by grp_resp grp_amt;
   var DONOR_AGE INCOME_GROUP WEALTH_RATING;
run;


options nolabel;
proc means data=pmlr.pva_train_imputed median;
   class grp_resp grp_amt;
   var DONOR_AGE INCOME_GROUP WEALTH_RATING;
run;
options label;

proc means data=pmlr.pva_train_imputed noprint nway;
   class cluster_code;
   var target_b;
   output out=work.level mean=prop;
run;

ods output clusterhistory=work.cluster;

proc cluster data=work.level method=ward 
             outtree=work.fortree
             plots=(dendrogram(horizontal height=rsq));
   freq _freq_;
   var prop;
   id cluster_code;
run;

proc freq data=pmlr.pva_train_imputed noprint;
   tables cluster_code*target_b / chisq;
   output out=work.chi(keep=_pchi_) chisq;
run;

data work.cutoff;
   if _n_=1 then set work.chi;
   set work.cluster;
   chisquare=_pchi_*rsquared;
   degfree=numberofclusters-1;
   logpvalue=logsdf('CHISQ',chisquare,degfree);
run;

title1 "Plot of the Log of the P-Value by Number of Clusters";
proc sgplot data=work.cutoff;
   scatter y=logpvalue x=numberofclusters 
           / markerattrs=(color=blue symbol=circlefilled);
   xaxis label="Number of Clusters";
   yaxis label="Log of P-Value" min=-40 max=0;
run;
title1; 

%global ncl;

proc sql;
   select NumberOfClusters into :ncl
   from work.cutoff
   having logpvalue=min(logpvalue);
quit;
proc tree data=work.fortree nclusters=&ncl 
          out=work.clus noprint;
   id cluster_code;
run;

proc sort data=work.clus;
   by clusname;
run;

title1 "Cluster Assignments";
proc print data=work.clus;
   by clusname;
   id clusname;
run;
filename clcode "&PMLRfolder/cluster_code.sas";

data _null_;
   file clcode;
   set work.clus end=last;
   if _n_=1 then put "select (cluster_code);";
   put "  when ('" cluster_code +(-1) "') cluster_clus='" cluster +(-1) "';";
   if last then do;
      put "  otherwise cluster_clus='U';" / "end;";
   end;
run;

data pmlr.pva_train_imputed_clus;
   set pmlr.pva_train_imputed;
   %include clcode;
run;

%global rho1_ex;

proc sql noprint;
   select mean(target_b) into :rho1_ex
   from pmlr.pva_train_imputed;
run; 

proc means data=pmlr.pva_train_imputed  
           sum nway noprint;
   class cluster_code;
   var target_b;
   output out=work.counts sum=events;
run;

filename clswoe "&PMLRfolder\swoe_cluster.sas";

data _null_;
   file clswoe;
   set work.counts end=last;
      logit=log((events + &rho1_ex*24)/
            (_FREQ_ - events + (1-&rho1_ex)*24));
   if _n_=1 then put "select (cluster_code);" ;
   put "  when ('" cluster_code +(-1) "') 
       cluster_swoe=" logit ";" ;
   if last then do;
      logit=log(&rho1_ex/(1-&rho1_ex));
      put "  otherwise cluster_swoe=" logit ";" / "end;";
   end;
run;

data pmlr.pva_train_imputed_swoe;
   set pmlr.pva_train_imputed;
   %include clswoe;
run;

title;

proc print data=pmlr.pva_train_imputed_swoe(obs=1);
   where cluster_code = "01";
   var cluster_code cluster_swoe;
run;
