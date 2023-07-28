proc import datafile="Guns_and_Crime.csv" out=gunsCrime replace; delimiter=',';
getnames=yes;
run;

data gunsCrime;
set gunsCrime;
drop ID; /* I removed as ID is pretty much a duplicate of Observation*/
dummyLaw = (law = "ye"); /*Law no = 0, Law ye = 1*/
sqrtCrime = sqrt(crime); */
/*drop law;*/
drop afam;
drop state; /*To use state there would have to be 50 cases/dummy variables*/
/*drop crime; */
run;
TITLE "Outliers and Influential points";
proc reg data = gunsCrime;
model Crime = year prisoners cauc male population income density dummyLaw / influence r; 
run;


Title "Full Regression Model for Crime";
proc reg corr;
model Crime = year prisoners cauc male population income density dummyLaw /vif;
RUN;

TITLE "Distribution of Crime";
PROC UNIVARIATE normal;
VAR crime;
histogram / normal (mu = est sigma = est);
RUN;


data gunscrime;
set gunscrime;
if _n_= 207 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 206 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 189 then delete;
run;


data gunscrime;
set gunscrime;
if _n_= 189 then delete;
run;


data gunscrime;
set gunscrime;
if _n_= 203 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 188 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 115 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 1005 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 1004 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 114 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 1002 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 1001 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 727 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 640 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 640 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 730 then delete;
run;

data gunscrime;
set gunscrime;
if _n_= 996 then delete;
run;


TITLE "Outliers and Influential points";
proc reg data = gunsCrime;
model sqrtCrime = year prisoners cauc male population income density dummyLaw / influence r; 
run;

TITLE "Distribution of Crime";
PROC UNIVARIATE normal;
VAR sqrtCrime;
histogram / normal (mu = est sigma = est);
RUN;

Title "Residual Plots";
proc reg;
model crime = year prisoners cauc male population income density dummyLaw;
/*Student Residual plot: vs pred. values */
plot student.*predicted.;
*normal probablity plot of residuals;
plot npp.*residual.;

run;

TITLE "Outliers and Influential points";
proc reg data = gunsCrime;
model Crime = year prisoners cauc male population income density dummyLaw / influence r; 
run;

Title "Full Regression Model for Crime";
proc reg corr;
model Crime = year prisoners cauc male population income density dummyLaw /vif;
RUN;

Title "Full Regression Model for Crime";
proc reg corr;
model Crime = year prisoners cauc male population income density dummyLaw;
RUN;


TITLE "Distribution of Crime";
PROC UNIVARIATE normal;
VAR crime;
histogram / normal (mu = est sigma = est);
RUN;

Title "Residual Plots";
proc reg;
model crime = year prisoners cauc male population income density dummyLaw;
/*Student Residual plot: vs pred. values */
/*plot student.*predicted.;

* normal probablity plot of residuals;
/*plot npp.*residual.;

run;

title "Correlation Values";
proc corr;
var crime year prisoners cauc male population income density dummyLaw;
run;

*/

TITLE "Outliers and Influential points";
proc reg data = gunsCrime;
model sqrtCrime = year prisoners cauc male population income density dummyLaw / influence r; 
run;

Title "Full Regression Model for Crime";
proc reg corr;
model sqrtCrime = year prisoners cauc male population income density dummyLaw /vif;
RUN;



TITLE "Distribution of Crime";
PROC UNIVARIATE normal;
VAR sqrtcrime;
histogram / normal (mu = est sigma = est);
RUN;

TITLE "Scatterplot Matrix";
PROC SGSCATTER;
MATRIX sqrtcrime year prisoners cauc male population income density dummyLaw;
RUN;

title "Correlation Values";
proc corr;
var sqrtcrime year prisoners cauc male population income density dummyLaw;
run;



Title "Residual Plots";
proc reg;
model sqrtcrime = year prisoners cauc male population income density dummyLaw;
/*Student Residual plot: vs pred. values */
plot student.*predicted.;

* normal probablity plot of residuals;
plot npp.*residual.;
run;

Title "Stepwise method";
proc reg;
model sqrtcrime = year prisoners cauc male population income density dummyLaw / selection = stepwise;
run;


TITLE "Final Model via stepwise";
proc reg data = gunsCrime;
model sqrtCrime = year prisoners cauc male population income density dummyLaw / influence r; 
run;

/*Splitting the data. Training set.*/
PROC SURVEYSELECT DATA= gunsCrime OUT = gunsSplit seed =450124
samprate = 0.75 outall;
run;

/* Print below to see what was selected for training and test. Select = 0 means test set. Select 1 = train.
proc print;
run;
*/

data gunsSplit;
set gunsSplit;
if selected then new_y = sqrtCrime;
run;
proc print data = gunsSplit;
run;


/*Using the training set to create model selection.*/
proc reg data = gunsSplit;
/*Model 1*/
model new_y = year prisoners cauc male population income density dummyLaw / selection = stepwise;
run;
/*Model 2*/
model new_y = year prisoners cauc male population income density dummyLaw / selection = cp;
run;

/*Using the test set to compute predicitve performance indicator*/
proc reg data = gunsSplit;
*Model 1;
model new_y = year prisoners cauc male population income density dummyLaw;
/*Contains predicted values for test set*/
output out=outm1(where=(new_y=.)) p=yhat;
*Model 2;
model new_y = year prisoners cauc population income dummyLaw;
output out=outm2 (where=(new_y=.)) p=yhat;
run;


Title "Comparing the Test set of both Models";
data outm1_sum;
set outm1;
d = sqrtCrime - yhat;
absd = abs(D);
run;

proc summary data = outm1_sum;
var d absd;
output out = outm1_stats std(d)=rmse mean(absd)=mae;
run;
proc print data=outm1_stats;
run;
proc corr data = outm1;
var sqrtCrime yhat;
run;

Title "Test model 2";
data outm2_sum;
set outm2;
d = sqrtCrime - yhat;
absd = abs(D);
run;

proc summary data = outm2_sum;
var d absd;
output out = outm2_stats std(d)=rmse mean(absd)=mae;
run;
proc print data=outm2_stats;
run;
proc corr data = outm2;
var sqrtCrime yhat;
run;

proc glmselect data = gunsCrime
	plots = (asePlot Criteria);
partition fraction(test = 0.25);
model sqrtCrime = year prisoners cauc male population income density dummyLaw / 
selection = stepwise (stop=CV) cvMethod = split(5) cvDetails = all;
run;


title "Compute Predictions";
data pred1;
input year prisoners cauc population income dummyLaw;
datalines;
1990 90 85 1 4500 1
;
proc print;
run;


data prediction1;
set pred1 gunsCrime; *combine both data sets *;
run;
proc print;
run;

proc reg;
model sqrtCrime =  year prisoners cauc population income dummyLaw / p clm cli;  *Values from final model*;
run;


title "Compute Predictions 2";
data pred2;
input year prisoners cauc population income dummyLaw;
datalines;
1975 250 65 3 9000 0
;
proc print;
run;


data prediction2;
set pred2 gunsCrime; *combine both data sets *;
run;
proc print;
run;

proc reg;
model sqrtCrime =  year prisoners cauc population income dummyLaw / p clm cli;  *Values from final model*;
run;


TITLE "Boxplot by Law";
PROC SORT;
BY dummyLaw;
RUN;
PROC BOXPLOT;
PLOT sqrtcrime*dummyLaw;
run;


TITLE "Final Model via cp";
proc reg corr;
model sqrtCrime =  year prisoners cauc population income dummyLaw  /vif;
RUN;

