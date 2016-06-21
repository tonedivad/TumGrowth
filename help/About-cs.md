
> **Overview**

<tumcode>TumGrowth</tumcode> cross-sectional functionality focuses at a single 
time point, *i.e.* only one measurement per animal is entering the analysis. It 
provides a convenient way to examine treatment effects along the time axis and 
an alternative approach to answer question at a time point of biological 
interest. In comparison to the other modules, `cross-sectional` analysis does 
not account for survival information or longitudinal correlation.

> **Settings**

Data selection:

1. treatment groups of interest;
2. response variable to <tumcode>TumGrowth</tumcode>;
3. transformation of the response variable: 
 * None, the raw measurement is used for the measurement;
 * Log, log-transformation of the raw measurement (zero's may be excluded, see 
parsing options `5`/`6`);
 * SqRt, for the square root of the raw measurement;
 * CuRt, for the cubic root of the raw measurement.
4. select the time range. By default, the slider is set time to the whole time range 
that covers the last measurement of each animal. To select a single time point, 
the two buttons must overlap;
5. choose whether the last observed measurement or the measurement with the largest value within 
the above selected time range enters the analysis.

Analysis settings:

1. outlier detection threshold - One round of outlier detection is performed to 
exclude automatically aberrant observations on the basis of 
Bonferroni-corrected p-value calculated from the residuals. <excode>Default: 
p<0.1</excode> is a not so stringent and reasonable cut-off. Exclusion of data points following these objective criteria must be mentioned in the description of the data set, usually in the Materials and Methods of scientific papers.
(ADD explicit removal in report);
2. check whether treatment-based weighting of the linear model must be assessed.

Visualisation settings:

1. back transform the data with the response has transformed for analysis;
2. set the minimum and maximum scale for the y-axis.

> **Output and interpretation**

Pairwise comparisons:
 * Differences in treatment means between pairs of treatement groups. Wilcoxon 
ranksum test is given for comparative purposes. General p-value adjustement 
performed by Holm method;
 * Interactions: TO BE ADDED in next release

Diagnostics plots:
 * `QQ-plot`: data points should lie as closely as possible to the straightline;
 * `Resid/Grp`, `Resid/Fit`: residuals of the models are plotted against the 
fitted values or across treatment groups.


Model information:
 * GLS model specification effect coeff matrix;
 * Weighting schema if heteroskedaticity was deemed reuqired;
 * Ids of any potentially excluded outliers;

Data points selection:
* A subpanel provids time point selection for each animal.


> **Implementation**

 Th on the 
basis of likelihood ratio test significant at *p<0.05*
