> **Overview**

Survival analysis is a conventional method for analyzing *in vivo* experiments 
by means of comparing time to endpoints between experimental groups. Endpoints can be automatically and 
interactively adjusted based on tumor size and/or length of the experiment, see 
below. Event rates and hazard ratios between treatment groups are estimated accordingly.

> **Settings**

Data selection:

1. treatment group of interests;
2. response variable in their original scale.

Modelling:

<tumcode>TumGrowth</tumcode> deals with right-censored data for which follow-up 
time (last observed time per animal) and associated events can be defined following two modes, so called overall survival (**OS**) and tumor-free survival (**TFS**): 

1. `OS`: for each animal, it uses the last recorded day for which the tumor size 
is **less or equal** than a given threshold (`censoring on response`, 
<excode>default: maximum size</excode>). After this day, all measurements are 
strictly greater than the specified size. Additionally, the maximum duration of 
the experiment can be adjusted (`censoring on time`, <excode>default: latest day 
of the experiment</excode>). If the follow up time coincides with the last 
animal recorded day, the software considers that an event has occurred for animals known to have died or that the time point is censored for animals known to have survived (see **Surv** column in the file or 
parsing option `7`) 
2. `TFS`: for each animal, it uses the last recorded time-point **before** the tumor 
first exceeds a given threshold (`tumor detection level`, <excode>default: 
0</excode>). Up to this day, all measurements are less than or equal to the 
threshold. If the tumour size is always smaller or equal than the detection limit, 
follow-up time is the last recorded day with no event (i.e. right censoring). If 
the tumor size is always greater than the detection limit, follow-up time corresponds to  
the first recorded day with event. Note that this mode do not use the 
information from the **Surv** column or from parsing option `7`.

A sub-panel `Censoring information` gives the explicit follow-up times/censoring distribution for each animal (see below).


Visualization/export:

1. The shifting factor controls artificially alters the plot along follow-up time to avoid the possible 
the overlap between KM survival curves. It does not have any impact on the outcome of the statistical analysis;
2. Line width/size/point size controls the KM curves control the rendering of the KM plots. 

> **Output and interpretation**

Kaplan Meier:
* Survival curves are updated together with both data selection and 
definition of follow-up times and right-censoring. 
* Points are graphed at each breakpoint. Mouse hovering highlights 
the percentage of surving animals and in parentheses the number of subjects at 
risk followed by the number of events and censors.

Modeling:
* Statistical analyses rely on the Cox regression model and Firth 
biased adjusted Cox regression. The later is slower and is useful to obtain hazard ratio estimates when the number events is quite small or null. For both, computations are 
enabled as soon as a baseline group (comparator) for estimating the hazard ratio is selected;
* Likelihod ratio, Wald and score tests are used to assess differences in 
survival rates to the baseline experimental group.  Confidence intervals are based on the Wald method or profile likelihood if `Firth` is selected. In addition, non parametric statistics from the log-rank 
(Mantel-Cox) test is given for the overall model (i.e. score test) and each pairwise
comparison. 

Censoring information:
* A subpanel is provided to explicitly enumerate follow-up time and censoring 
information for each treatment group. The number of events (`Event`) is given 
alongside the median survival if reached (`MedSurv`) and the median follow-up 
(`MedFUP`) both calculated from the standard (reverse) Kaplan-Meier method.


> **Implementation**

* `survival:::survmean`: median survival/follow-up calculations
* `rms:::cph` Cox regression modeling and log-rank statistics
* `coxphf:::coxphf` Firth-bias corrected Cox regression
