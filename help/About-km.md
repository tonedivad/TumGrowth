> **Overview**

Survival analysis is a conventional method for analyzing *in vivo* experiments by means of comparing time to endpoinds. Endpoints are automatically and interactively based on tumour exceeding a specified threshold, see below. Assuming proportional hazards across groups, event rates or hazard ratios between treatment groups are estimated accordingly.


> **Settings**

Data selection:

1. treatment group of interests;
2. response variable in their orginal raw format.

Modelling:

<tumcode>TumGrowth</tumcode> deals with right-censored data for which follow-up time (last observed time per animal) and associated event are defined labelled as overall survival (**OS**) and tumor-free survival (**TFS**): 

1. `OS`: for each animal, it uses the last recorded day for which the tumor size is less or equal than a given threshold (`censoring on response`, <excode>default: maximum size</excode>). After this day, all measurements are strictly greater than the specified size. Additionally, the maximum duration of the experiment can be adjusted (`censoring on time`, <excode>default: latest day of the experiment</excode>). If the follow up time coincides with the last animal recorded day, event occured for animals known to have died and censoring applied for animals known to have survived (see **Surv** column in the file or parsing option `7`) 
2. `TFS`: for each animal, it uses the last recorded time-point before the tumor first exceeds a given threshold (`tumor detection level`, <excode>default: 0</excode>). Up to this day, all measurements are less than or equal to the threshold. and If the tumour size is less (or equal) than the detection limit, follow-up time is the last recorded day with no event (i.e. right censoring). If the tumour size is always greater than the detection limit,  follow-up time is the first recorded day with event. Note that this mode do not use the information from the **Surv** column or from parsing option `7`.

Visualisation:

1. The shifting factor controls along the shift along follow-up time to avoid the overlap of the survival curves;
2. Lwd is scaling the width of the lines. It is noticeable when exporting the KM curves as images. 

> **Output and interpretation**

Kaplan Meier:
* Survival curves are updated together with the data selection and the definition of follow-up times and associated censoring. 
* Points are graphed at each breakpoint. Mouse hovering on each point highlights the percentage of surving animals and in parentheses the number of subjects at risk followed by the number of events and censors.

Modelling:
* Statistical analyses rely on the Cox regression model (`rms:::cph`) and Firth biased adjusted Cox regression (`coxphf:::coxphf`). The later is slower and required to overcome issues due to groups with very few events. Computations are enabled as soon as a baseline group for estimating the hazard ratio is selected;
* Likelihod ratio, Wald and score tests are used to assess differences in survival rates between all the groups whereas for pairwise comparisons, confidence intervals are based on the Wald method or profile likelihood if `Firth` is enabled. In addition, non parametric statistics from the log-rank (Mantel-Cox) test is given for the overall model (i.e. score test) and each comparison. 

Censoring information:
* A subpanel is provided to explicitly enumerate follow-up time and censoring information for each treatment group. The number of events (`Event`) is given alongside the median survival if reached (`MedSurv`) and the median follow-up (`MedFUP`) both calculated from the standard Kaplan-Meier method.

