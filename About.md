
Guided steps for using and perhaps understanding ```Tumgrowth```:

- [Data upload](#data-upload)
- [Line charts](#line-charts)
- [Cross-sectional analysis](#csanal)
- [Longitudinal analysis](#longitudinal-analysis)
- [Survival analysis](#survival-analysis)


### **Data upload**

For historical reasons down to pre-filled Excel sheet being firmly in place at the kroemerlab, the parsing proceeds as follows:

0. Num. of digits: rounding used for import and exporting data;
1. Remove zeros from the end: if ticked, measurements at days **15** and **20** will be excluded for *animal6* but not at day 5;
2. Trimming identical values from the end: if ticked, measurements at days **15** and **20** will be excluded for *animal2*.
This mouse will then be considered to have survived until time point **10** ;
3. Shifting time couses to the left: the first non zero measurement will be set to the value given in the box. In the table above, nothing will change for all animals excepted *animal3* for which *V* at day 0 will be 14.3, at day 5, 25.6 etc...;
4. Exclude any other zeros: should be zeros considered NA;
5. For log-transformation, zeros (if any left) are imputed by the minimum value divided by 2;
6. This applies is **Surv**  is not specified in the original file. By default, animals are considered dead on their last recorded day. Option ```6``` overwrites this by censoring (*i.e. alive*) animals if there is a valid measurement at the **latest** day of the experiment. For more subtile or complex situations, it is best to specifiy **Surv** properly in the input text file.

### **Line charts**

### **Cross-sectional analysis**

### **Longitudinal analysis**


Advantages of linear mixed effects modelling

#### Settings


#### Output



### Survival analysis

Survival analysis is aimed at visualising proportion of surviving or tumor-free animals at all experimental time points and to estimate event rates or hazard ratios between treatment groups. Other than to time to event endpoints, this is also applicable to heterogenous experiments including tumour recession.

#### Settings

'``TumGrowth``` deals with right-censored data for which follow-up time and associated event can be approached from two angles, labelled as overall survival (**OS**) and tumor-free survival (**TFS**): 

* **OS**: for each animal, it uses the last recorded day for which the tumor size is less or equal than a given threshold (slider: *censoring on response*, default: maximum size). After this day, all measurements are strictly greater than the specified size. Additionally, the maximum duration of the experiment can be adjusted (slider: *censoring on time*, default: latest day of the experiment). If the follow up time coincides with the last animal recorded day, event occured for animals known to have died and censoring applied for animals known to have survived (see **Surv** column in the file or parsing option ```6```) 
* **TFS**: for each animal, it uses the last recorded time-point before the tumor first exceeds a given threshold (slider:  *tumor detection level*, default: 0). Up to this day, all measurements are less than or equal to the threshold. and If the tumour size is less (or equal) than the detection limit, follow-up time is the last recorded day with no event (i.e. right censoring). If the tumour size is always greater than the detection limit,  follow-up time is the first recorded day with event. Note that this mode do not use the information from the **Surv** column or from parsing option ```6```.

#### Output

Kaplan-Meier curves and follow-up times and associated censoring are automatically updated in the ```Kaplan Meier``` and ```Censoring information``` subpanels.

Statistical analyses rely on the Cox regression model (```rms:::cph```) and Firth biased adjusted Cox regression (```coxphf:::coxphf```). The later is slower but is adequate to overcome issues due to groups with very few events. Computations are enabled as soon as a baseline group for estimating the hazard ratio is selected. Likelihod ratio, Wald and score test are used to assess differences in survival rates between all the groups whereas pairwise comparisons confidence interval are based on the Wald method or profile likelihood if ```Firth``` is enabled. In addition, non parametric statistics from the log-rank (Mantel-Cox) test as given for the overall model (i.e. score test) and each comparison.


