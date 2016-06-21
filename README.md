TumGrowth
=============

Source code for the analysis of tumour growth experiments.

installation
------------

Before running the app you will need to have 1) R and 2) RStudio installed:

- https://cran.rstudio.com/
- https://www.rstudio.com/products/rstudio/download/
 
and have a few packages installed. For doing so, open RStudio and run these lines in the console:

- install.packages(c("shiny","shinyFiles","shinythemes","shinyBS","devtools"))
- install.packages(c("beeswarm","car","coxphf","nlme","multcomp","survival","doBy","rmarkdown","rms"))
- devtools::install_github("hadley/svglite") ## necessary for exporting SVGs
- devtools::install_github('ramnathv/rCharts')
- devtools::install_github('rstudio/DT')
- devtools::install_github("daattali/shinyjs")
 
usage
------------
Open RStudio and run this line in the console: ``shiny:::runGitHub('TumGrowth','kroemerlab')``

Your web browser will open the web app.

Optionnally, it can be used on the [ShinyappsIO website](https://kroemerlab.shinyapps.io/TumGrowth/).

file format
------------
```TumGrowth``` accepts tab delimited files. It is best to download an example to get a grasp on a *cleaned-up* structure. The formatting basics are as follows:

- First line must contain a **Grp** field that describe the treatment group and the days of sampling given in numerals. Any cell on the first line that do not correspond to a given experimental information (*Grp*, *Mid*, *Use*, *Surv*) or that it is not a number will not be considered;
- Second line contains the name of the variable associated to each time point. There will be as many measurement type as different names found under each potential time point. Columns that corresponding to a time point will be discarded if the cells on the second line are empty;
- Remaining lines:  raw measurements/information for each mice.

Optional columns on the first line:

- *Mid*: to specify conveniently the Id for each animal. Default: *Grp* plus a number;
- *Use*: if not empty, the animal will not be used for graphing and analyses but the data remains available for export. Default: all lines are considered;
- *Surv*: if anything other than empty, for the animal to have survived on its last recorded time point (see point 6 below). Default: none have escaped.

The parsing takes care of the comma/dot issue to get the numbers right but anything other than this will be considered as NA. Graphes/calculations are done using the order of the groups in the original file. Animal ids may be amended to avoid duplication: one line=one animal.

*Small working example:* 

| Grp | Use | Surv | Mid  | 0 | 5 | 10 | 15 | 15 | 20 | d25 |
| --- | --- | ---  | --- |--- |--- |--- |--- |--- |--- |--- |
|  what | ever  |  is | here | V | V | V | V|  W |V | V|
| PBS | x | x  | animal1 | 2 | 4  | 7  | 8 | 500 | 14| 1
| PBS  | x|  x | animal2  | 2.5 | 10.3 | 15.6  |15.6| 432 |15.6 | like |
| PBS  | x|   | animal3  | 0 | 14.3 | 25.6  |65.6| 432 |85.1 |  |
| Drug | x | x | animal4 | 5 | 10 | 15 | 23| 147| 60 | b10
| Drug | x |  | animal5 | 2.6 | 5.4  | 9.7  | 0 | 285| eighty | 0 |
| Drug  | x| x | animal6 | 0.6 | 0  | 0.7  |  0 | 120 | 0 | 613 |

This is interpreted as: 

- Some measures named *V* has been recorded at 2/5/10/15/20 and another one 
named *W* at 15 (first 2 rows, mandatory);
- 3 *PBS* abd 3 *Drug*-treated mice (**Grp**, mandatory column);
- Animals are called *animal1*,*animal2* etc... (**Mid**, optional column);
- Use them all for graphing and stats (**Use**, optional column);
- Last recorded day is censored for *animal3* (**Surv**, optional column);
- Measurements in *V* at day 15 and 20 are not valid for *animal5*;
- ```TumGrowth``` is not interested in the last column as the first 
cell is not a number.






