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
Open RStudio and run this line in the console:
 - shiny:::runGitHub("TumGrowth", "tonedivad")

Your web browser will open the web app.

Optionnally, it can be used on the [ShinyappsIO website](https://tonedivad.shinyapps.io/TumGrowth/).

file format
------------
TumGrowth accepts tab delimited files. It is best to download an example to get a grasp on a *cleaned-up* structure. The formatting basics are as follows:

- First line must contain a **Grp** field that describe the treatment group and the days of sampling given in numerals. Any cell on the first line that do not correspond to a given experimental information (*Grp*, *Mid*, *Use*, *Surv*) or that it is not a number will not be considered;
- Second line contains the name of the variable associated to each time point. There will be as many measurement type as different names found under each potential time point. Columns that corresponding to a time point will be discarded if the cells on the second line are empty;
- Remaining lines:  raw measurements/information for each mice.

Optional columns on the first line:

- *Mid*: to specify conveniently the Id for each animal. Default: *Grp* plus a number;
- *Use*: if not empty, the animal will not be used for graphing and analyses but the data remains available for export. Default: all lines are considered;
- *Surv*: if anything other than empty, the animal is considered to have survived at the last recorded time point. Default: none have escaped.

The parsing takes care of the comma/dot issue to get the number rights but anything other than this will be considered as NA. Results/graphes are presented in the order of the groups in the original file. Animal ids may be amended to avoid any duplicates.
