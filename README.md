TumGrowth
=============

Source code for the analysis of tumour growth experiments.

installation
------------

Before running the app you will need to have 1) R and 2) RStudio installed:
 - https://cran.rstudio.com/
 - https://www.rstudio.com/products/rstudio/download/
and have a few packages installed. For doing so, ppen RStudio and run these lines in the console:
 - install.packages(c("shiny","shinyFiles","shinythemes","shinyBS","devtools","RSVGTipsDevice","RColorBrewer"))
 - install.packages(c(,"car","coxphf","beeswarm","nlme","xtable","multcomp","survival","doBy"))
 - library(devtools)
 - install_github('ramnathv/rCharts')

To use the application, open RStudio and run this line in the console:
 - shiny:::runGitHub("TumGrowth", "tonedivad")

Your web browser will open the web app.
