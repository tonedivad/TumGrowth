####################################################
# Loading libraries
library(shinyFiles)
library(shiny)
library(rCharts)

library(nlme)
library(xtable) ##keep
library(multcomp)
library(survival)
library(car)
library(coxphf)
library(beeswarm)
library(DT)

## add?
library(rms)
library(shinyjs)
library(rmarkdown)
library(AICcmodavg)
library(lme4)
library(lmerTest)
library(doBy)
library(pander)
library(markdown)
library(mgcv)


##########################################################################
## Formatting p-values
.myf<-function(pvs,n=4){
  
  myf<-function(pv,n=4){
    if(pv<(10^-n)) return(paste(c("<0.",rep(0,n-1),"1"),collapse=""))
  return(sprintf(paste("%.",n,"f",sep=""),pv)) 
  }
  sapply(pvs,myf,n=n)
}

.myfpv<-function(pvs){
  pvt=rep("",length(pvs))
  pvt[which(pvs<=0.1)]="."
  pvt[which(pvs<=0.05)]="*"
  pvt[which(pvs<=0.01)]="**"
  pvt[which(pvs<=0.001)]="***"
  pvt
}

##########################################################################
## Add alpha to colors
add.alpha=function (hex.color.list, alpha) 
  sprintf("%s%02X", hex.color.list, floor(alpha * 256))

##########################################################################
## Default colors for running TG from source
getCols<-function(grp,mid=NULL,what=4){
  require(RColorBrewer)
  pals=list(rev(brewer.pal(9,"Greens")[c(4,7)]),
            rev(brewer.pal(9,"Blues")[c(5,8)]),
            rev(brewer.pal(9,"Reds")[c(4,8)]),
            rev(brewer.pal(9,"Purples")[c(5,7)]),
            rev(brewer.pal(9,"Greys")[c(4,7)]),
            rev(brewer.pal(9,"Oranges")[c(4,7)]),
            brewer.pal(11,"BrBG")[c(3,2)],
            brewer.pal(11,"BrBG")[c(10,9)])
  pals=c(pals,pals)
  pals0=sapply(pals,function(x) rev(colorRampPalette(x)(7))[what])
  lugrp=unique(grp)
  if(is.null(mid)){
    lcols=pals0[1:length(lugrp)]
    names(lcols)=lugrp
    return(lcols)
  }
  
  unlist(lapply(1:length(lugrp),function(x){
    lmids=unique(mid[grp==lugrp[x]])
    lcols=colorRampPalette(pals[[x]])(length(lmids))
    names(lcols)=lmids
    lcols
  }))
  
}



################################################################################################
# mygenerateOptions<-function (inputId, choices, selected, inline, type = "checkbox",taglab=NULL) 
# {
#   options <- mapply(choices, names(choices), FUN = function(value, 
#                                                             name) {
#     inputTag <- tags$input(type = type, name = inputId, value = value)
#     if (value %in% selected) 
#       inputTag$attribs$checked <- "checked"
#       typ0=ifelse(inline,paste0(type, "-inline"),type)
#       if(is.null(taglab)) return(tags$label(class = typ0, inputTag, tags$span(name)))
#       if(!is.null(taglab)) return(tags$label(class = typ0,`for`=taglab, inputTag, tags$span(name)))
#   }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
#   div(class = "shiny-options-group", options)
# }
