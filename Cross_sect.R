############################################################
### functions for CS analysis
# getCSmat: prepare data
# plotCS: boxplot in server
# compCS: compute CS model
# formatCSpw: format CS for the reactive
# prepDiagCS: prepare data for diagnostic plots
############################################################


############################################################
## format the data
getCSmat<-function(cdat,resp=cdat$Resp[1],lgrps=levels(cdat$dataM$Grp),gcols=getCols(cdat$dataM$Grp),
                   trans='None',rangetp=range(cdat$data$Tp),usemax=FALSE){
  

  lmids=cdat$dataM$Id[cdat$dataM$Use & cdat$dataM$Grp%in%lgrps]
  iresp=resp
  if(trans!='None') iresp=paste(resp,tolower(trans),sep=".")
  

  l=which(cdat$data$Id%in%lmids & !is.na(cdat$data[,iresp]) & cdat$data$Tp>=min(rangetp)  & cdat$data$Tp<=max(rangetp))
  
  df=cdat$data[l,c("Id","Tp",iresp,resp)]
  names(df)=c("Id","Tp","Resp","Resp_ori")
  df=df[order(df$Id,df$Tp),]
  l2use=tapply(1:nrow(df),df$Id,function(x) rev(x)[1])
  if(usemax) l2use=tapply(1:nrow(df),df$Id,function(x) x[which.max(df$Resp[x])])
  df=df[l2use,]
  df$Grp=factor(cdat$dataM[df$Id,]$Grp)
  df$color=gcols[as.character(df$Grp)]
  newrange=range(df$Tp)
  if(newrange[1]<newrange[2]) ylab=paste(ifelse(usemax,"Max. r","R"),"esponse in time range: ",newrange[1],"-",newrange[2],sep="")
  if(newrange[1]==newrange[2]) ylab=paste("Response at time=",newrange[1],sep="")

  return(list(Df=df,Resp=resp,Trans=trans,Par=ylab))
}
############################################################################################

log_breaks <- function(n = 7) {
  function(x) {
    rng <- log10(range(x, na.rm = TRUE))
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    
#     if (max == min) return(10^(min))
    
    by <- floor((max - min) / n) + 1
    yaxt=10^(seq(min, max, by = by))
    if(length(yaxt)>=n) return(yaxt)
    
    yaxtn=sort(rep(yaxt,2)*rep(c(1,2),each=length(yaxt)))
    l=range(which(log10(yaxtn)>rng[1] & log10(yaxtn)<rng[2]))
    l[1]=max(1,l[1]-1);l[2]=min(length(yaxtn),l[2]+1)
    yaxtn=yaxtn[l[1]:l[2]]
    if(length(yaxtn)<n){
      yaxtn=sort(rep(yaxt,3)*rep(c(1,2,5),each=length(yaxt)))
      l=range(which(log10(yaxtn)>rng[1] & log10(yaxtn)<rng[2]))
      l[1]=max(1,l[1]-1);l[2]=min(length(yaxtn),l[2]+1)
      yaxtn=yaxtn[l[1]:l[2]]
    }
    yaxtn
  }
}


sqrt_breaks <- function(n = 7) {
  function(x) {
    rng <- sqrt(range(x, na.rm = TRUE))
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    
    #     if (max == min) return(10^(min))
    
    by <- floor((max/min) / n) + 1
    yaxt=(seq(min, max, by = by))^2
    if(length(yaxt)>=n) return(yaxt)
    
    yaxtn=sort(rep(yaxt,2)*rep(c(1,2),each=length(yaxt)))
    l=range(which(log10(yaxtn)>rng[1] & log10(yaxtn)<rng[2]))
    l[1]=max(1,l[1]-1);l[2]=min(length(yaxtn),l[2]+1)
    yaxtn=yaxtn[l[1]:l[2]]
    if(length(yaxtn)<n){
      yaxtn=sort(rep(yaxt,3)*rep(c(1,2,5),each=length(yaxt)))
      l=range(which(log10(yaxtn)>rng[1] & log10(yaxtn)<rng[2]))
      l[1]=max(1,l[1]-1);l[2]=min(length(yaxtn),l[2]+1)
      yaxtn=yaxtn[l[1]:l[2]]
    }
    yaxtn
  }
}



############################################################################################
### boxplot in server
plotCS<-function(objres,miny=NA,maxy=NA,retplot=TRUE){
  
  idf=objres$Df
  gcols=tapply(idf$color,idf$Grp,unique)
  if(is.na(miny)) miny=min(idf$Resp,na.rm=TRUE) 
  if(is.na(maxy)) maxy=max(idf$Resp,na.rm=TRUE) 
  ylim=c(miny,maxy)
  yaxt=pretty(ylim)
  
  idf=idf[order(idf$Grp,idf$Id),]
  idf$x2=idf$x=as.numeric(idf$Grp)
  for(i in names(which(table(idf$x)>1)))
    idf$x2[idf$x==i]=swarmx(idf$x[idf$x==i],idf$Resp[idf$x==i],xsize=.08,ysize=.08,cex=2)[,1]
  
  bwstats = setNames(as.data.frame(boxplot(Resp ~ Grp, data = idf, plot = FALSE)$stats), nm = NULL)
  bwstats=boxplot(Resp ~ Grp, data = idf, plot = FALSE)$stats
  bwdat=lapply(1:ncol(bwstats),function(i)
    list(x=i-1,low=bwstats[1,i],q1=bwstats[2,i],median=bwstats[3,i],q3=bwstats[4,i],high=bwstats[5,i],
         color=unname(gcols[i]),fillColor= '#F0F0E0',lineWidth= 2,
         medianColor=gcols[i],medianWidth=4,fillColor= "#F0F0F0"))
  
  bwpts=lapply(1:nrow(idf),function(i) 
    list(name=idf$Id[i],color=unname(idf$color[i]),type='scatter',
         data=list(list(x=idf$x2[i]-1,y=idf$Resp[i],grp=idf$Grp[i],Id=idf$Id[i],radius=8)),
         tooltipText=paste(idf$Id[i],':',idf$Resp[i]),marker=list(symbol='circle')))
  
  title=objres$Par
  if(objres$Trans!="None") title=paste(title," (",objres$Trans,")",sep="")
  if(!retplot) return(list(m=idf,ylim=ylim,yaxt=yaxt,title=title))
  
  b <- Highcharts$new()
  b$set(series = append(list(list(name =objres$Resp,data = bwdat,tooltip=list(enabled=FALSE))),bwpts))
  b$xAxis(labels=list(rotation = -90),categories = levels(idf$Grp),title = list(text = 'Groups'))
  b$xAxis(categories = paste(levels(idf$Grp)," (",table(idf$Grp),")",sep="")
          ,title = list(text = 'Groups'))
  b$legend(enabled = FALSE)
  b$tooltip( shared=FALSE,formatter = "#! function() { return this.series.options.tooltipText ; } !#")
  b$plotOptions(followPointer=FALSE)
  b$yAxis(title = list(text = objres$Par), min = min(yaxt), max = max(yaxt), tickPositions = yaxt)
  b$chart(type = 'boxplot')
  return(list(plot=b,m=idf,ylim=ylim,yaxt=yaxt,title=title))
}


##########################################################################################
compCS<-function(objres,bfco=0.1,checkvar=TRUE){
  library(nlme)
  idf=objres$Df
  idf=idf[idf$Grp %in% names(which((table(idf$Grp)>1))),]
  idf$Grp=factor(idf$Grp)
  idf$out=FALSE
  if(nlevels(idf$Grp)<2) return(NULL)

  #####
  mod=gls(Resp~Grp,data=idf,method="REML")
  if(checkvar){
    modw=update(mod,weights=varIdent(form=~1|Grp))
    if(AIC(modw)<AIC(mod)){
      ll1=logLik(mod)
      ll2=logLik(modw)
      if((1-pchisq(as.numeric(ll2-ll1)*2,attr(ll2,"df")-attr(ll1,"df")))<0.05) mod=modw
    }
  }
  
  resid=residuals(update(mod,method="ML"),'pearson')
  names(resid)=rownames(idf)
  out=outlierTest(lm(resid~1),cutoff = bfco)
  if(out$signif){
    idf[names(out$rstudent),]$out=TRUE
    mod=gls(Resp~Grp,data=idf[!idf$out,],method="REML")
    if(checkvar){
      modw=update(mod,weights=varIdent(form=~1|Grp))
      if(AIC(modw)<AIC(mod)){
        ll1=logLik(mod)
        ll2=logLik(modw)
        if((1-pchisq(as.numeric(ll2-ll1)*2,attr(ll2,"df")-attr(ll1,"df")))<0.05) mod=modw
      }
    }
  }
  modfin=update(mod,.~.,method="ML")
  modfin0=update(mod,.~1,method="ML")
  
  ##################
  # compute tests
  lrt=2*(modfin$logLik-modfin0$logLik)
  ndf=nlevels(idf$Grp)-1
  lrtpv=1-pchisq(lrt,ndf)
  lrt=sprintf("%.2f  (d.f.=%d), p<%s",lrt,ndf,.myf(lrtpv))
#   wald=Anova(modfin,test.statistic="Chisq")[1,2]
#   waldpv=1-pchisq(wald,ndf)
#   wald=sprintf("%.2f  (d.f.=%d), p<%s%s",wald,ndf,.myf(waldpv),.myfpv(waldpv))
  Ftest=anova(modfin)[2,2]
  Ftestpv=1-pchisq(Ftest,ndf)
  Ftest=sprintf("%.2f  (d.f.=%d), p<%s",Ftest,ndf,.myf(Ftestpv))
  npt=NA
  if(ndf==1){
    kw=suppressWarnings(wilcox.test(Resp~Grp,idf[!idf$out,]))
    npt=sprintf("p<%s",.myf(kw$p.value))
  }
  if(ndf>1){
    kw=suppressWarnings(kruskal.test(Resp~Grp,idf[!idf$out,]))
  npt=sprintf("%.2f  (d.f.=%d), p<%s",kw$statistic,kw$parameter,.myf(kw$p.value))
  }
  modtab=cbind(lrt,Ftest,npt)
  dimnames(modtab)=list(c("Treat" ),c("Likelihood ratio test","F test",
                                      ifelse(ndf>1,"Kruskal-Wallis","Wilcoxon RankSum test")))
  
  ##################
  # compute contrasts
  ctm=contrMat(table(idf$Grp),type="Tukey")
  ctm[,1]=0
  ctres=glht(modfin,linfct=ctm)
  jtab=data.frame(cbind(confint(ctres)$confint[,1:3,drop=F],summary(ctres)$test$pvalues))
  jtab=cbind(do.call("rbind",strsplit(rownames(ctm)," - ")),jtab,stringsAsFactors=F)
  names(jtab)=c("Grp1","Grp2","Contrast","Lower","Upper","Pvalue" )
  rownames(jtab)=NULL

  jtab$Wil=suppressWarnings(sapply(strsplit(rownames(ctm)," - "),function(x) 
    wilcox.test(Resp~factor(Grp),idf[idf$Grp%in%x & !idf$out,])$p.value))

  ##################
  # get model infos
  tab=nlme:::summary.gls(modfin)$tT
  rownames(tab)=gsub("^Grp=","",rownames(tab))
  tab=data.frame(tab)
  tab[,4]=sapply(tab[,4],function(x) sprintf("%.5f",x))
  for(i in 1:3) tab[,i]=sapply(tab[,i],function(x) sprintf("%.3f",x))
  
  
  weightCoef=NULL
  mstruct=coef(modfin$modelStruct,unconstrained=FALSE)
  if(!is.null(mstruct))
    if(any(grepl("^varStruct",names(mstruct)))){
    weightCoef=mstruct[grep("^varStruct",names(mstruct))]
    names(weightCoef)=gsub("^varStruct\\.","",names(weightCoef))
    weightCoef=weightCoef[levels(idf$Grp)]
    weightCoef[is.na(weightCoef)]=1
    names(weightCoef)=levels(idf$Grp)
   }
  
  newdf=expand.grid(Grp=levels(idf$Grp))
  newdf=cbind(newdf,data.frame(predictSE.gls(modfin,newdf,se=T)))
  names(newdf)[2]="fit"


  return(list(model=modfin,coefTab=tab,pairTab=jtab,AnovaTab=modtab,weightCoef=weightCoef,
              data=idf,pred=newdf,Resp=objres$Resp,Trans=objres$Trans))
}

##############
## format pairwise
formatCSpw<-function(csres,ref='All',padjust="holm",backtrans=F){
  
  if(padjust=='no') padjust="none"

  btfct<-function(x) x;collab="Difference"
  if(csres$Trans=="Log" & backtrans){btfct<-function(x) exp(x);collab='Ratio'}
#  if(csres$Trans=="Sqrt" & backtrans) btfct<-function(x) x^2
  
  if(length(ref)==1) if(ref=='All') ref=levels(csres$data$Grp)
  ref=ref[ref%in%levels(csres$data$Grp)]
  if(length(ref)==0) ref=levels(csres$data$Grp)
  pwt=csres$pairTab
  if(length(ref)>0)  pwt=pwt[which(pwt[,1]%in%ref | pwt[,2]%in%ref),]
  if(length(ref)==1){
    if(any(pwt[,1]%in%ref)){
      for(i in which(pwt[,1]%in%ref)){
        pwt[i,1:2]=pwt[i,2:1]
        pwt[i,4:5]=-pwt[i,5:4]
        pwt[i,3]=-pwt[i,3]
      }
    }
  }
  
  dcts=apply(as.matrix(pwt[,3:5]),1,function(x) sprintf("%.3f [%.3f;%.3f]",btfct(x[1]),btfct(x[2]),btfct(x[3])))
  
  top=data.frame(pwt[,1:2],dcts,
                 Pvalue=.myf(pwt$Pval),PvalueAdj=.myf(p.adjust(pwt$Pval,padjust)),
                 WilcoxPvalue=.myf(pwt$Wil),WilcoxPvalueAdj=.myf(p.adjust(pwt$Wil,padjust)),stringsAsFactors=F)
  names(top)[3]=collab
  if(length(ref)==1){
    top=top[,-2]
    colnames(top)[1]=paste("Comp to ",ref)
  }
  return(top)
  
}


################################################################################################
### Prep diagplots

prepDiagCS<-function(csres){
  
  gcols=tapply(csres$data$color,csres$data$Grp,unique)
  resp=csres$Resp
  btfct<-function(x) x
  if(csres$Trans=="Log"){btfct<-function(x) exp(x)}
  if(csres$Trans=="Sqrt") btfct<-function(x) x^2
  if(csres$Trans=="CuRt") btfct<-function(x) x^3
  
   f<-function(x) {min(which( x*10^(0:20)==floor(x*10^(0:20)) )) - 1} 
  
   ###############
   ## format data
   tmpdata=csres$data[!csres$data$out,c("Resp","Resp_ori","Grp","Tp","Id")]
  #  names(tmpdata)=c("y","Grp","x","Id")
  ndigy=ceiling(median(sapply(tmpdata$Resp_ori,f)))+1
  ndigye=ceiling(median(sapply(tmpdata$Resp,f)))+1
  tmpdata$Id=factor(tmpdata$Id,levels=unique(tmpdata$Id))
  tmpdata$color=gcols[as.character(tmpdata$Grp)]
  tmpdata$Resp=round(tmpdata$Resp,ndigye)
  tmpdata$Fit=round(csres$model$fitted,ndigye)
  tmpdata$Resp_ori=round(tmpdata$Resp_ori,ndigy)
  tmpdata$Fite=round(btfct(csres$model$fitted),ndigy)
  tmpdata$resid=round(unclass(nlme:::residuals.gls(csres$model, "pearson")),ndigye+2)
  lso=order(order(tmpdata$resid))
  tmpdata$qt=qnorm(ppoints(length(lso)))[lso]
  
  ###############
  ## format predcited data
  
  tmppred=csres$pred[,c("Grp","Grp","fit","se.fit")]
  names(tmppred)=c("x","Grp","fit","se.fit")
  tmppred$x=as.numeric(tmppred$x)
  tmppred$color=gcols[as.character(tmppred$Grp)]
  tmppred$y=round(tmppred$fit,ndigye)
  tmppred$ymin=round(tmppred$fit-1.96*tmppred$se.fit,ndigye)
  tmppred$ymax=round(tmppred$fit+1.96*tmppred$se.fit,ndigye)
  tmppred$ybt=round(btfct(tmppred$fit),ndigy)
  tmppred$ybtmin=round(btfct(tmppred$fit-1.96*tmppred$se.fit),ndigy)
  tmppred$ybtmax=round(btfct(tmppred$fit+1.96*tmppred$se.fit),ndigy)
  tmppred$Grp=factor(tmppred$Grp,levels=levels(tmpdata$Grp))
  
  ###############
  ## format axes
  limtp=pretty(tmpdata$Tp)
  limyfit=pretty(c(tmppred$ybtmin,tmppred$ybtmax,tmpdata$Resp_ori,tmpdata$Fite)) ## original response/backT data
  limyfit0=pretty(c(tmppred$ymin,tmppred$ymax,tmpdata$Resp,tmpdata$Fit)) ## model fit
  limxqq=range(c(tmpdata$resid,tmpdata$qt))
  limxqq=floor(limxqq[1]):ceiling(limxqq[2])
  
  return(list(data=tmpdata,pred=tmppred,limtp=limtp,limyfit=limyfit,limyfit0=limyfit0,limxqq=limxqq,Type="cs",Resp=csres$Resp,Trans=csres$Trans))
}

