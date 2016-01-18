
getCSmat<-function(df,dfm,resp=names(df)[1],lgrps=levels(dfm$Grp),rangetp=range(df$Tp),usemax=FALSE){
  

  lmids=dfm$Id[dfm$Use & dfm$Grp%in%lgrps]
  df$Resp=df[,resp]
  l=which(df$Id%in%lmids & !is.na(df$Resp) & df$Tp>=min(rangetp)  & df$Tp<=max(rangetp))
  
  df=df[l,]
  df=df[order(df$Id,df$Tp),]
  l2use=tapply(1:nrow(df),df$Id,function(x) rev(x)[1])
  if(usemax) l2use=tapply(1:nrow(df),df$Id,function(x) x[which.max(df$Resp[x])])
  df=df[l2use,]
  df$Grp=factor(dfm[df$Id,]$Grp)
  newrange=range(df$Tp)
  if(newrange[1]<newrange[2]) ylab=paste(ifelse(usemax,"Max. r","R"),"esponse in time range: ",newrange[1],"-",newrange[2],sep="")
  if(newrange[1]==newrange[2]) ylab=paste("Response at time=",newrange[1],sep="")

  return(list(Df=df,Resp=resp,Par=ylab))
}



############################################################################################
plotCS<-function(objres,gcols=getCols(objres$Df$Grp),miny=NA,maxy=NA,retplot=T){
  
  idf=objres$Df
  idf$color=gcols[idf$Grp]
  gcols=gcols[gcols%in%idf$color]
  ylim=range(idf$Resp,na.rm=T)
  if(!is.na(miny)) ylim[1]=miny
  if(!is.na(maxy)) ylim[2]=maxy
  ylim=pretty(seq(ylim[1],ylim[2],length.out = 8))
#  if(all(idf$Resp>0,na.rm=T)) ylim[1]=max(0,ylim[1])

  idf=idf[order(idf$Grp,idf$Id),]
  idf$x2=idf$x=as.numeric(idf$Grp)
  for(i in names(which(table(idf$x)>1)))
    idf$x2[idf$x==i]=swarmx(idf$x[idf$x==i],idf$Resp[idf$x==i],xsize=.08,ysize=.08,cex=2)[,1]
  
   bwstats = setNames(as.data.frame(boxplot(Resp ~ Grp, data = idf, plot = F)$stats), nm = NULL)
  bwstats=boxplot(Resp ~ Grp, data = idf, plot = F)$stats
  bwdat=lapply(1:ncol(bwstats),function(i)
    list(x=i-1,low=bwstats[1,i],q1=bwstats[2,i],median=bwstats[3,i],q3=bwstats[4,i],high=bwstats[5,i],
         color=unname(gcols[i]),fillColor= '#F0F0E0',lineWidth= 2,
         medianColor=gcols[i],medianWidth=4,fillColor= "#F0F0F0"))

   bwpts=lapply(1:nrow(idf),function(i) 
    list(name=idf$Id[i],color=unname(idf$color[i]),type='scatter',
         data=list(list(x=idf$x2[i]-1,y=idf$Resp[i],grp=idf$Grp[i],Id=idf$Id[i],radius=8)),
         tooltipText=paste(idf$Id[i],':',idf$Resp[i]),marker=list(symbol='circle')))
  
   if(!retplot) return(list(m=idf,ylim=ylim,title=objres$Par))

  b <- Highcharts$new()
  b$set(series = append(list(list(name =objres$Resp,data = bwdat,tooltip=list(enabled=FALSE))),bwpts))
  b$xAxis(labels=list(rotation = -90),categories = levels(idf$Grp),title = list(text = 'Groups'))
  b$xAxis(categories = paste(levels(idf$Grp)," (",table(idf$Grp),")",sep="")
          ,title = list(text = 'Groups'))
   b$legend(enabled = F)
    b$tooltip( shared=FALSE,formatter = "#! function() { return this.series.options.tooltipText ; } !#")
   b$plotOptions(followPointer=F)
  b$yAxis(title = list(text = objres$Par), min = min(ylim), max = max(ylim), tickInterval = diff(ylim)[1])
  b$chart(type = 'boxplot')
  return(list(plot=b,m=idf,ylim=ylim,title=objres$Par))
}


##########################################################################################
compCS<-function(objres,bfco=0.1,checkvar=TRUE){
  
  idf=objres$Df
  #####
  idf2=idf[idf$Grp %in% names(which((table(idf$Grp)>1))),]
  idf2$Grp=factor(idf2$Grp)
  idf2$out=FALSE
  if(nlevels(idf2$Grp)<2) return(NULL)
  
  mod=Gls(Resp~Grp,data=idf2[!idf2$out,],method="REML")
  if(checkvar){
    modw=update(mod,weights=varIdent(form=~1|Grp))
    if(AIC(modw)<AIC(mod)){
      ll1=logLik(mod)
      ll2=logLik(modw)
      if((1-pchisq(as.numeric(ll2-ll1)*2,attr(ll2,"df")-attr(ll1,"df")))<0.05) mod=modw
    }
  }
  
  resid=residuals(update(mod,method="ML"),'pearson');names(resid)=rownames(idf2)
  out=outlierTest(lm(resid~1),cutoff = bfco)
  if(out$signif){
    idf2[names(out$rstudent),]$out=TRUE
    mod=Gls(Resp~Grp,data=idf2[!idf2$out,],method="REML")
    if(checkvar){
      modw=update(mod,weights=varIdent(form=~1|Grp))
      if(AIC(modw)<AIC(mod)){
        ll1=logLik(mod)
        ll2=logLik(modw)
        if((1-pchisq(as.numeric(ll2-ll1)*2,attr(ll2,"df")-attr(ll1,"df")))<0.05) mod=modw
      }
    }
  }
  mod2=update(mod,.~.,method="ML")
  mod20=update(mod,.~1,method="ML")
  lrt=2*(mod2$logLik-mod20$logLik)
  lrtpv=1-pchisq(lrt,nlevels(idf2$Grp)-1)
  lrt=sprintf("%.2f  (d.f.=%d), p<%s%s",lrt,nlevels(idf2$Grp)-1,.myf(lrtpv),.myfpv(lrtpv))
  wald=rms:::anova.rms(mod2,ss=FALSE)[1,1]
  waldpv=1-pchisq(wald,nlevels(idf2$Grp)-1)
  wald=sprintf("%.2f  (d.f.=%d), p<%s%s",wald,nlevels(idf2$Grp)-1,.myf(waldpv),.myfpv(waldpv))
  npt=NA
  if(nlevels(idf2$Grp)==2){
    kw=suppressWarnings(wilcox.test(Resp~Grp,idf2[!idf2$out,]))
    npt=sprintf("p<%s%s",.myf(kw$p.value),.myfpv(kw$p.value))
  }
  if(nlevels(idf2$Grp)>2){
    kw=suppressWarnings(kruskal.test(Resp~Grp,idf2[!idf2$out,]))
  npt=sprintf("%.2f  (d.f.=%d), p<%s%s",kw$statistic,
              kw$parameter,.myf(kw$p.value),.myfpv(kw$p.value))
  }
  modtab=cbind(lrt,wald,npt)
  dimnames(modtab)=list(c("Treat" ),c("Wald test","Likelihood ratio test",
                                      ifelse(nlevels(idf2$Grp)>2,"Kruskal-Wallis","Wilcoxon RankSum test")))
  ct=contrMat(table(idf2$Grp),type="Tukey")
  lcts=lapply(strsplit(rownames(ct)," - "),function(x)
    contrast(mod2,list(Grp=x[1]),list(Grp=x[2])))
  jtab=data.frame(t(sapply(lcts,function(x)
    c(unlist(x[c("Contrast","Lower","Upper","Pvalue" )])))))
  jtab=cbind(do.call("rbind",strsplit(rownames(ct)," - ")),jtab,stringsAsFactors=F)
  names(jtab)=c("Grp1","Grp2","Contrast","Lower","Upper","Pvalue" )
  
  jtab$Wil=suppressWarnings(sapply(strsplit(rownames(ct)," - "),function(x) 
    wilcox.test(Resp~factor(Grp),idf2[idf2$Grp%in%x & !idf2$out,])$p.value))
  
  print(jtab)
  tab=nlme:::summary.gls(mod2)$tT
  rownames(tab)=gsub("^Grp=","",rownames(tab))
  tab=data.frame(tab)
  tab[,4]=sapply(tab[,4],function(x) sprintf("%.5f",x))
  for(i in 1:3) tab[,i]=sapply(tab[,i],function(x) sprintf("%.3f",x))
  
  weightCoef=NULL
  mstruct=coef(mod2$modelStruct,unconstrained=FALSE)
  if(!is.null(mstruct))
    if(grepl("^varStruct",names(mstruct))){
    weightCoef=mstruct[grep("^varStruct",names(mstruct))]
    names(weightCoef)=gsub("^varStruct\\.","",names(weightCoef))
    weightCoef=weightCoef[levels(idf2$Grp)]
    weightCoef[is.na(weightCoef)]=1
    names(weightCoef)=levels(idf2$Grp)
   }
  
  newdf=expand.grid(Grp=levels(idf2$Grp))
  newdf=cbind(newdf,data.frame(predict(mod2,newdf,se=T)))
  names(newdf)[2]="fit"
  

  return(list(model=mod2,coefTab=tab,pairTab=jtab,AnovaTab=modtab,weightCoef=weightCoef,data=idf2,pred=newdf,Resp=objres$Resp))
  

}

#################################################
################################################################################################
### Diagplots

plotDiagCS<-function(mod,gcols=NULL,typplot="qqplot"){
  
  if(is.null(gcols)) gcols=getCols(mod$data$Grp)
  resp=mod$Resp
  if(grepl("\\.log$",resp)) myf<-function(x) exp(x) else myf<-function(x) x
  
  ###############
  ## format data
  tmpdata=mod$data[!mod$data$out,c("Resp","Grp","Tp","Id")]
  names(tmpdata)=c("y","Grp","x","Id")
  tmpdata$Id=factor(tmpdata$Id,levels=unique(tmpdata$Id))
  tmpdata$color=gcols[tmpdata$Grp]
  ndigy=max(c(3,nchar(pretty(tmpdata$y-floor(tmpdata$y)))))
  ndigye=min(c(3,nchar(pretty(myf(tmpdata$y)-floor(myf(tmpdata$y))))))
  ndigx=max(nchar(abs(pretty(tmpdata$x-floor(tmpdata$x)))))
  tmpdata$x=round(tmpdata$x,ndigx)
  tmpdata$y=round(myf(tmpdata$y),ndigye)
  tmpdata$fit=round(mod$model$fitted,ndigy)
  tmpdata$fite=round(myf(mod$model$fitted),ndigye)
  tmpdata$resid=round(unclass(resid(mod$model,"pearson")),ndigy)
  lso=order(order(tmpdata$resid))
  tmpdata$qt=qnorm(ppoints(length(lso)))[lso]
  
  tmppred=mod$pred[,c("Grp","Grp","fit","se.fit")]
  names(tmppred)=c("x","Grp","fit","se.fit")
  tmppred$x=as.numeric(tmppred$x)
  tmppred$color=gcols[tmppred$Grp]
  tmppred$y=round(myf(tmppred$fit),ndigye)
  tmppred$ymin=round(myf(tmppred$fit-1.96*tmppred$se.fit),ndigye)
  tmppred$ymax=round(myf(tmppred$fit+1.96*tmppred$se.fit),ndigye)
  
  
  limtp=pretty(tmpdata$x)
  limyfit=pretty(c(tmppred$ymin,tmppred$ymax,tmpdata$fite))
  limyfit0=pretty(c(tmpdata$fit))
  limxqq=range(c(tmpdata$resid,tmpdata$qt))
  limxqq=floor(limxqq[1]):ceiling(limxqq[2])
  
  if(typplot=="None")    return(list(data=tmpdata,pred=tmppred,
                                     limtp=limtp,limyfit=limyfit,limyfit0=limyfit0,limxqq=limxqq))
  
  ## format Fit
#   if(typplot=="Fit"){
#     
#     a <- rCharts::Highcharts$new()
#     for(i in unique(tmpdata$Id))
#       a$series(data = lapply(which(tmpdata$Id==i),function(j) as.list(tmpdata[j,c("x","y","Grp","Id")])), name=i,type = "line",
#                color=unname(tmpdata$color[which(tmpdata$Id==i)][1]),
#                lineWidth = 1.5,
#                showInLegend = FALSE, marker= list(enabled = FALSE))
#     for(i in unique(tmppred$Grp)){
#       a$series(data = lapply(which(tmppred$Grp==i),function(j) as.list(tmppred[j,c("x","y","Grp")])), name=i,type = "line",
#                fillOpacity = 0.3,lineWidth = 4,
#                color=unname(tmppred$color[which(tmppred$Grp==i)][1]),
#                zIndex = 1)
#       a$series(data = lapply(which(tmppred$Grp==i),function(j) 
#         unname(as.list(tmppred[j,c("x","ymin","ymax")]))),type = "arearange",# showInLegend = FALSE,
#         fillOpacity = 0.3,lineWidth = 0,
#         color=unname(tmppred$color[which(tmppred$Grp==i)][1]),
#         zIndex = 0)
#     }
#     a$xAxis(title = list(text = "Time"), min = min(limtp), max = max(limtp), tickInterval = diff(limtp)[1])
#     a$yAxis(title = list(text = paste("Response:",resp)), min =  min(limyfit), max = max(limyfit), tickInterval = diff(limyfit)[1])
#     a$legend(verticalAlign = "right", align = "right", layout = "vertical", title = list(text = "Treatment group"))
#     #  a
#     return(a)
#   }
#   
  ## format qqplot
  if(typplot=="QQ-plot"){
    tmpqqp=tmpdata[,c("resid","qt","x"  ,"Grp","Id","color")]
    names(tmpqqp)[1:3]=c("x","y","Tp")
    ab=c(range(limxqq),range(limxqq))
    a <- rCharts::Highcharts$new()
    a$series(data = list(list(x=ab[1],y=ab[3]),list(x=ab[2],y=ab[4])), name='reg',type = "line",color='grey',
             showInLegend = FALSE, marker= list(enabled = FALSE))
    a$series(data = lapply(1:nrow(tmpqqp),function(i) as.list(tmpqqp[i,])), name='qqnorm',type = "scatter")
    a$tooltip( formatter = "#! function() { return this.point.Grp + ': ' + this.point.Id + ' (' +this.point.Tp + ')' ; } !#")
    a$legend(enabled = F)
    a$xAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    a$yAxis(title = list(text = "Quantile"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    #    a
    return(a)
  }
  
  ## format resid/mid
  if(typplot=="Resid/Grp"){
    tmpbxp=tmpdata[,c("Id","resid","Grp","color")]
    names(tmpbxp)[2]="x"
    bwstats = setNames(as.data.frame(boxplot(x ~ Grp, data = tmpbxp, plot = F)$stats), nm = NULL)
    b <- Highcharts$new()
    b$set(series = list(list(name = 'Residuals',data = bwstats)))
    b$xAxis(labels=list(rotation = -90),categories = levels(tmpbxp$Grp),title = list(text = 'Group'))
    b$legend(enabled = F)
    b$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    b$chart(type = 'boxplot')
    #    b
    return(b)
  }
  
  # format resid,fit
  tmpresf=tmpdata[,c("resid","fit" ,"x" ,"Grp","Id","color")]
  names(tmpresf)[1:3]=c("y","x","Tp")
  c <- rCharts::Highcharts$new()
  #  c$series(data = list(list(x=flim[1],y=0),list(x=flim[2],y=0)), name='reg',type = "line",color='grey')
  c$series(data = lapply(1:nrow(tmpresf),function(i) as.list(tmpresf[i,])), name='resid',type = "scatter")
  c$tooltip( formatter = "#! function() { return this.point.Grp + ': ' + this.point.Id + ' (' +this.point.Tp + ')' ; } !#")
  c$legend(enabled = F)
  c$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
  c$xAxis(title = list(text = "Fitted"), min = min(limyfit0), max = max(limyfit0), tickInterval = diff(limyfit0)[1])
  return(c)
  
}

