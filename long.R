
compModLG<-function(lgdf,bfco=0.1,checkvar=TRUE){
  
  lgdf$Id=factor(lgdf$Id)
  lgdf=lgdf[order(lgdf$Grp,lgdf$Id,lgdf$Tp),]
  lgdf$out=FALSE
  ######
 # print("OKKK")
  ga=Gls(Resp~Tp*Grp, data=lgdf,correlation=corCompSymm(form=~1|Id),method="REML")
  ga2=Gls(Resp~Tp*Grp, data=lgdf,correlation=corCAR1(form=~Tp|Id),method="REML")
  if(AIC(ga2)<AIC(ga)) ga=ga2
  if(checkvar){
    gaw=update(ga,weights=varIdent(form=~1|Grp))
    if(AIC(gaw)<AIC(ga)){
      ll1=logLik(ga)
      ll2=logLik(gaw)
      if((1-pchisq(as.numeric(ll2-ll1)*2,attr(ll2,"df")-attr(ll1,"df")))<0.05) ga=gaw
    }
  }
  # print("OKKK1")
  
  iresid=residuals(ga,'pearson')
  names(iresid)=rownames(lgdf)
  out=outlierTest(lm(iresid~1),cutoff = bfco)
  if(any(out$signif)){
    #    print(out)
    lgdf$out[which(rownames(lgdf)%in%names(out$bonf.p))]=T
    ga=Gls(Resp~Tp*Grp, data=lgdf[!lgdf$out,],correlation=corCompSymm(form=~1|Id),method="REML")
    ga2=Gls(Resp~Tp*Grp, data=lgdf[!lgdf$out,],correlation=corCAR1(form=~Tp|Id),method="REML")
    if(AIC(ga2)<AIC(ga)) ga=ga2
    if(checkvar){
      gaw=update(ga,weights=varIdent(form=~1|Grp))
      if(AIC(gaw)<AIC(ga)){
        ll1=logLik(ga)
        ll2=logLik(gaw)
        if((1-pchisq(as.numeric(ll2-ll1)*2,attr(ll2,"df")-attr(ll1,"df")))<0.05) ga=gaw
      }
    }
  }
  
  ga2=update(ga,.~.,method="ML")
  m2=as.matrix(rms:::anova.rms(ga2,ss=FALSE))
  ga3=update(ga2,.~Tp+Grp,method="ML")
  m3=as.matrix(rms:::anova.rms(ga3,ss=FALSE))
  mwald=rbind(m2[5,],m3[1:2,])
  
  lrt=2*abs(c(ga2$logLik,update(ga3,.~Grp,method="ML")$logLik,update(ga3,.~Tp,method="ML")$logLik)-ga3$logLik)
  mlrt=cbind(lrt,c(1,1,nlevels(lgdf$Grp)-1))
  mlrt=cbind(mlrt,1-pchisq(mlrt[,1],mlrt[,2]))
  antab=cbind(apply(mwald,1,function(x) sprintf("%.2f  (d.f.=%d), p<%s%s",x[1],x[2],.myf(x[3]),.myfpv(x[3]))),
              apply(mlrt,1,function(x) sprintf("%.2f  (d.f.=%d), p<%s%s",x[1],x[2],.myf(x[3]),.myfpv(x[3]))))
  dimnames(antab)=list(c("Time x Treat","Time","Treat" ),c("Wald test","Likelihood ratio test"))
  
  ################################################################################################
  # Compute pairwise comparisons
  
  ct=contrMat(table(lgdf$Grp),type="Tukey")[,-1,drop=F]
  lcts=lapply(strsplit(rownames(ct)," - "),function(x)
  contrast(ga2,list(Grp=x[1],Tp=c(0)),list(Grp=x[2],Tp=c(0)),list(Grp=x[1],Tp=c(1)),list(Grp=x[2],Tp=c(1)),type='joint'))
  jtab=data.frame(t(sapply(lcts,function(x)
    c(ChiSq=x$jointstat,Pvalue=1-pchisq(x$jointstat,1),unlist(x[c("Contrast","Lower","Upper","Pvalue" )])))))
  jtab=cbind(do.call("rbind",strsplit(rownames(ct)," - ")),jtab,stringsAsFactors=F)
  names(jtab)=c("Largest","Smallest","ChiSq","Pvalue","Contrast","Lower","Upper","PvalueCt" )
  
  for(i in which(jtab$Contrast<0)){
  jtab[i,1:2]=jtab[i,2:1]
  jtab[i,6:7]=-jtab[i,7:6]
  jtab[i,5]=-jtab[i,5]
  }
  
#   
#   ct=contrMat(table(lgdf$Grp),type="Tukey")[,-1,drop=F]
#   jt1=cbind(0,0,ct,matrix(0,nrow=nrow(ct),ncol=ncol(ct)))
#   jt2=cbind(0,0,matrix(0,nrow=nrow(ct),ncol=ncol(ct)),ct)
#   jtab=data.frame(do.call("rbind",lapply(1:nrow(jt1),function(i) esticon(ga,rbind(jt1[i,],jt2[i,]),join=T))))
#   
#   ltps=sort(unique(lgdf$Tp))
#   amct=lapply(1:nrow(jt1),function(i) 
#     sapply(ltps,function(itp) c(0,0,contrMat(table(lgdf$Grp),type="Tukey")[i,-1],
#                                 contrMat(table(lgdf$Grp),type="Tukey")[i,-1]*itp)))
#   jtabs=lapply(amct,function(x) esticon(ga,t(x)))
#   lsign=sapply(jtabs,function(x) sign(mean(x[,2])))
#   jtab=cbind(do.call("rbind",strsplit(rownames(jt1)," - ")),-1,jtab)
#   v1=jtab[,1]=as.character(jtab[,1]);v2=jtab[,2]=as.character(jtab[,2])
#   if(any(jtab[,3]==-1)){
#     v1[which(jtab[,3]==-1)]=jtab[which(jtab[,3]==-1),2]
#     v2[which(jtab[,3]==-1)]=jtab[which(jtab[,3]==-1),1]
#   }
#   jtab[,1]=v1;jtab[,2]=v2;jtab=jtab[,-c(3,5)]
#   names(jtab)=c("Largest","Smallest","ChiSq","Pvalue")
#   
  ################################################################################################
  # Format output
  tab=nlme:::summary.gls(ga2)$tT
  rownames(tab)=gsub("^Tp * Grp=","Tp:",gsub("^Grp=","",rownames(tab)))
  tab=data.frame(tab)
  tab[,4]=sapply(tab[,4],function(x) sprintf("%.5f",x))
  for(i in 1:3) tab[,i]=sapply(tab[,i],function(x) sprintf("%.3f",x))
  
  corrCoef=weightCoef=NULL
  mstruct=coef(ga2$modelStruct,unconstrained=FALSE)
  if(!is.null(mstruct)){
    if(any(grepl("^corStruct",names(mstruct)))){
    corrCoef=mstruct[grep("^corStruct",names(mstruct))]
    corrCoef=sprintf("%s=%.3f",tolower(gsub("^corStruct\\.","",names(corrCoef))),corrCoef)
  }
  
    if(any(grepl("^varStruct",names(mstruct)))){
    weightCoef=mstruct[grep("^varStruct",names(mstruct))]
    names(weightCoef)=gsub("^varStruct\\.","",names(weightCoef))
    weightCoef=weightCoef[levels(lgdf$Grp)]
    weightCoef[is.na(weightCoef)]=1
    names(weightCoef)=levels(lgdf$Grp)
    #weightCoef=round(weightCoef,3)
    }
  }
  
  newdf=expand.grid(Tp=min(lgdf$Tp):max(lgdf$Tp),Grp=levels(lgdf$Grp))
  newdf=cbind(newdf,data.frame(predict(ga2,newdf,se=T)))
  names(newdf)[3]="fit"
  list(model=ga2,coefTab=tab,pairTab=jtab,AnovaTab=antab,weightCoef=weightCoef,corrCoef=corrCoef,data=lgdf,pred=newdf)
}


################################################################################################
### Diagplots

plotDiagLG<-function(mod,gcols=NULL,typplot="QQ-plot"){
  
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
  
  tmppred=mod$pred[,c("Tp","Grp","fit","se.fit")]
  names(tmppred)=c("x","Grp","fit","se.fit")
  tmppred$color=gcols[tmppred$Grp]
  tmppred$x=round(tmppred$x,ndigx)
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
  if(typplot=="Fit"){
    
    a <- rCharts::Highcharts$new()
    for(i in unique(tmpdata$Id))
      a$series(data = lapply(which(tmpdata$Id==i),function(j) as.list(tmpdata[j,c("x","y","Grp","Id")])), name=i,type = "line",
               color=unname(tmpdata$color[which(tmpdata$Id==i)][1]),
               lineWidth = 1.5,
               showInLegend = FALSE, marker= list(enabled = FALSE))
    for(i in unique(tmppred$Grp)){
      a$series(data = lapply(which(tmppred$Grp==i),function(j) as.list(tmppred[j,c("x","y","Grp")])), name=i,type = "line",
               fillOpacity = 0.3,lineWidth = 4,
               color=unname(tmppred$color[which(tmppred$Grp==i)][1]),
               zIndex = 1)
      a$series(data = lapply(which(tmppred$Grp==i),function(j) 
        unname(as.list(tmppred[j,c("x","ymin","ymax")]))),type = "arearange",# showInLegend = FALSE,
        fillOpacity = 0.3,lineWidth = 0,
        color=unname(tmppred$color[which(tmppred$Grp==i)][1]),
        zIndex = 0)
    }
    a$xAxis(title = list(text = "Time"), min = min(limtp), max = max(limtp), tickInterval = diff(limtp)[1])
    a$yAxis(title = list(text = paste("Response:",resp)), min =  min(limyfit), max = max(limyfit), tickInterval = diff(limyfit)[1])
    a$legend(verticalAlign = "right", align = "right", layout = "vertical", title = list(text = "Treatment group"))
    #  a
    return(a)
  }
  
  ## format qqplot
  if(typplot=="QQ-plot"){
    tmpqqp=tmpdata[,c("resid","qt","x"  ,"Grp","Id","color")]
    names(tmpqqp)[1:3]=c("x","y","Tp")
    ab=c(range(limxqq),range(limxqq))
    if(nrow(tmpqqp)>800) tmpqqp=tmpqqp[rev(order(-abs(tmpqqp[,1]))[1:800]),]
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
  if(typplot=="Resid/Mice"){
    tmpbxp=tmpdata[,c("Id","resid","Grp","color")]
    names(tmpbxp)[2]="x"
    bwstats = setNames(as.data.frame(boxplot(x ~ Id, data = tmpbxp, plot = F)$stats), nm = NULL)
    b <- Highcharts$new()
    b$set(series = list(list(name = 'Residuals',data = bwstats)))
    b$xAxis(labels=list(rotation = -90),categories = levels(tmpbxp$Id),title = list(text = 'Mice ids'))
    b$legend(enabled = F)
    b$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    b$chart(type = 'boxplot')
    #    b
    return(b)
  }
  
  # format resid,fit
  tmpresf=tmpdata[,c("resid","fit" ,"x" ,"Grp","Id","color")]
  names(tmpresf)[1:3]=c("y","x","Tp")
  if(nrow(tmpresf)>800) tmpresf=tmpresf[rev(order(-abs(tmpresf[,1]))[1:800]),]

  c <- rCharts::Highcharts$new()
  #  c$series(data = list(list(x=flim[1],y=0),list(x=flim[2],y=0)), name='reg',type = "line",color='grey')
  c$series(data = lapply(1:nrow(tmpresf),function(i) as.list(tmpresf[i,])), name='resid',type = "scatter")
  c$tooltip( formatter = "#! function() { return this.point.Grp + ': ' + this.point.Id + ' (' +this.point.Tp + ')' ; } !#")
  c$legend(enabled = F)
  c$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
  c$xAxis(title = list(text = "Fitted"), min = min(limyfit0), max = max(limyfit0), tickInterval = diff(limyfit0)[1])
  return(c)
  
}

############################################################################################
