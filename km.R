

compKM<-function(objres,ref,firth=FALSE){
  
  cdf=objres$Df
  cdf$Grp=relevel(cdf$Grp,ref)
  
  modtab=hrtab=NULL
  if(nlevels(cdf$Grp)==1 | all(!cdf$Event) | (all(cdf$Event) & length(unique(cdf$Time))==1))
    return(list(typ=objres,model=NULL,data=cdf,modTab=NULL,hrTab=NULL))
  
  ####################
  if(!firth){
    mod=cph(Surv(Time,Event)~Grp,cdf,eps=1e-6,surv=T,x=T,y=T)
    
    mtest=cbind(c(2*diff(mod$loglik),anova(mod)[1,1],mod$score),nlevels(cdf$Grp)-1)
    mtest=cbind(mtest,1-pchisq(mtest[,1],mtest[,2]))
    modtab=matrix(apply(mtest,1,function(x) 
      sprintf("%.2f  (d.f.=%d), p<%s%s",x[1],x[2],.myf(x[3]),.myfpv(x[3]))),ncol=3)
    dimnames(modtab)=list("Treat",c("Likelihood ratio test","Wald test","LogRank test"))
    
    cf=exp(cbind(mod$coefficients,confint(mod)))
    cf[cf>10000]=Inf
    cf[cf<1/10000]=-Inf
    pv=(1-pchisq(abs(mod$coefficients/sqrt(diag(mod$var)))^2,1))
    
    hrs=apply(cf,1,function(x) sprintf("%.3f [%.3f;%.3f]",x[1],x[2],x[3]))
    hrpva=sprintf('%s',.myf(p.adjust(pv,"holm")))
    hrpv=sprintf('%s',.myf(pv))
  }
  ####################
  if(firth){
    mod0=cph(Surv(Time,Event)~Grp,cdf,eps=1e-6)
    
    mod<-try(coxphf(Surv(Time,Event)~Grp,cdf),TRUE)
    wald.z <- t(coef(mod)) %*% solve(mod$var) %*% coef(mod)
    
    mtest=cbind(c(2*diff(mod$loglik),wald.z,mod0$score),nlevels(cdf$Grp)-1)
    mtest=cbind(mtest,1-pchisq(mtest[,1],mtest[,2]))
    modtab=matrix(apply(mtest,1,function(x) 
      sprintf("%.2f  (d.f.=%d), p<%s%s",x[1],x[2],.myf(x[3]),.myfpv(x[3]))),ncol=3)
    dimnames(modtab)=list("Treat",c("Likelihood ratio test","Wald test","LogRank test"))

    hrs=apply(cbind(mod$coefficients,mod$ci.lower,mod$ci.upper),1,function(x) sprintf("%.3f [%.3f;%.3f]",exp(x[1]),exp(x[1]-1.96*x[2]),exp(x[1]+1.96*x[2])))
    hrpva=sprintf('%s',.myf(p.adjust(mod$prob,"holm")))
    hrpv=sprintf('%s',.myf(mod$prob))
  }
  ####################
  lrs=sapply(levels(cdf$Grp)[-1],function(x) 
    summary(coxph(Surv(Time,Event)~factor(Grp),cdf[cdf$Grp%in%c(ref,x),]))$sctest["pvalue"])
  lrsa=sprintf('%s',.myf(p.adjust(lrs,"holm")))
  lrs=sprintf('%s',.myf(lrs))
  hrtab=cbind(Covariate=levels(cdf$Grp)[-1],'Hazard ratio'=hrs,"Pvalue"=hrpv,"PvalueAdj"=hrpva,"LogRPval"=lrs,"LogRPvalAdj"=lrsa)
  rownames(hrtab)=NULL
  ####################
  
  list(model=mod,Df=cdf,modTab=modtab,hrTab=hrtab,Typ=objres$Typ,Resp=objres$Resp,Par=objres$Par)
}

getOSTab<-function(cdat,resp=gsub("\\.log$","",cdat$Resp[1]),lgrps=levels(cdat$dataM$Grp),
                   lastT=max(cdat$data$Tp,na.rm=T),lastM=Inf){
  
  df=cdat$data
  dfm=cdat$dataM
  lmids=dfm$Id[dfm$Use & dfm$Grp%in%lgrps]
  lastMid=tapply(df$Tp,df$Id,max)
  
  
  andf=list()
  for(ipid in lmids){
    if(!dfm[ipid,]$Surv){
      l=which(!is.na(df[,resp]) & df$Tp<=lastT & df[,resp]<=lastM & df$Id==ipid)
      ndf=data.frame(Id=ipid,Time=max(df$Tp[l]),Resp=rev(df[l,resp])[1],stringsAsFactors=F)
      ndf$Event=(ndf$Time<=lastT & ndf$Resp>0) #else  ndf$Event=(ndf$Time<lastT & ndf$Resp>0)
    }
    if(dfm[ipid,]$Surv){
      l=which(!is.na(df[,resp]) & df[,resp]<=lastM & df$Id==ipid)
      ndf=data.frame(Id=ipid,Time=max(df$Tp[l]),Resp=rev(df[l,resp])[1],Event=FALSE,stringsAsFactors=F)
      if(ndf$Time<lastMid[ipid]) ndf$Event=TRUE
    }
    andf[[ipid]]=ndf
  }
  ndf=do.call("rbind",andf)
  ndf$Grp=factor(dfm[ndf$Id,]$Grp)
  params=paste("censoring on response=",lastM," and time=",lastT,sep="")
  return(list(Df=ndf,Typ="OS",Resp=resp,Par=params))
}


getTFSTab<-function(cdat,resp=gsub("\\.log$","",cdat$Resp[1]),lgrps=levels(cdat$dataM$Grp),firstM=0){
  
  df=cdat$data
  dfm=cdat$dataM
  lmids=dfm$Id[dfm$Use & dfm$Grp%in%lgrps]
  

  andf=list()
  for(ipid in lmids){
      l=which(!is.na(df[,resp]) & df$Id==ipid)
      if(all(df[l,resp]<=firstM)){
        andf[[ipid]]=data.frame(Id=ipid,Time=max(df$Tp[l]),Resp=df[max(l),resp],Event=FALSE,stringsAsFactors=F)
        next
      }
      l1=which(df[l,resp]<=firstM)
      if(length(l1)==0) l1=0
      if(length(l1)>0 & l1[1]!=1) l1=0
      if(length(l1)>1){
        ldi1=diff(l1)
        l1=ifelse(all(ldi1==1),max(l1),min(which(ldi1>1)))
      }
      andf[[ipid]]=data.frame(Id=ipid,Time=df$Tp[l[l1+1]],Resp=df[l[l1+1],resp],Event=TRUE,stringsAsFactors=F)
  }
  ndf=do.call("rbind",andf)
  ndf$Grp=factor(dfm[ndf$Id,]$Grp)
  params=paste("detection limit ",firstM,sep="")
  return(list(Df=ndf,Typ="TFS",Resp=resp,Par=params))
}

plotKM<-function(obj,gcols=getCols(ndf$Grp),shift=0.1,lwd=1,maxtp=NULL,title= "Perc. surviving",retplot=T){
  
  ndf=obj$Df
  if(is.null(maxtp)) maxtp=max(ndf$Time*1.1)
  limtp=pretty(c(0,maxtp))
  
  shift=as.vector(scale((1:nlevels(ndf$Grp))*shift[1],scale=F))
  names(shift)=levels(ndf$Grp)
  akms=lapply(levels(ndf$Grp),function(i){ 
    tt=data.frame(unclass(survfit(Surv(Time,Event)~1,ndf[ndf$Grp==i,]))[c("time","n.risk","n.event")])
    names(tt)[1]="x"
    n0=tt$n.risk[1]
    tt=rbind(c(0,n0,0),tt)
    tt$y=100*tt$n.risk/n0
    if(nrow(tt)>2){
      add=tt[2:(nrow(tt)-1),]
      add$y=tt$y[2:(nrow(tt)-1)+1]
      tt=rbind(tt,add)
    }
    tt=tt[order(tt$x,-tt$y),]
    if(tt[nrow(tt),2]==tt[nrow(tt),3]){
      add=tt[nrow(tt),]
      add[2]=add[3]=add[4]=0
      tt=rbind(tt,add)
    }
    tt$Grp=i;tt$n0=n0;
    tt$x0=tt$x
    tt$x=tt$x+shift[i]
    tt$y=round(tt$y,2)
    tt$color=gcols[tt$Grp]
    tt
  })
  names(akms)=levels(ndf$Grp)
  
  if(!retplot) return(list(m=akms,limtp=limtp,title=title))
  
  a <- rCharts::Highcharts$new()
  for(i in names(akms))
    a$series(data = lapply(1:nrow(akms[[i]]),function(j) as.list(akms[[i]][j,])), name=i,color=akms[[i]]$color[1],type = "line",lineWidth = lwd)
  a$tooltip( formatter = "#! function() { return this.point.Grp + ' at ' + this.point.x0 +
             ' (' + this.point.y + '/' + this.point.n0 + ')' ; } !#")
  a$xAxis(title = list(text = "Time"), min = min(limtp), max = max(limtp), tickInterval = diff(limtp)[1])
  a$yAxis(title = list(text =title), min = 0, max = 100, tickInterval = 20)
  return(list(plot=a,m=akms,limtp=limtp,title=title))
}

############################################################################################

