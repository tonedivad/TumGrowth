#####################################################################
### functions for survival analysis
## getOSTab: form table when OS selected
## getTFSTab: form table when TFS selected
## compKM: compute Cox regression models
## plotKM: plot KM surv
#####################################################################

##################

getOSTab<-function(cdat,resp=cdat$Resp[1],lgrps=levels(cdat$dataM$Grp),gcols=getCols(cdat$dataM$Grp),
                   lastT=max(cdat$data$Tp,na.rm=T),lastM=Inf){
  
  print("Getos")
  df=cdat$data
  dfm=cdat$dataM
  lmids=dfm$Id[dfm$Use & dfm$Grp%in%lgrps]
  lastMid=tapply(df$Tp,df$Id,max,na.rm=T)
#  print(lastMid)
  
  andf=list()
  for(ipid in lmids){
    l=which(!is.na(df[,resp]) & df$Tp<=lastT & df[,resp]<=lastM & df$Id==ipid)
    itp=max(df$Tp[l])
    iresp=df[l[which.max(df$Tp[l])],resp]
    l2=which(!is.na(df[,resp]) & df$Tp>itp  & df$Tp<=lastT &  df$Id==ipid) ## any greater values after lastT
    ndf=data.frame(Id=ipid,Time=itp,Resp=iresp,Event=!(length(l2)==0),stringsAsFactors=F)
    andf[[ipid]]=ndf
  }
  ndf=do.call("rbind",andf)
  rownames(ndf)=ndf$Id
  ndf$Grp=factor(dfm[ndf$Id,]$Grp)
  ndf$color=gcols[as.character(ndf$Grp)]
  ndf$lastMid=lastMid[ndf$Id]
  ndf$SurvLast=dfm[ndf$Id,]$Surv
  
  ######## here the ndf$Resp is obviously less than lastM
  l=which(ndf$lastMid==ndf$Time & !ndf$Surv)
  if(length(l)>0) ndf$Event[l]=TRUE
  l=which(ndf$lastMid==ndf$Time & ndf$Surv)
  if(length(l)>0) ndf$Event[l]=FALSE
  ########
  params=paste("censoring on response=",lastM," and time=",lastT,sep="")
  return(list(Df=ndf,Typ="OS",Resp=resp,Par=params))
}

###########################

getTFSTab<-function(cdat,resp=cdat$Resp[1],lgrps=levels(cdat$dataM$Grp),gcols=getCols(cdat$dataM$Grp),firstM=0){
  
  df=cdat$data
  dfm=cdat$dataM
  lmids=dfm$Id[dfm$Use & dfm$Grp%in%lgrps]
  
  lastMid=tapply(df$Tp,df$Id,max,na.rm=T)
  andf=list()
  for(ipid in lmids){
      l=which(!is.na(df[,resp]) & df$Id==ipid)
      if(all(df[l,resp]<=firstM)){
        andf[[ipid]]=data.frame(Id=ipid,Time=max(df$Tp[l]),Resp=df[max(l),resp],Event=FALSE,stringsAsFactors=F)
        next
      }
      if(all(df[l,resp]>firstM)){
        andf[[ipid]]=data.frame(Id=ipid,Time=df$Tp[l][1],Resp=df[l,resp][1],Event=TRUE,stringsAsFactors=F)
        next
      }
      l1=which(df[l,resp]<=firstM)
      if(length(l1)>1){
        ldi1=diff(l1)
        l1=ifelse(all(ldi1==1),max(l1),min(which(ldi1>1)))
      }
      andf[[ipid]]=data.frame(Id=ipid,Time=df$Tp[l[l1+1]],Resp=df[l[l1+1],resp],Event=TRUE,stringsAsFactors=F)
  }
  ndf=do.call("rbind",andf)
  ndf$Grp=factor(dfm[ndf$Id,]$Grp)
  ndf$color=gcols[as.character(ndf$Grp)]
  ndf$lastMid=lastMid[ndf$Id]
  ndf$SurvLast=dfm[ndf$Id,]$Surv
  params=paste("detection limit ",firstM,sep="")
  return(list(Df=ndf,Typ="TFS",Resp=resp,Par=params))
}

###############################
sumIdKM<-function(objres){

  ndf=objres$Df
  v=paste(ndf$Id," (",ndf$Time,c("-","+")[ndf$Event+1],")",sep="")
  names(v)=NULL
  lso=order(ndf$Time,ndf$Event)
  v=tapply(v[lso],ndf$Grp[lso],c)
  nv=tapply(v[lso],ndf$Grp[lso],length)
  nev=tapply(ndf$Event[lso],ndf$Grp[lso],sum)
  
  x=survival:::survmean(survfit(Surv(Time,Event)~Grp,ndf),1,"individual")$matrix
  add=data.frame(x[,c(1,4,7)])
  names(add)=c("N","Event","MedSurv")
  add$N=sprintf("%d",add$N)
  add$Event=sprintf("%d",add$Event)
  add$MedSurv=sprintf("%.2f",add$MedSurv)
  add$MedFUP=sprintf("%.2f",survival:::survmean(survfit(Surv(Time,!Event)~Grp,ndf),1,"individual")$matrix[,"median"])
  sumIds=data.frame(cbind(Group=names(v),add,"Censoring"=unlist(sapply(v,paste,collapse=" "))))
  sumIds
}

###############################
compKM<-function(objres,ref,firth=FALSE){
  
  cdf=objres$Df
  cdf$Grp=relevel(cdf$Grp,ref)
  lnull=list(model=NULL,Df=cdf,modTab=NULL,hrTab=NULL,Typ=objres$Typ,Resp=objres$Resp,Par=objres$Par)
  
  modtab=hrtab=NULL
  if(nlevels(cdf$Grp)==1 | ((all(!cdf$Event) | all(cdf$Event)) & length(unique(cdf$Time))==1)) return(lnull)
    
  
  ####################
  if(!firth){
    mod<-try(cph(Surv(Time,Event)~Grp,cdf,eps=1e-6,surv=T,x=T,y=T),silent=TRUE)
    if("try-error"%in%class(mod)) return(lnull)
    mtest=cbind(c(2*diff(mod$loglik),anova(mod)[1,1],mod$score),nlevels(cdf$Grp)-1)
    mtest=cbind(mtest,1-pchisq(mtest[,1],mtest[,2]))
    modtab=matrix(apply(mtest,1,function(x) 
      sprintf("%.2f  (d.f.=%d), p<%s",x[1],x[2],.myf(x[3]))),ncol=3)
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
    mod<-try(cph(Surv(Time,Event)~Grp,cdf,eps=1e-6),silent=TRUE)
    m0score=ifelse("try-error"%in%class(mod),0,mod$score)
    mod<-try(coxphf(Surv(Time,Event)~Grp,cdf),TRUE)
    if("try-error"%in%class(mod)) return(lnull)
    wald.z <- t(coef(mod)) %*% solve(mod$var) %*% coef(mod)
    
    mtest=cbind(c(2*diff(mod$loglik),wald.z,m0score),nlevels(cdf$Grp)-1)
    mtest=cbind(mtest,1-pchisq(mtest[,1],mtest[,2]))
    modtab=matrix(apply(mtest,1,function(x) 
      sprintf("%.2f  (d.f.=%d), p<%s",x[1],x[2],.myf(x[3]))),ncol=3)
    dimnames(modtab)=list("Treat",c("Likelihood ratio test","Wald test","LogRank test"))
    
    hrs=apply(cbind(mod$coefficients,mod$ci.lower,mod$ci.upper),1,function(x) 
      sprintf("%.3f [%.3f;%.3f]",exp(x[1]),exp(x[1]-1.96*x[2]),exp(x[1]+1.96*x[2])))
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

##########################################
plotKM<-function(obj,shift=0.1,lwd=1,maxtp=max(obj$Df$lastMid),title= "Perc. surviving",retplot=T){
  
  if(is.null(obj)) return(list(list(plot=NULL,m=NULL,limtp=NULL,title=title)))
  ndf=obj$Df
  gcols=tapply(ndf$color,ndf$Grp,unique)
  limtp=pretty(c(0,maxtp*1.1))
  
  shift=as.vector(scale((1:nlevels(ndf$Grp))*shift[1],scale=F))
  names(shift)=levels(ndf$Grp)
  akms=lapply(levels(ndf$Grp),function(i){ 
    tt=data.frame(unclass(survfit(Surv(Time,Event)~1,ndf[ndf$Grp==i,]))[c("time","n.risk","n.event","n.censor")])
    names(tt)=c("x", "nrisk" , "nevent" , "ncensor")
    n0=tt$nrisk[1]
    print(i)
    y0=tt$nrisk/n0
    if(length(y0)==1) tt$y=1
    if(length(y0)>1){
    y1=y0[-1]
    tt=rbind(tt,tt[-nrow(tt),])
    tt$y=c(y0,y1)
    tt=tt[order(tt$x,-tt$y),]
    }
    if(tt$x[1]!=0){
      tt=rbind(tt[1,],tt)
      tt$x[1]=0
      tt$y[1]=1
    }
    add=tt[nrow(tt),]
    add$y=(add$nrisk-add$nevent)/n0
    tt=rbind(tt,add)
    
    tt$Grp=i;tt$n0=n0;
    tt$x0=tt$x
    tt$x=tt$x+shift[i]
    tt$y=round(tt$y*100,2)
    tt$color=gcols[as.character(tt$Grp)]
    tt
  })
  names(akms)=levels(ndf$Grp)
  
  if(!retplot) return(list(m=akms,limtp=limtp,title=title))
  
  a <- rCharts::Highcharts$new()
  for(i in names(akms))
    a$series(data = lapply(1:nrow(akms[[i]]),function(j) as.list(akms[[i]][j,c("x","y","x0","n0","nrisk","nevent","ncensor","Grp")])), name=i,
             color=unname(akms[[i]]$color[1]),type = "line",lineWidth = lwd)
  a$tooltip( formatter = "#! function() { return this.point.Grp+ '=' +  this.point.y + ' at ' + this.point.x0 +
            ' (' + this.point.nrisk + ':' + this.point.nevent + '/' + this.point.ncensor + ')' ; } !#")
  a$xAxis(title = list(text = "Time"), min = min(limtp), max = max(limtp), tickInterval = diff(limtp)[1])
  a$yAxis(title = list(text =title), min = 0, max = 100, tickInterval = 20)
  return(list(plot=a,m=akms,limtp=limtp,title=title))
}

############################################################################################

