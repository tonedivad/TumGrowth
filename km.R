
.infctKM<-function(object){
  LL <- 2 * diff(object$loglik)
  LLpv=1 - pchisq(LL, object$df)
  wald.z <- t(coef(object)) %*% solve(object$var) %*% coef(object)
  wald.zpv=1 - pchisq(wald.z, object$df)
  rbind(sprintf("Likelihood ratio: %.2f, p<%s",LL,myf(LLpv)),
        sprintf("Wald: %.2f, p<%s",wald.z,myf(wald.zpv)))
}


plotKM<-function(df,resp,lgrps=NULL,lastT=max(df$tp,na.rm=T),lastM=Inf,nextday=FALSE,shift0=0){
  
  if(length(lgrps)==0) lgrps=levels(df$grp)
  df=df[df$grp%in%lgrps & df$Use,]
  df$grp=factor(df$grp)
  df$Resp=df[,resp]
  l=which(!is.na(df$Resp) & df$tp<=lastT & df$Resp<=lastM)
  ndf=data.frame(Time=tapply(l,df$Id[l],function(x) max(df$tp[x])),
                 Resp=tapply(l,df$Id[l],function(x) rev(df$Resp[x])[1]),
                 color=tapply(l,df$Id[l],function(x) df$colorG[x][1]))
  ndf$grp=factor(df$grp[match(rownames(ndf),df$Id)])
  ndf$Id=df$Id[match(rownames(ndf),df$Id)]
  #ndf$Event0=sapply(ndf$Id,function(x) length(which(df$Id==x & df$tp>ndf[x,]$Time & !is.na(df[x,]$Resp)))>0)
  ndf$Event=(ndf$Time!=lastT)
  
  if(nextday){
    ltps=sort(unique(df$tp))
    l=which(ndf$Event & ndf$Time!=lastT)
    if(length(l)>0) 
      for(i in l) ndf$Time[i]=min(ltps[ltps>ndf$Time[i]])
  }
  
  shift=as.vector(scale((1:nlevels(df$grp))*shift0,scale=F))
  names(shift)=levels(df$grp)
  akms=lapply(levels(ndf$grp),function(i){ 
    tt=data.frame(unclass(survfit(Surv(Time,Event)~1,ndf[ndf$grp==i,]))[c("time","n.risk","n.event")])
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
    tt$grp=i;tt$n0=n0;
    tt$x0=tt$x
    tt$x=tt$x+shift[i]
    tt$y=round(tt$y,1)
    tt$color=ndf$color[ndf$grp==i][1]
    tt
  })
  names(akms)=levels(ndf$grp)
  
  a <- rCharts::Highcharts$new()
  for(i in names(akms))
    a$series(data = lapply(1:nrow(akms[[i]]),function(j) as.list(akms[[i]][j,])), name=i,color=akms[[i]]$color[1],type = "line")
  a$tooltip( formatter = "#! function() { return this.point.grp + ' at ' + this.point.x0 +
             ' (' + this.point.y + '/' + this.point.n0 + ')' ; } !#")
  #a$legend(enabled = F)
  a$yAxis(title = list(text = "Perc. surviving"), min = 0, max = 100, tickInterval = 20)
  
  #######################3
  v=paste(ndf$Id," (",ndf$Time,c("","+")[ndf$Event+1],")",sep="")
  names(v)=NULL
  lso=order(-ndf$Time)
  v=tapply(v[lso],ndf$grp[lso],sort)
  sumids=cbind(Group=names(v),"Censoring"=unlist(sapply(v,paste,collapse=" ")))
  
  exptxt=list('****Ids***',c('Group','Censoring'))
  for(i in 1:nrow(sumids)) exptxt=c(exptxt,list(sumids[i,]))
  
  ndf2=ndf[,-3];ndf2$grp=as.character(ndf2$grp)
  exptxt=c(exptxt,list('****Data***'),list(colnames(ndf2)))
  for(i in 1:nrow(ndf2)) exptxt=c(exptxt,list(ndf2[i,]))
  
  return(list(plot=a,df=ndf,akms=akms,sumids=sumids,exptxt=exptxt,resp=resp,lgrps=lgrps,lastT=lastT,lastM=lastM))
  }

compKM<-function(objres,ref,firth=FALSE){
  
  ndf=objres$df
  ndf$grp=relevel(ndf$grp,ref)
  
  exptxt=objres$exptxt
  modtab=hrtab=NULL
  if(nlevels(ndf$grp)>1){
    ####################
    if(!firth){
      mod=coxph(Surv(Time,Event)~grp,ndf)
      x=summary(mod)
      modtab=rbind(sprintf("Likelihood ratio: %.2f, p<%s",x$logtest["test"],myf(x$logtest["pvalue"])),
                   sprintf("Wald: %.2f, p<%s",x$waldtest["test"],myf(x$waldtest["pvalue"])),
                   sprintf("log-rank: %.2f, p<%s",x$sctest["test"],myf(x$sctest["pvalue"])))
      colnames(modtab)="Test"
      hrs=apply(x$coefficients,1,function(x) sprintf("%.2f [%.2f;%.2f]",exp(x[1]),exp(x[1]-1.96*x[2]),exp(x[1]+1.96*x[2])))
      hrpva=sprintf('%s',myf(p.adjust(x$coefficients[,5],"holm")))
      hrpv=sprintf('%s',myf(x$coefficients[,5]))
    }
    ####################
    if(firth){
      mod<-try(coxphf(Surv(Time,Event)~grp,ndf),TRUE)
      modtab=.infctKM(mod)
      colnames(modtab)="Test"
      hrs=apply(cbind(mod$coefficients,mod$ci.lower,mod$ci.upper),1,function(x) sprintf("%.2f [%.2f;%.2f]",exp(x[1]),exp(x[1]-1.96*x[2]),exp(x[1]+1.96*x[2])))
      hrpva=sprintf('%s',myf(p.adjust(mod$prob,"holm")))
      hrpv=sprintf('%s',myf(mod$prob))
    }
    ####################
    lrs=sapply(levels(ndf$grp)[-1],function(x) 
      summary(coxph(Surv(Time,Event)~factor(grp),ndf[ndf$grp%in%c(ref,x),]))$sctest["pvalue"])
    lrsa=sprintf('%s',myf(p.adjust(lrs,"holm")))
    lrs=sprintf('%s',myf(lrs))
    hrtab=cbind(Covariate=levels(ndf$grp)[-1],'Hazard ratio'=hrs,"HR Pval"=hrpv,"HR PvalAdj"=hrpva,"LR Pval"=lrs,"LR PvalAdj"=lrsa)
    rownames(hrtab)=NULL
    ####################

        exptxt=c(exptxt,list('***Stats***'),as.list(modtab[,1]))
    exptxt=c(exptxt,list(paste('***HR to',ref)),list(c('Covariate','Hazard ratio','HR pvalue','HR adj. pvalue','LR pvalue','LR adj. pvalue')))
    for(i in 1:nrow(hrtab)) exptxt=c(exptxt,list(hrtab[i,]))
    
    
  }
  
  list(df=ndf,mod=modtab,hr=hrtab,sumids=objres$sumids,exptxt=exptxt,
       resp=objres$resp,lgrps=objres$lgrps,lastT=objres$lastT,lastM=objres$lastM)
}
############################################################################################
############################################################################################

