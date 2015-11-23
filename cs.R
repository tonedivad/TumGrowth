############################################################################################
plotCS<-function(df,resp,lgrps,rangetp,usemax=FALSE){
  
  if(length(lgrps)==0) lgrps=levels(df$grp)
  df$Resp=df[,resp]
  if(length(lgrps)<1) lgrps=levels(df$grp)
#  print(rangetp)
  l=which(df$grp%in%lgrps & !is.na(df$Resp) & df$tp>=rangetp[1]  & df$tp<=rangetp[2])

  idf=df[l,]
ylim=range(idf$Resp,na.rm=T)
ylim=pretty(seq(ylim[1],ylim[2],length.out = 8))
if(all(idf$Resp>0,na.rm=T)) ylim[1]=max(0,ylim[1])

idf=idf[order(idf$grp,idf$Id,idf$tp),]
  l2use=tapply(1:nrow(idf),idf$Id,function(x) rev(x)[1])
  if(usemax) l2use=tapply(1:nrow(idf),idf$Id,function(x) x[which.max(idf$Resp[x])])
  idf=idf[l2use,]
  idf$grp=factor(idf$grp)
  newrange=range(idf$tp)
  if(newrange[1]<newrange[2]) ylab=paste(ifelse(usemax,"Max. r","R"),"esponse in time range: ",newrange[1],"-",newrange[2],sep="")
  if(newrange[1]==newrange[2]) ylab=paste("Response at time=",newrange[1],sep="")
  
  
  idf$Resp=round(idf$Resp,2)
  bwstats = setNames(as.data.frame(boxplot(Resp ~ grp, data = idf, plot = F)$stats), nm = NULL)
  b <- Highcharts$new()
  b$set(series = list(list(name = 'Residuals',data = bwstats)))
  b$xAxis(labels=list(rotation = -90),categories = lgrps,title = list(text = 'Groups'))
  b$xAxis(categories = paste(lgrps," (",table(idf$grp)[lgrps],")",sep=""),title = list(text = 'Groups'))
  b$legend(enabled = F)
  b$yAxis(title = list(text = ylab), min = min(ylim), max = max(ylim), tickInterval = diff(ylim)[1])
  b$chart(type = 'boxplot')
  
  #####
  v=idf$Id
  if(length(unique(newrange))>1) v=paste(idf$Id," (",idf$tp,")",sep="")
  names(v)=NULL
  lso=order(-idf$tp)
  v=tapply(v[lso],idf$grp[lso],sort)
  sumids=cbind(Group=names(v),Animals=unlist(sapply(v,paste,collapse=" ")))
  #####
  exptxt=list('****Ids***',c('Group','Animals'))
  for(i in 1:nrow(sumids)) exptxt=c(exptxt,list(sumids[i,]))
  
  
  idf2=idf;idf2$grp=as.character(idf2$grp)
  exptxt=c(exptxt,list('****Data***'),list(colnames(idf2)[1:5]))
  for(i in 1:nrow(idf)) exptxt=c(exptxt,list(idf2[i,1:5]))
  
  
  return(list(plot=b,df=idf,tp=newrange,sumids=sumids,exptxt=exptxt,resp=resp,lgrps=lgrps))
}

###############
compCS<-function(objres,bfco=0.1,checkvar=TRUE,ref='All'){
  
  idf=objres$df
  lgrps=levels(idf$grp)
  sumids=objres$sumids
  
  exptxt=list('****Ids***',c('Group','Animals'))
  for(i in 1:nrow(sumids)) exptxt=c(exptxt,list(sumids[i,]))
  
  
  #####
  cttab=modtab=NULL
  if(sum(table(idf$grp)>1)>1){
    
    idf2=idf[idf$grp %in% names(which((table(idf$grp)>1))),]
    idf2$grp=factor(idf2$grp)
    idf2$out=FALSE
    if(ref%in%levels(idf2$grp)) idf2$grp=relevel(idf2$grp,ref)
    mod=gls(Resp~grp,data=idf2[!idf2$out,])
    if(checkvar){
      modw=gls(Resp~grp,data=idf2[!idf2$out,],weights = varIdent(form=~1|grp))
      if(anova(mod,modw)[2,9] & AIC(modw)<AIC(mod)) mod=modw
    }
    out=outlierTest(lm(residuals(mod,'pearson')~1),cutoff = bfco)
    if(out$signif){
      idf2[names(out$rstudent),]$out=TRUE
      mod=gls(Resp~grp,data=idf2[!idf2$out,])
      if(checkvar){
        modw=gls(Resp~grp,data=idf2[!idf2$out,],weights = varIdent(form=~1|grp))
        if(anova(mod,modw)[2,9] & AIC(modw)<AIC(mod)) mod=modw
      }
    }
    aovres=anova(update(mod,method="ML"),update(mod,model=.~1,method="ML"))
    #print("OKK1")
    ctmat=contrMat(table(idf2$grp),ifelse(ref%in%levels(idf2$grp),'Dunnet',"Tukey"))
    ctmat[,1]=0
    hrs=esticon(mod,ctmat)
    hrpv=sprintf('%s',myf(hrs[,6]))
    hrpva=sprintf('%s',myf(p.adjust(hrs[,6],"holm")))
    hrs=sprintf("%.2f [%.2f;%.2f]",hrs[,2],hrs[,7],hrs[,8])
    
    lrs<-suppressWarnings(sapply(strsplit(rownames(ctmat)," - "),function(x) wilcox.test(Resp~factor(grp),idf2[idf2$grp%in%x,])$p.value))
    lrspv=sprintf('%s',myf(lrs))
    lrspva=sprintf('%s',myf(p.adjust(lrs,"holm")))
    
    cttab=cbind(Group=gsub(" - .*","",rownames(ctmat)),Comp=gsub(".* - ","",rownames(ctmat)),
                Contrasts=hrs,Pvalue=hrpv,PvalueAdj=hrpva,WilPVal=lrspv,WilPvalAdj=lrspv)
    #print("OKK2")
    
    exptxt=c(exptxt,list('***Stats***'),list(colnames(cttab)))
    for(i in 1:nrow(cttab)) exptxt=c(exptxt,list(cttab[i,]))
    
    if(ref%in%levels(idf2$grp)) cttab=cttab[,-2,drop=F]
    
    kt=kruskal.test(Resp~grp,data=idf2[!idf2$out,])
    #print("OKK3")
    #print("OKK4")
    modtab=rbind(sprintf("Kruskal test: %.2f, p<%s",kt$statistic,myf(kt$p.value)),
                 sprintf("LR test: %.2f, p<%s", aovres[2,8],myf( aovres[2,9])))
    colnames(modtab)="Test"
    
    
  }
  
  
  idf2=idf;idf2$grp=as.character(idf2$grp)
  exptxt=c(exptxt,list('****Data***'),list(colnames(idf2)[1:5]))
  for(i in 1:nrow(idf2)) exptxt=c(exptxt,list(idf2[i,1:5]))
  
  return(list(plot=objres$plot,df=idf,tp=objres$newrange,sumids=sumids,
              exptxt=exptxt,ct=cttab,resp=objres$resp,mod=modtab,lgrps=objres$lgrps))
  
}

