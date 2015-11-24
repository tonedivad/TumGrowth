################################################################################################
################################################################################################

runmod<-function(idf,checkvar=TRUE){
  ga=gls(Resp~tp*grp, data=idf[!idf$out,],correlation=corCompSymm(form=~1|Id))
  ga2=gls(Resp~tp*grp, data=idf[!idf$out,],correlation=corCAR1(form=~tp|Id))
  if(AIC(ga2)<AIC(ga)) ga=ga2
  if(checkvar){
    gaw=update(ga,weights=varIdent(form=~1|grp))
    if(AIC(gaw)<AIC(ga) & anova(ga,gaw)[2,9]<0.05 )
      ga=gaw
  }
  ga
}

#######################################################################################################################
compMod<-function(data,grps,resp,bfco=0.1,checkvar=TRUE){
  
  
  idf=data[data$grp%in%grps & !is.na(data[,resp]) & data$Use,]
  idf$grp=factor(idf$grp,levels = grps)
  idf$Resp=idf[,resp]
  idf$Id=factor(idf$Id)
  idf=idf[order(idf$grp,idf$Id,idf$tp),]
  idf$out=FALSE
  
  ga=runmod(idf,checkvar)
  out=outlierTest(lm(residuals(ga,'pearson')~1),cutoff = bfco)
  if(any(out$signif)){
    idf[names(out$bonf.p),]$out=T
    ga=runmod(idf,checkvar)
  }
  
  
  fmm=update(ga,method="ML")
  fmm2=update(fmm,model=Resp~tp+grp)
  fmm21=update(fmm,model=Resp~grp)
  fmm22=update(fmm,model=Resp~tp)
  
  #######################
  
  antab=rbind("Time x Treat"=anof(anova(fmm,fmm2)),
              "Time"=anof(anova(fmm2,fmm21)),
              "Treat"=anof(anova(fmm2,fmm22)))
  colnames(antab)="LR test"
  
  
  ################################################################################################
  # Format output
  tab=summary(ga)$tT
  rownames(tab)=gsub("^grp","",rownames(tab))
  tab=data.frame(tab)
  tab[,4]=sapply(tab[,4],function(x) sprintf("%.4f",x))
  for(i in 1:3) tab[,i]=sapply(tab[,i],function(x) sprintf("%.3f",x))
  
  corrCoef=weightCoef=NULL
  mstruct=coef(ga$modelStruct,unconstrained=FALSE)
  if(length(grep("^corStruct",names(mstruct)))>0){
    corrCoef=mstruct[grep("^corStruct",names(mstruct))]
    corrCoef=sprintf("%s=%.3f",tolower(gsub("^corStruct\\.","",names(corrCoef))),corrCoef)
  }
  if(length(grep("^varStruct",names(mstruct)))>0){
    weightCoef=mstruct[grep("^varStruct",names(mstruct))]
    names(weightCoef)=gsub("^varStruct\\.","",names(weightCoef))
    weightCoef=weightCoef[levels(idf$grp)]
    weightCoef[is.na(weightCoef)]=1
    names(weightCoef)=levels(idf$grp)
    #weightCoef=round(weightCoef,3)
  }
  
  list(model=ga,coef=tab,antab=antab,weightCoef=weightCoef,corrCoef=corrCoef,data=idf)
}

########################
compMod2<-function(objres,ref){
  
  idf=objres$data
  ga=objres$model
  if(!ref%in%levels(idf$grp)){
    ct=contrMat(table(idf$grp),type="Tukey")[,-1,drop=F]
    jt1=cbind(0,0,ct,matrix(0,nrow=nrow(ct),ncol=ncol(ct)))
    jt2=cbind(0,0,matrix(0,nrow=nrow(ct),ncol=ncol(ct)),ct)
    jtab=do.call("rbind",lapply(1:nrow(jt1),function(i) esticon(ga,rbind(jt1[i,],jt2[i,]),join=T)))
    jtabtop=cbind(Largest=gsub(" - .*","",rownames(jt1)),Lowest=gsub(".* - ","",rownames(jt1)),
                  ChiSq=sprintf("%.2f",jtab[,1]),Pvalue=myf(jtab[,3]),AdjPvalue=myf(p.adjust(jtab[,3],"holm")))
    ltps=sort(unique(idf$tp))
    amct=lapply(1:nrow(jt1),function(i) 
      sapply(ltps,function(itp) c(0,0,contrMat(table(idf$grp),type="Tukey")[i,-1],contrMat(table(idf$grp),type="Tukey")[i,-1]*itp)))
    jtabs=lapply(amct,function(x) esticon(ga,t(x)))
    lsign=sapply(jtabs,function(x) sign(mean(x[,2])))
    for(isign in which(lsign<0)) jtabtop[isign,1:2]=jtabtop[isign,2:1]
  }
  
  if(ref%in%levels(idf$grp)){
    idf$grp=relevel(idf$grp,ref)
    gai=update(ga,data=idf[!idf$out,])
    ct=contrMat(table(idf$grp),type="Dunnet")[,-1,drop=F]
    jt1=cbind(0,0,ct,matrix(0,nrow=nrow(ct),ncol=ncol(ct)))
    jt2=cbind(0,0,matrix(0,nrow=nrow(ct),ncol=ncol(ct)),ct)
    jtab=do.call("rbind",lapply(1:nrow(jt1),function(i) esticon(gai,rbind(jt1[i,],jt2[i,]),join=T)))
    jtabtop=cbind(Group=gsub(" - .*","",rownames(jt1)),
                  ChiSq=sprintf("%.2f",jtab[,1]),Pvalue=myf(jtab[,3]),AdjPvalue=myf(p.adjust(jtab[,3],"holm")))
  }
  jtabtop=data.frame(jtabtop,stringsAsFactors = FALSE)
  jtabtop$ChiSq=as.numeric(jtabtop$ChiSq)
  rownames(jtabtop)=NULL
  
  exptxt=list('***ANOVA***',c("Term","LR test"))
  antab=objres$antab
  for(i in 1:nrow(antab)) exptxt=c(exptxt,list(antab[i,]))
  exptxt=c(exptxt,list('***PW comparison***'),list(c(colnames(jtabtop))))
  for(i in 1:nrow(jtabtop)) exptxt=c(exptxt,list(jtabtop[i,]))
#  print(exptxt)
  list(model=objres$model,coef=objres$tab,pw=jtabtop,antab=objres$antab,weightCoef=objres$weightCoef,
       corrCoef=objres$corrCoef,data=objres$data,exptxt=exptxt)
}



################################################################################################
### Diagplots

plotDiag<-function(mod,typplot="qqplot"){
  
  tmpdata=mod$data[!mod$data$out,c("Resp","grp","tp","Id")]
  
  x=resid(mod$model,"pearson")
  lso=order(order(x))
  y=qnorm(ppoints(length(x)))[lso]
  fit=predict(mod$model)
  levsid=unique(unlist(tapply(as.character(tmpdata$Id),tmpdata$grp,sort)))
  tmpdata$Id=factor(tmpdata$Id,levels=levsid)
  tmpdata$x=round(x,2)
  tmpdata$y=round(y,2)
  tmpdata$fit=round(fit,2)
  xlim=c(floor(min(x*1.95)),ceiling(max(x*2.05)))/2
  ylim=c(floor(min(y*1.95)),ceiling(max(y*2.05)))/2
  flim=c(floor(min(fit*1.95)),ceiling(max(fit*2.05)))/2;dfim=0.5
  if(diff(flim)>10){flim=c(floor(min(fit*19.5)),ceiling(max(fit*20.5)))/20;dfim=5}
  if(diff(flim)>100){flim=c(floor(min(fit*195)),ceiling(max(fit*205)))/200;dfim=50}
  
  ## format qqplot
  if(typplot=="QQ-plot"){
    ab=c(max(xlim[1],ylim[1]),max(xlim[2],ylim[2]))
    a <- rCharts::Highcharts$new()
    a$series(data = list(list(x=ab[1],y=ab[1]),list(x=ab[2],y=ab[2])), name='reg',type = "line",color='grey')
    a$series(data = lapply(1:nrow(tmpdata),function(i) as.list(tmpdata[i,])), name='qqnorm',type = "scatter")
    a$tooltip( formatter = "#! function() { return this.point.grp + ': ' + this.point.Id + ' (' +this.point.tp + ')' ; } !#")
    a$legend(enabled = F)
    a$yAxis(title = list(text = "Quantile"), min = ylim[1], max = ylim[2], tickInterval = 0.5)
    a$xAxis(title = list(text = "Pearson residuals"), min = xlim[1], max = xlim[2], tickInterval = 0.5)
    return(a)
  }
  
  ## format resid/mid
  if(typplot=="Resid/Mice"){
    bwstats = setNames(as.data.frame(boxplot(x ~ Id, data = tmpdata, plot = F)$stats), nm = NULL)
    b <- Highcharts$new()
    b$set(series = list(list(name = 'Residuals',data = bwstats)))
    b$xAxis(labels=list(rotation = -90),categories = levsid,title = list(text = 'Mice ids'))
    b$legend(enabled = F)
    b$yAxis(title = list(text = "Pearson residuals"), min = xlim[1], max = xlim[2], tickInterval = 0.5)
    b$chart(type = 'boxplot')
    return(b)
  }
  
  # format resid,fit
  tmpdata$y=tmpdata$x
  tmpdata$x=tmpdata$fit
  c <- rCharts::Highcharts$new()
  c$series(data = list(list(x=flim[1],y=0),list(x=flim[2],y=0)), name='reg',type = "line",color='grey')
  c$series(data = lapply(1:nrow(tmpdata),function(i) as.list(tmpdata[i,])), name='resid',type = "scatter")
  c$tooltip( formatter = "#! function() { return this.point.grp + ': ' + this.point.Id + ' (' +this.point.tp + ')' ; } !#")
  c$legend(enabled = F)
  c$yAxis(title = list(text = "Pearson residuals"), min = xlim[1], max = xlim[2], tickInterval = 0.5)
  c$xAxis(title = list(text = "Fitted"), min = flim[1], max = flim[2], tickInterval = dfim)
  return(c)
  
}

############################################################################################
############################################################################################
