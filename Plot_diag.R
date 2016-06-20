
##########################################################################
## General function for diagnostic plots
plotDiag<-function(datdiag,typplot="QQ-plot"){
  
  ## format qqplot
  if(typplot=="QQ-plot"){
    tmpqqp=datdiag$data[,c("resid","qt","Tp"  ,"Grp","Id","color")]
    names(tmpqqp)[1:3]=c("x","y","Tp")
    limxqq=datdiag$limxqq
    ab=c(range(limxqq),range(limxqq))
    if(nrow(tmpqqp)>800) tmpqqp=tmpqqp[rev(order(-abs(tmpqqp[,1]))[1:800]),] ## pb if >1000
    
    a <- rCharts::Highcharts$new()
    a$series(data = list(list(x=ab[1],y=ab[3]),list(x=ab[2],y=ab[4])), name='reg',type = "line",color='grey',
             enableMouseTracking=FALSE,showInLegend = FALSE, marker= list(enabled = FALSE))
    
    a$series(data = lapply(1:nrow(tmpqqp), function(i) 
      list(x=tmpqqp$x[i],y=tmpqqp$y[i],Grp=as.character(tmpqqp$Grp[i]),Id=as.character(tmpqqp$Id[i]),
           Tp=as.character(tmpqqp$Tp[i]),color=unname(tmpqqp$color[i]))),
      name='qqnorm',type = "scatter")
    a$tooltip( formatter = "#! function() { return this.point.Grp + ': ' + this.point.Id + ' (' +this.point.Tp + ')' ; } !#")
    a$legend(enabled = F)
    a$xAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    a$yAxis(title = list(text = "Quantile"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    return(a)
  }
  
  ## format resid/grp
  if(typplot=="Resid/Grp"){
    tmpbxp=datdiag$data[,c("Id","resid","Grp","color")]
    limxqq=datdiag$limxqq
    bwstats = setNames(as.data.frame(boxplot(resid ~ Grp, data = tmpbxp, plot = F)$stats), nm = NULL)
    #   print(bwstats)
    b <- Highcharts$new()
    b$set(series = list(list(name = 'Residuals',data = bwstats)))
    b$xAxis(labels=list(rotation = -90),categories = levels(tmpbxp$Grp),title = list(text = 'Group'))
    b$legend(enabled = F)
    b$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    b$chart(type = 'boxplot')
    return(b)
  }
  
  ## format resid/mid
  if(typplot=="Resid/Mice"){
    tmpbxp=datdiag$data[,c("Id","resid","Id","color")]
    limxqq=datdiag$limxqq
    ilevs=unlist(tapply(as.character(datdiag$data$Id),datdiag$data$Grp,unique))
    tmpbxp$Tp=factor(tmpbxp$Id,levels=ilevs)
    bwstats = setNames(as.data.frame(boxplot(resid ~ Id, data = tmpbxp, plot = F)$stats), nm = NULL)
    #   print(bwstats)
    b <- Highcharts$new()
    b$set(series = list(list(name = 'Residuals',data = bwstats)))
    b$xAxis(labels=list(rotation = -90),categories = levels(tmpbxp$Id),title = list(text = 'Mice'))
    b$legend(enabled = F)
    b$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    b$chart(type = 'boxplot')
    return(b)
  }
  
  ## format resid/Tp
  if(typplot=="Resid/Tp"){
    tmpbxp=datdiag$data[,c("Id","resid","Tp","color")]
    limxqq=datdiag$limxqq
    tmpbxp$Tp=factor(tmpbxp$Tp,levels=unique(tmpbxp$Tp))
    bwstats = setNames(as.data.frame(boxplot(resid ~ Tp, data = tmpbxp, plot = F)$stats), nm = NULL)
    #   print(bwstats)
    b <- Highcharts$new()
    b$set(series = list(list(name = 'Residuals',data = bwstats)))
    b$xAxis(labels=list(rotation = -90),categories = levels(tmpbxp$Tp),title = list(text = 'Group'))
    b$legend(enabled = F)
    b$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    b$chart(type = 'boxplot')
    return(b)
  }
  
  # format resid,fit
  if(typplot=="Resid/Fit"){
    tmpresf=datdiag$data[,c("resid","Fit" ,"Tp" ,"Grp","Id","color")]
    if(nrow(tmpresf)>800) tmpresf=tmpresf[rev(order(-abs(tmpresf$resid))[1:800]),]
    names(tmpresf)[1:2]=c("y","x")
    limxqq=datdiag$limxqq
    limyfit=pretty(tmpresf$x)
    c <- rCharts::Highcharts$new()
    #     c$series(data = list(list(x=min(limyfit),y=0),list(x=max(limyfit),y=0)), name='reg',type = "line",color='grey',lineWidth = 1,
    #              enableMouseTracking=FALSE,showInLegend = FALSE, marker= list(enabled = FALSE))
    ndf=data.frame(x=seq(min(limyfit),max(limyfit),length=100))
    if(datdiag$Type!="cs"){
      ndf=cbind(ndf,as.data.frame(predict(mgcv:::gam(y~s(x),data=tmpresf),newdata=ndf,se=T)))
      c$series(data = unname(apply(cbind(x=ndf$x,y=ndf$fit),1,as.list)), name='sm',type = "line",color='grey',lineWidth = 1.5,
               enableMouseTracking=FALSE,showInLegend = FALSE, marker= list(enabled = FALSE))
      c$series(data = unname(apply(cbind(x=ndf$x,y=ndf$fit-ndf$se.fit),1,as.list)), name='sm',type = "line",color='grey',lineWidth = 1,
               enableMouseTracking=FALSE,showInLegend = FALSE, marker= list(enabled = FALSE))
      c$series(data = unname(apply(cbind(x=ndf$x,y=ndf$fit+ndf$se.fit),1,as.list)), name='sm',type = "line",color='grey',lineWidth = 1,
               enableMouseTracking=FALSE,showInLegend = FALSE, marker= list(enabled = FALSE))
    }
    
    c$series(data = lapply(1:nrow(tmpresf),function(i) 
      list(x=tmpresf$x[i],y=tmpresf$y[i],Grp=as.character(tmpresf$Grp[i]),Id=as.character(tmpresf$Id[i]),
           Tp=as.character(tmpresf$Tp[i]),color=unname(tmpresf$color[i]))), name='resid',type = "scatter")
    c$tooltip( formatter = "#! function() { return this.point.Grp + ': ' + this.point.Id + ' (' +this.point.Tp + ')' ; } !#")
    c$legend(enabled = F)
    c$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    c$xAxis(title = list(text = "Fitted"), min = min(limyfit), max = max(limyfit), tickInterval = diff(limyfit)[1])
    return(c)
  }
  
  if(typplot=="ResidBt/Fit"){
    tmpresf=datdiag$data[,c("resid","Fite" ,"Tp" ,"Grp","Id","color")]
    if(nrow(tmpresf)>800) tmpresf=tmpresf[rev(order(-abs(tmpresf$resid))[1:800]),]
    names(tmpresf)[1:2]=c("x","y")
    limxqq=datdiag$limxqq
    limyfit0=datdiag$limyfit0
    d <- rCharts::Highcharts$new()
    d$series(data = lapply(1:nrow(tmpresf),function(i) 
      list(x=tmpresf$x[i],y=tmpresf$y[i],Grp=as.character(tmpresf$Grp[i]),Id=as.character(tmpresf$Id[i]),
           Tp=as.character(tmpresf$Tp[i]),color=unname(tmpresf$color[i]))), name='resid',type = "scatter")
    d$tooltip( formatter = "#! function() { return this.point.Grp + ': ' + this.point.Id + ' (' +this.point.Tp + ')' ; } !#")
    d$legend(enabled = F)
    d$yAxis(title = list(text = "Pearson residuals"), min = min(limxqq), max = max(limxqq), tickInterval = diff(limxqq)[1])
    d$xAxis(title = list(text = "Fitted"), min = min(limyfit0), max = max(limyfit0), tickInterval = diff(limyfit0)[1])
    return(d)
  }
  
  
  ## format Fit
  if(typplot%in%c("FitBt","Fit")){
    if(typplot=="FitBt"){
      tmpdata=datdiag$data[,c("Resp_ori" ,"Tp" ,"Grp","Id","color")]
      tmppred=datdiag$pred[,c("x","Grp","color","ybt","ybtmin","ybtmax")]
      names(tmppred)=c("x","Grp","color","y","ymin","ymax")
      limtp=datdiag$limtp
      limyfit=datdiag$limyfit
      ytitle=paste("Response",datdiag$Resp,"backtransformed")
    }
    if(typplot=="Fit"){
      tmpdata=datdiag$data[,c("Resp" ,"Tp" ,"Grp","Id","color")]
      tmppred=datdiag$pred[,c("x","Grp","color","y","ymin","ymax")]
      limtp=datdiag$limtp
      limyfit=datdiag$limyfit0
      ytitle=paste("Response ",datdiag$Resp," (",datdiag$Trans,")",sep="")
    }
    names(tmpdata)[1:2]=c("y","x")
    gcols=tapply(tmppred$color,tmppred$Grp,unique)
    idcols=tapply(tmpdata$color,tmpdata$Id,unique)
    #idcols=add.alpha(idcols,0.5)
    a <- rCharts::Highcharts$new()
    for(i in unique(tmppred$Grp)){
      lids=unique(as.character(tmpdata$Id[tmpdata$Grp==i]))
      #     print(lids)
      k=lids[1]
      a$series(data = lapply(which(tmpdata$Id==k),function(j) as.list(tmpdata[j,c("x","y","Grp","Id")])),
               name=paste(i,": raw",sep=""),k,type = "line",
               color=unname(gcols[i]),lineWidth = 1,
               showInLegend = TRUE, marker= list(enabled = FALSE))
      
      if(length(lids)>1) for(k in lids[-1])
        a$series(data = lapply(which(tmpdata$Id==k),function(j) as.list(tmpdata[j,c("x","y","Grp","Id")])),
                 linkedTo=':previous', name=k,type = "line",
                 color=unname(gcols[i]),
                 lineWidth = 1,
                 showInLegend = FALSE, marker= list(enabled = FALSE))
      
      a$series(data = lapply(which(tmppred$Grp==i),function(j) as.list(tmppred[j,c("x","y","Grp")])), type = "line",
               name=paste(i,": fit",sep=""),
               fillOpacity = 0.2,lineWidth = 4,
               color=unname(gcols[i]),
               zIndex = 1)
      a$series(data = lapply(which(tmppred$Grp==i),function(j) 
        unname(as.list(tmppred[j,c("x","ymin","ymax")]))),type = "arearange",name=paste(i,": 95%CI",sep=""),# showInLegend = FALSE,
        fillOpacity = 0.3,lineWidth = 2,
        color=unname(gcols[i]),
        zIndex = 0)
    }
    a$xAxis(title = list(text = "Time"), min = min(limtp), max = max(limtp), tickInterval = diff(limtp)[1])
    a$yAxis(title = list(text =ytitle, min =  min(limyfit), max = max(limyfit), tickInterval = diff(limyfit)[1]))
    a$legend(verticalAlign = "right", align = "right", layout = "vertical", title = list(text = "Treatment"))
    #  a
    return(a)
  }
  
  return(NULL)
}

