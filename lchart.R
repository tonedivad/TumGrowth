################################################################################################
## plot line charts
getLGmat<-function(cdat,resp=cdat$Resp[1],lgrps=levels(cdat$dataM$Grp),
                   gcols=getCols(cdat$dataM$Grp),trans='None'){
  
  if(length(lgrps)==0) lgrps=levels(cdat$dataM$Grp)
  iresp=resp
  if(trans!='None') iresp=paste(resp,tolower(trans),sep=".")
  lmids=cdat$dataM$Id[cdat$dataM$Use & cdat$dataM$Grp%in%lgrps]
  
  l=which(cdat$data$Id%in%lmids & !is.na(cdat$data[,iresp]))
  df=cdat$data[l,c("Id","Tp",iresp,resp)]
  names(df)=c("Id","Tp","Resp","Resp_ori")
  df$Grp=factor(cdat$dataM[df$Id,]$Grp)
  idlevs=unlist(tapply(as.character(df$Id),df$Grp,unique))
  df$Id=factor(df$Id,levels=idlevs)
  df$color=gcols[as.character(df$Grp)]
  df=df[order(df$Grp,df$Id,df$Tp),]

  return(list(Df=df,Resp=resp,Trans=trans))
}

getLGmat2<-function(cdat,resp=cdat$Resp[1],lgrps=levels(cdat$dataM$Grp),
                   gcols=getCols(cdat$dataM$Grp)){
  
  if(length(lgrps)==0) lgrps=levels(cdat$dataM$Grp)
  lmids=cdat$dataM$Id[cdat$dataM$Use & cdat$dataM$Grp%in%lgrps]
  
  l=which(cdat$data$Id%in%lmids & !is.na(cdat$data[,resp]))
  df=cdat$data[l,c("Id","Tp",resp)]
  names(df)=c("Id","Tp","Resp")
  
  iresp=paste(resp,c("log","sqrt","curt"),sep=".")
  iresp= iresp[iresp%in%names(cdat$data)]
  nresp=gsub(paste("^",resp,"\\.",sep=""),"Resp_",iresp)
  df[,nresp]=cdat$data[l,iresp]
  
  df$Grp=factor(cdat$dataM[df$Id,]$Grp)
  idlevs=unlist(tapply(as.character(df$Id),df$Grp,unique))
  df$Id=factor(df$Id,levels=idlevs)
  df$color=gcols[as.character(df$Grp)]
  df=df[order(df$Grp,df$Id,df$Tp),]
  
  return(list(Df=df,Resp=resp,Trans=gsub(paste("^",resp,"\\.",sep=""),"",iresp)))
}

plotLineC<-function(lgdata,type=c('tc','mese'),force2zero=FALSE,defzero=NA,miny=NA,maxy=NA){
  

  #force2zero=FALSE;type='All';se=TRUE;defzero=NA;miny=NA;maxy=NA;gcols=getCols(cdat$dataM$Grp)
  if(length(type)==0) type='tc'
  se=('mese'%in%type)
  
  idf=lgdata$Df
  resp=lgdata$Resp
  gcols=tapply(lgdata$Df$color,lgdata$Df$Grp,unique)
  ### set ylim and xlim
  xlim=pretty(seq(ifelse(force2zero,0,min(idf$Tp)),max(idf$Tp),length=9))
  if(is.na(miny)){
    miny=round(min(idf$Resp,na.rm=T),3)
    if(force2zero & all(idf$Tp>0,na.rm=T))  miny=ifelse(is.na(defzero),miny,round(defzero,3))
  }
  if(is.na(maxy)) maxy=max(idf$Resp)
  ylim=pretty(sort(c(miny,maxy)))
  
  #### pad for t=0
#  idf=df
  if(force2zero & all(idf$Tp>0)){
    aidf=idf[tapply(1:nrow(idf),idf$Id,function(x) x[1]),]
    aidf$Tp=0
    aidf$Resp=miny
    idf=rbind(aidf,idf)
    idf=idf[order(idf$Grp,idf$Id,idf$Tp),]
    idf$Resp[which(idf$Resp<=miny)]=miny
  }
  
  ###### compute sd/se/me data for plot
  tmpdata=do.call("rbind",lapply(levels(idf$Grp),function(i){
    l=which(idf$Grp==i)
    tmp=t(do.call("cbind",tapply(idf$Resp[l],idf$Tp[l],function(x) 
      c(n=length(x),y=mean(x),sd=sd(x),se=sd(x)/sqrt(length(x))))))
    data.frame(cbind(tmp,x=tapply(idf$Tp[l],idf$Tp[l],unique)),Grp=i,color=idf$color[l][1],stringsAsFactors=F)
  }))
  ##################################
  
  levgrp=levels(idf$Grp)
  idf$Grp=as.character(idf$Grp)
  idf$Id=as.character(idf$Id)
  names(idf)[which(names(idf)=="Resp")]="y"
  names(idf)[which(names(idf)=="Tp")]="x"
  
  a <- rCharts::Highcharts$new()
  if(any(c('mese','mesd')%in%type)){
    for(i in levgrp){
      tmp=tmpdata[tmpdata$Grp==i,]
      tmp$ymin=round(tmp$y-ifelse(se,1,0)*tmp$se-ifelse(se,0,1)*tmp$sd,3)
      tmp$ymax=round(tmp$y+ifelse(se,1,0)*tmp$se+ifelse(se,0,1)*tmp$sd,3)
      if(any(is.na(tmp$se))) tmp$ymin[is.na(tmp$se)]=tmp$ymax[is.na(tmp$se)]=tmp$y[is.na(tmp$se)]
      tmp$y=round(tmp$y,3)
      tmp$se=round(tmp$se,3)
      
      a$series(data = lapply(1:nrow(tmp),function(j) as.list(tmp[j,c("x","y","Grp")])),
               name=i,type = "line",color=tmp$color[1],lineWidth=4)
      a$series(data = lapply(which(tmp$Grp==i),function(j) 
        unname(as.list(tmp[j,c("x","ymin","ymax")]))),type = "arearange",name=paste(i,": ",ifelse(se,"SE","SD"),sep=""),
        fillOpacity = 0.3,lineWidth = 0,color=unname(tmp$color[which(tmp$Grp==i)][1]),zIndex = 0)
    }
  }
  if('tc'%in%type){
    for(i in unique(idf$Id))
      a$series(data = unname(lapply(which(idf$Id==i),function(j) as.list(idf[j,c("x","y","Grp","Id")]))), name=i,type = "line",
               color=unname(idf$color[which(idf$Id==i)][1]))
    a$tooltip( formatter = "#! function() { return this.point.Id + ' (' + this.point.Grp + ') at ' +
               this.point.x + ': ' + this.point.y ; } !#")
  }
  
  a$yAxis(title = list(text = "Response"), min = min(ylim), max = max(ylim), tickInterval = diff(ylim)[1])
  a$xAxis(title = list(text = "Time"), min =  min(xlim), max = max(xlim), tickInterval = diff(xlim)[1])
  a$legend(verticalAlign = "right", align = "right", layout = "vertical", title = list(text = "Mice"))
  
  return(list(plot=a,df=idf,sesd=tmpdata,xlim=xlim,ylim=ylim,Resp=resp))
  
  
}

