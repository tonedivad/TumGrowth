library(shiny)
library(nlme)
library(xtable) ##keep
library(multcomp)
library(survival)
library(car)
library(coxphf)
library(beeswarm)
library(rCharts)
library(DT)

## add?
library(rms)
library(shinyjs)
library(rmarkdown)


.myf<-function(pvs,n=4){
  
  myf<-function(pv,n=4){
    if(pv<(10^-n)) return(paste(c("<0.",rep(0,n-1),"1"),collapse=""))
  return(sprintf(paste("%.",n,"f",sep=""),pv)) 
  }
  sapply(pvs,myf,n=n)
}


add.alpha=function (hex.color.list, alpha) 
  sprintf("%s%02X", hex.color.list, floor(alpha * 256))

.myfpv<-function(pvs){
  pvt=rep("",length(pvs))
  pvt[which(pvs<=0.1)]="."
  pvt[which(pvs<=0.05)]="*"
  pvt[which(pvs<=0.01)]="**"
  pvt[which(pvs<=0.001)]="***"
  pvt
}

# getylim<-function(v){
#   ylim=range(v,na.rm=T)
#   ylim=ylim+c(-1,1)*diff(ylim)/20;ylim[1]=max(ylim[1],0);dylim=5
#   if(diff(ylim)<10){ylim[1]=floor(ylim[1]);ylim[2]=ceiling(ylim[2]);dylim=1}
#   if(diff(ylim)>100){ylim[1]=floor(ylim[1]/100)*100;ylim[2]=ceiling(ylim[2]/50)*50;dylim=50}
#   return(list(ylim=ylim,dylim=dylim))
# }
# anof<-function(x) sprintf("%.2f, p<%s%s",x[2,8],myf(x[2,9]),myfpv(x[2,9]))
# 

############################################################################################
getCols<-function(grp,mid=NULL,what=4){
  require(RColorBrewer)
  pals=list(rev(brewer.pal(9,"Greens")[c(4,7)]),
            rev(brewer.pal(9,"Blues")[c(5,8)]),
            rev(brewer.pal(9,"Reds")[c(4,8)]),
            rev(brewer.pal(9,"Purples")[c(5,7)]),
            rev(brewer.pal(9,"Greys")[c(4,7)]),
            rev(brewer.pal(9,"Oranges")[c(4,7)]),
            brewer.pal(11,"BrBG")[c(3,2)],
            brewer.pal(11,"BrBG")[c(10,9)])
  pals=c(pals,pals)
  pals0=sapply(pals,function(x) rev(colorRampPalette(x)(7))[what])
  lugrp=unique(grp)
  if(is.null(mid)){
    lcols=pals0[1:length(lugrp)]
    names(lcols)=lugrp
    return(lcols)
  }
  
  unlist(lapply(1:length(lugrp),function(x){
    lmids=unique(mid[grp==lugrp[x]])
    lcols=colorRampPalette(pals[[x]])(length(lmids))
    names(lcols)=lmids
    lcols
  }))
  
}

################################################################################################
################################################################################################

plotLineC<-function(df,resp,lgrps,force2zero=FALSE,type='All',se=TRUE,defzero=NA,miny=NA,maxy=NA){
  
  df$Resp2=round(df[,resp],3)
  xlim=pretty(seq(ifelse(force2zero,0,min(df$Tp)),max(df$Tp),length=9))
  
  if(is.na(miny)){
    miny=round(min(df$Resp2,na.rm=T),3)
  if(force2zero & all(df$Tp>0)){
    miny=ifelse(grepl("\\.log",resp),min(df$Resp2,na.rm=T)-log(2),0)
    miny=ifelse(is.na(defzero),miny,round(defzero,3))
  }
    miny=0.95*miny
  }
  
  df=df[!is.na(df$Resp2),]
  if(is.na(maxy)) maxy=max(df$Resp2)
  ylim=pretty(c(miny,maxy))
  idf=df[df$Grp%in%lgrps & !is.na(df$Resp2) & df$Use,c("Tp","Grp","Id","Resp2","color")]
  idf$Id=factor(idf$Id)
  idf$Grp=factor(idf$Grp)
  if(force2zero & all(df$Tp>0)){
    aidf=idf[tapply(1:nrow(idf),idf$Id,function(x) x[1]),]
    aidf$Tp=0
    aidf$Resp2=miny
    idf=rbind(aidf,idf)
    idf=idf[order(idf$Grp,idf$Id,idf$Tp),]
    idf$Resp2[which(idf$Resp2<=miny)]=miny
  }
 
  ##################################
  tmpdata=do.call("rbind",lapply(levels(idf$Grp),function(i){
    l=which(idf$Grp==i)
  tmp=t(do.call("cbind",tapply(idf$Resp2[l],idf$Tp[l],function(x) 
    c(n=length(x),y=mean(x),sd=sd(x),se=sd(x)/sqrt(length(x))))))
  data.frame(cbind(tmp,x=tapply(idf$Tp[l],idf$Tp[l],unique)),Grp=i,color=idf$color[l][1],stringsAsFactors=F)
}))
  ##################################
  
  idf$Grp=as.character(idf$Grp)
  idf$Id=as.character(idf$Id)
  names(idf)[which(names(idf)=="Resp2")]="y"
  names(idf)[which(names(idf)=="Tp")]="x"
  
  a <- rCharts::Highcharts$new()
  if(type!='All'){
    for(i in unique(idf$Grp)){
      tmp=tmpdata[tmpdata$Grp==i,]
      tmp$ymin=round(tmp$y-ifelse(se,1,0)*tmp$se-ifelse(se,0,1)*tmp$sd,3)
      tmp$ymax=round(tmp$y+ifelse(se,1,0)*tmp$se+ifelse(se,0,1)*tmp$sd,3)
      if(any(is.na(tmp$se))) tmp$ymin[is.na(tmp$se)]=tmp$ymax[is.na(tmp$se)]=tmp$y[is.na(tmp$se)]
      tmp$y=round(tmp$y,3)
      tmp$se=round(tmp$se,3)
      
      a$series(data = lapply(1:nrow(tmp),function(j) as.list(tmp[j,])), name=i,type = "line",color=tmp$color[1],lineWidth=4)
      a$series(data = lapply(which(tmp$Grp==i),function(j) 
        unname(as.list(tmp[j,c("x","ymin","ymax")]))),type = "arearange",# showInLegend = FALSE,
        fillOpacity = 0.3,lineWidth = 0,color=unname(tmp$color[which(tmp$Grp==i)][1]),zIndex = 0)
      
    }
  }
  if(type%in%c('All','Both')){
      for(i in unique(idf$Id))
      a$series(data = unname(lapply(which(idf$Id==i),function(j) as.list(idf[j,c("x","y","Grp","Id")]))), name=i,type = "line",
               color=unname(idf$color[which(idf$Id==i)][1]))
    a$tooltip( formatter = "#! function() { return this.point.Id + ' (' + this.point.Grp + ') at ' + this.point.x + ': ' + this.point.y ; } !#")
  }

  a$yAxis(title = list(text = "Response"), min = min(ylim), max = max(ylim), tickInterval = diff(ylim)[1])
  a$xAxis(title = list(text = "Time"), min =  min(xlim), max = max(xlim), tickInterval = diff(xlim)[1])
  a$legend(verticalAlign = "right", align = "right", layout = "vertical", title = list(text = "Mice"))
  return(list(plot=a,df=idf,sesd=tmpdata,xlim=xlim,ylim=ylim,Resp=resp))

  
}

