library(shiny)
library(nlme)
library(xtable)
library(multcomp)
library(survival)
library(doBy)
library(RColorBrewer)
library(car)
library(coxphf)
library(beeswarm)
library(rCharts)
library(DT)
library(AICcmodavg)


.myf<-function(pvs){
  
  if(pvs<0.0001) return("<0.0001")
  return(sprintf("%.4f",pvs))  
}
myf<-function(pvs) sapply(pvs,.myf)

add.alpha=function (hex.color.list, alpha) 
  sprintf("%s%02X", hex.color.list, floor(alpha * 256))

myfpv<-function(pvs){
  pvt="*"
  if(pvs>0.1) return("")
  if(pvs<=0.1) pvt="."
  if(pvs<=0.05) pvt="*"
  if(pvs<=0.01) pvt="**"
  if(pvs<=0.001) pvt="***"
  pvt
}

getylim<-function(v){
  ylim=range(v,na.rm=T)
  ylim=ylim+c(-1,1)*diff(ylim)/20;ylim[1]=max(ylim[1],0);dylim=5
  if(diff(ylim)<10){ylim[1]=floor(ylim[1]);ylim[2]=ceiling(ylim[2]);dylim=1}
  if(diff(ylim)>100){ylim[1]=floor(ylim[1]/100)*100;ylim[2]=ceiling(ylim[2]/50)*50;dylim=50}
  return(list(ylim=ylim,dylim=dylim))
}
anof<-function(x) sprintf("%.2f, p<%s%s",x[2,8],myf(x[2,9]),myfpv(x[2,9]))


############################################################################################
getCols<-function(grp,mid=NULL,what=4){
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

plotTC3<-function(df,resp,lgrps,force2zero=FALSE,dogrps=TRUE,type='All',se=TRUE,defzero=NA){
  
  df$Resp2=round(df[,resp],3)
  xlim=pretty(seq(ifelse(force2zero,0,min(df$tp)),max(df$tp),length=9))
  
  miny=round(min(df$Resp2,na.rm=T),3)
  if(force2zero & all(df$tp>0)){
    miny=ifelse(grepl("\\.log",resp),min(df$Resp2,na.rm=T)-log(2),0)
    miny=ifelse(is.na(defzero),miny,round(defzero,3))
  }
 # if(any(is.na(df$Resp2)) & grepl('\\.log',resp)) df$Resp2[is.na(df$Resp2)]=round(min(df$Resp2,na.rm=T)-log(2),3)
  
  df=df[!is.na(df$Resp2),]
ylim=pretty(seq(0.95*miny,max(df$Resp2),length=8))
   #  print(ylims)
  idf=df[df$grp%in%lgrps & !is.na(df$Resp2) & df$Use,c("tp","grp","Id","Resp2","colorI","colorG")]
  if(force2zero & all(df$tp>0)){
    aidf=idf[tapply(1:nrow(idf),idf$Id,function(x) x[1]),]
    aidf$tp=0
    aidf$Resp2=miny
    idf=rbind(aidf,idf)
    idf=idf[order(idf$grp,idf$Id,idf$tp),]
    idf$Resp2[which(idf$Resp2<=miny)]=miny
  }
  idf$grp=factor(idf$grp)
  names(idf)[which(names(idf)=="Resp2")]="y"
  names(idf)[which(names(idf)=="tp")]="x"
  
  #  print(str(idf))
  #   idf$color=rep(brewer.pal(9,"Set1"),10)[-6][as.numeric(factor(idf$Id))]
  #   if(length(unique(idf$grp))>1)
  #     idf$color=getCols(idf$grp,as.character(idf$Id))[as.character(idf$Id)]
  #   
  if(type=='All'){
    a <- rCharts::Highcharts$new()
    for(i in unique(idf$Id))
      a$series(data = lapply(which(idf$Id==i),function(j) as.list(idf[j,])), name=i,type = "line",
               color=idf$colorI[which(idf$Id==i)][1])
    a$tooltip( formatter = "#! function() { return this.point.Id + ' (' + this.point.grp + ') at ' + this.point.x + ': ' + this.point.y ; } !#")
    a$yAxis(title = list(text = "Response"), min = min(ylim), max = max(ylim), tickInterval = diff(ylim)[1])
    a$xAxis(title = list(text = "Time"), min =  min(xlim), max = max(xlim), tickInterval = diff(xlim)[1])
    a$legend(verticalAlign = "right", align = "right", layout = "vertical", title = list(text = "Mice"))
    return(a)
  }
  
  # lcols=getCols(unique(idf$grp),NULL)
  #  print(lcols)
  a <- rCharts::Highcharts$new()
  for(i in unique(idf$grp)){
    l=which(idf$grp==i)
    
    tmp=t(do.call("cbind",tapply(idf$y[l],idf$x[l],function(x) c(n=length(x),y=mean(x),se=sd(x)/sqrt(length(x))))))
    tmp=data.frame(cbind(tmp,x=tapply(idf$x[l],idf$x[l],unique)),grp=i)
    tmp$y=round(tmp$y,2)
    tmp$se=round(tmp$se,2)
    tmp$color=unname(idf$colorG[l][1])
    a$series(data = lapply(1:nrow(tmp),function(j) as.list(tmp[j,])), name=i,type = "line",color=tmp$color[1],lineWidth=4)
    tmp$y=tmp$y+ifelse(se,1,1.96)*tmp$se
    a$series(data = lapply(1:nrow(tmp),function(j) as.list(tmp[j,])), name=i,type = "line",color=tmp$color[1],dashStyle= 'dot',marker=list(enabled=F))
    tmp$y=tmp$y-2*ifelse(se,1,1.96)*tmp$se
    a$series(data = lapply(1:nrow(tmp),function(j) as.list(tmp[j,])), name=i,type = "line",color=tmp$color[1],dashStyle= 'dot',marker=list(enabled=F))
  }
  a$tooltip( formatter = "#! function() { return this.point.grp + ' (' + this.point.n + ') at ' + this.point.x + ': ' + this.point.y ; } !#")
  a$yAxis(title = list(text = "Response"), min = min(ylim), max = max(ylim), tickInterval = diff(ylim)[1])
  a$xAxis(title = list(text = "Time"), min =  min(xlim), max = max(xlim), tickInterval = diff(xlim)[1])
  a$legend(verticalAlign = "right", align = "right", layout = "vertical", title = list(text = "Group"))
  
  return(a)
  
  
}

###############################################################################################################33
# 
# ################################################################################################
# plotTC2<-function(mod){
#   
#   df=mod$data
#   ylim=range(df$Resp)
#   ylim=ylim+c(-1,1)*diff(ylim)/20
#   xlim=c(floor(min(df$tp)),ceiling(max(df$tp)*1.2))
#   
#   lgrps=levels(df$grp)
#   newdf=expand.grid(tp=min(df$tp):max(df$tp),grp=lgrps)
#   newdf$pr=predict(mod$model,newdata=newdf)
#   
#   p1=rPlot(Resp ~ tp | grp, data = df, type = 'point', color = 'grp',cex=.6)
#   return(p1)
# }
# 
# 
# plotTC<-function(mod){
#   
#   df=mod$data
#   ylim=range(df$Resp)
#   ylim=ylim+c(-1,1)*diff(ylim)/20
#   xlim=c(floor(min(df$tp)),ceiling(max(df$tp)*1.2))
#   
#   lgrps=levels(df$grp)
#   newdf=expand.grid(tp=min(df$tp):max(df$tp),grp=lgrps)
#   newdf$pr=predict(mod$model,newdata=newdf)
#   
#   
#   def.par <- par(no.readonly = TRUE) # save default, for resetting...
#   
#   par(mar=c(4,4.5,2,1),cex.main=1.2,cex.lab=0.9,cex.axis=0.9,mfrow=c(ceiling(length(lgrps)/2),2))
#   for(iix in 1:length(lgrps)){
#     plot(0,0,cex=0,xlab="Time",ylab="Response",xlim=xlim,ylim=ylim,axes=F,main=lgrps[iix])
#     lines(tapply(newdf$tp,newdf$tp,mean),tapply(newdf$pr,newdf$tp,mean),lwd=2,col=1)
#     idf=df[which(df$grp==lgrps[iix]),]
#     idf$Id=factor(idf$Id)
#     icol=ifelse(is.null(idf$col),brewer.pal(8,"Set1")[iix],idf$col[1])
#     re=tapply(1:nrow(idf),factor(idf$Id),function(x) lines(idf$tp[x],idf$Resp[x],pch=16,cex=.5,col=icol))
#     rep=tapply(1:nrow(idf),factor(idf$Id),function(x) points(idf$tp[x],idf$Resp[x],pch=16,cex=.8,col=icol))
#     axis(1)
#     axis(2,las=2)
#   }
#   par(def.par)  #- reset to default
#   
# }
