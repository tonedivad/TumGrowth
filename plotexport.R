trans<-function(inam){
  doc=scan(inam,what="raw",sep="\n")
  for(k in grep(">Gst</text",doc)) doc[[k]]="<g>"
  for(k in grep(">Gend</text",doc)) doc[[k]]="</g>"

# lline=grep("<line x1",doc)
# lline=lline[!lline%in%c(grep("dasharray",doc),grep("linecap",doc))]
# for(i in lline) 
#       doc[[i]]=paste(gsub("/>","",doc[[i]]),' style=\"stroke-linecap:round\"/>',sep="")
write(doc,file=inam,sep="\n")
}

exportTC<-function(df,resp,lgrps,force2zero=FALSE,dogrps=TRUE,type='All',se=TRUE,defzero=NA){
  
  df=df[df$Use,]
  df$Resp=round(df[,resp],3)
  xlim=pretty(seq(ifelse(force2zero,0,min(df$tp)),max(df$tp),length=9))
  miny=round(min(df$Resp,na.rm=T),3)
  if(force2zero & all(df$tp>0)){
    miny=ifelse(grepl("\\.log",resp),min(df$Resp,na.rm=T)-log(2),0)
    miny=ifelse(is.na(defzero),miny,round(defzero,3))
  }
#  if(any(is.na(df$Resp)) & grepl('\\.log',resp)) df$Resp[is.na(df$Resp)]=round(min(df$Resp,na.rm=T)-log(2),3)
  
  ylim=pretty(seq(0.95*miny,max(df$Resp),length=8))
  #  print(ylims)
  idf=df[df$grp%in%lgrps & !is.na(df$Resp) & df$Use,c("tp","grp","Id","Resp","colorI","colorG","colorL")]
  if(force2zero & all(df$tp>0)){
    aidf=idf[tapply(1:nrow(idf),idf$Id,function(x) x[1]),]
    aidf$tp=0
    aidf$Resp=miny
    idf=rbind(aidf,idf)
    idf=idf[order(idf$grp,idf$Id,idf$tp),]
    idf$Resp[which(idf$Resp<=miny)]=miny
  }
  idf$grp=factor(idf$grp)
#   names(idf)[which(names(idf)=="Resp2")]="y"
#   names(idf)[which(names(idf)=="tp")]="x"
#   
xtk=xlim
xlim=range(xlim)
ytk=ylim
ylim=range(ylim)

  par(xpd=F)
  re=plot(range(xtk),range(ytk),pch=16,cex=0,axes=F,ylim=range(ytk),xlim=range(xtk),xlab="Time (d)",ylab=resp)
  
  if(dogrps)  text(mean(xtk)*.99,mean(ytk[-1])*.98,"Gst")
  axis(1,at=xtk,pos=ytk[1]-range(ytk)*.02,lwd=2,tick=F)
  axis(1,at=xtk,pos=ytk[1]-range(ytk)*.02,lwd=2,labels = rep("",length(xtk)))
if(dogrps)    text(mean(xtk)*.99,mean(ytk[-1])*.98,"Gend")
  if(dogrps)    text(mean(xtk)*.9,mean(ytk[-1])*.8,"Gst")
  axis(2,at=ytk,pos=xtk[1],las=2,lwd=2,tick=F)
  axis(2,at=ytk,pos=xtk[1],lwd=2,labels = rep("",length(ytk)))
if(dogrps)    text(mean(xtk)*.92,mean(ytk[-1])*.92,"Gend")
  
  if(type=='All'){
  for(igrp in levels(idf$grp)){
    if(dogrps)  text(mean(xtk)*.95,mean(ytk[-1])*.78,"Gst")
    cols=idf$colorG[idf$grp==igrp][1]
    for(imid in unique(idf$Id[idf$grp==igrp])){
      x=idf$tp[idf$Id==imid]
      y=idf$Resp[idf$Id==imid]
      id=idf$Id[idf$Id==imid]
      lines(x,y,col=cols,lwd=2)
    }
    if(dogrps)  text(mean(xtk)*.942,mean(ytk[-1])*.912,"Gend")
  }
}
if(type!='All'){
  for(i in levels(idf$grp)){
  l=which(idf$grp==i)

tmp=t(do.call("cbind",tapply(idf$Resp[l],idf$tp[l],function(x) c(n=length(x),Resp=mean(x),sd=sd(x),se=sd(x)/sqrt(length(x))))))
tmp=data.frame(cbind(tmp,tp=tapply(idf$tp[l],idf$tp[l],unique)),grp=i)
tmp$Resp=round(tmp$Resp,2)
tmp$se=round(tmp$se,2)
tmp$color=unname(idf$colorG[l][1])
lines(tmp$tp,tmp$Resp,col=tmp$col)
lines(tmp$tp,tmp$Resp-ifelse(se,1,0)*tmp$se-ifelse(se,0,1)*tmp$sd,col=tmp$col,lty=3)
lines(tmp$tp,tmp$Resp+ifelse(se,1,0)*tmp$se+ifelse(se,0,1)*tmp$sd,col=tmp$col,lty=3)
}
}
  legend(mean(xtk[1:2]),max(ytk),levels(idf$grp),ncol=nlevels(idf$grp),col=tapply(idf$colorG,idf$Id,unique),bty="n",lwd=par()$lwd)
  par(xpd=T)


}

##################################################################################################
exportCS<-function(cset,dogrps=TRUE,cexpt=1.2){
  
  idf=cset$df
  ylim=range(idf$Resp,na.rm=T)
  ylim=pretty(seq(ylim[1],ylim[2],length.out = 8))
  if(all(idf$Resp>=0,na.rm=T)) ylim[1]=max(0,ylim[1])
  if(length(unique(cset$tp))==2) toadd=paste(" between ",cset$tp[1],"-",cset$tp[2],sep="")
  if(length(unique(cset$tp))==1) toadd=paste(" at ",cset$tp[1],sep="")
  #mtext(paste("Tumor size",toadd),2,padj = -2.8)
  xlim=c(0.4,nlevels(idf$grp)+.6)
  re=boxplot(idf$Resp~idf$grp,pch=16,cex=0,axes=F,ylim=range(ylim),lwd=par('lwd'),lty=1,ylab=paste("Tumor size",toadd),
             border=tapply(idf$colorG,idf$grp,unique)[levels(idf$grp)],medlwd=par('lwd'),
             xlim=xlim)
  segments(xlim[1],min(ylim),xlim[2],min(ylim))
  re2=beeswarm(idf$Resp~idf$grp,pch=16,at=1:nlevels(idf$grp),pwcex = rep(cexpt,nrow(idf)),
               pwcol=idf$colorG,spacing=cexpt*1.01,
               corral = 'wrap',corralWidth = .5,add = T,do.plot=F)
  for(i in levels(idf$grp)){
    print(i)
    ire=re2[!is.na(re2[,1]) & re2$x.ori==i,]
    if(dogrps)     text(mean(ire[,1]),mean(ire[,2]),"Gst")
    points(ire[,1],ire[,2],cex=cexpt,pch=ire$pch,col=ire$col)
    if(dogrps)    text(mean(ire[,1]),mean(ire[,2]),"Gend")
  }
  if(dogrps)   text(nlevels(idf$grp)*.52,mean(ylim[-1])*.8,"Gst")
  for(i in 1:nlevels(idf$grp)) axis(1,at=i,labels = paste(re$names,"\n(",re$n,")",sep="")[i],
                                    tick=FALSE,pos=ylim[1])
  if(dogrps)   text(nlevels(idf$grp)*.52,mean(ylim[-1])*.92,"Gend")

  if(dogrps)   text(nlevels(idf$grp)*.49,mean(ylim[-1])*.8,"Gst")
  axis(2,at=ylim,pos=xlim[1],las=2,lwd=2,tick=F)
  axis(2,at=ylim,pos=xlim[1],lwd=2,labels = rep("",length(ylim)))
  if(dogrps)   text(nlevels(idf$grp)*.42,mean(ylim[-1])*.92,"Gend")
   
}

##################################################################################################
exportKM<-function(kmeir,dinc=0.1,lwd=2,cexpt=2,dogrps=TRUE){
  
  ndf=kmeir$df
  akms=kmeir$akms
  ndf$Inc=as.numeric(ndf$grp)-(1+nlevels(ndf$grp))/2
  ndf$Time2=ndf$Time+ndf$Inc*dinc
  cols=tapply(ndf$color,ndf$grp,unique)
  ticks= unique(sort(ndf$Time))
  xaxt=pretty(seq(0,max( ndf$Time2),length.out = 8))
  plot(survfit(Surv(Time2,Event)~grp,ndf),col=cols,lwd=lwd,lty=1,axes=F,cex=0,mark.time=FALSE,
       ylab="Proportion surviving (%)",xlab="Time in days",ylim=c(0,1),xlim=c(0,max(xaxt)*1.1))
  if(!is.na(cexpt) & cexpt>0)
    for(i in names(akms)){
      tmp=akms[[i]];tmp=tmp[tmp[,'x0']>0 & !is.na(tmp[,'y']),]
      if(dogrps)   text(mean(tmp[,'x'])*.48,mean(tmp[,'y']/100)*.8,"Gst")
      points(tmp[,'x'],tmp[,'y']/100,col=tmp[,'color'],pch=16,cex=cexpt)
      if(dogrps)   text(mean(tmp[,'x'])*.48,mean(tmp[,'y']/100)*.8,"Gend")
    }
  if(dogrps)   text(mean(xaxt)*.48,.8,"Gst")
  axis(1,at=ticks,lab=rep("",length(ticks)),pos=0,col.ticks="grey",lwd.ticks=par('lwd')*1.5,lwd=0)
  if(dogrps)   text(mean(xaxt)*.52,.7,"Gend")
  if(dogrps)   text(mean(xaxt)*.48,.48,"Gst")
  axis(1,at=xaxt,pos=0)
  if(dogrps)   text(mean(xaxt)*.52,.47,"Gend")
  
  if(dogrps)   text(mean(xaxt)*.45,.8,"Gst")
  axis(2,at=seq(0,1,.2),lab=seq(0,1,.2)*100,las=2,pos=0)
  if(dogrps)   text(mean(xaxt)*.4,.8,"Gend")
  if(dogrps)   text(mean(xaxt)*.42,.58,"Gst")
  legend('topright',paste(levels(ndf$grp)," (",table(ndf$grp[ndf$Event]),"/",table(ndf$grp),")",sep=""),xjust=0,
         ncol=1,bty="n",col=cols,lwd =lwd*1.5,seg.len =1)
  if(dogrps)   text(mean(xaxt)*.42,.38,"Gend")
  
}
