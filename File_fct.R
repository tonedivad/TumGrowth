##############################################################################################
## Perform several options to clean-up the orginal datafile
verifOne<-function(df,dfm,trim=TRUE,trimzer=TRUE,exclzer=FALSE){
  
  df$IdTp=paste(df$tp,df$Id,sep=";;")
  df$IdoTp=paste(df$tp,df$Idold,sep=";;")
  
  ####################################
  ## combine repeated meas at the same Tp
  if(any(table(df$IdTp)>1)){
    ldups=names(which(table(df$IdTp)>1))
    #cat("Duplicated time points:",ldups,"\n")
    newx=tapply(df$X,df$IdTp,median,na.rm=T)
    df=df[match(names(newx),df$IdTp),]
    df$X=newx[df$IdTp]
  }
  rownames(df)=df$IdTp
  df=df[order(df$Id,df$tp),]
  
  ####################################
  ## Exclude trailing zeros/NAs
  if(trimzer){
    l2rm=NULL
    ## only use animals that do not  survive at the end: dfm$Surv=FALSE
    lmids2chk=unique(df$Id)
    lmids2chk=lmids2chk[lmids2chk%in%dfm$Id[!dfm$Surv]]
    for(ipid in lmids2chk){
      idf=df[df$Id==ipid,]
      idf=idf[order(-idf$tp),]
      while((is.na(idf[1,1]) | idf[1,1]==0) & nrow(idf)>0){
        l2rm=c(l2rm,rownames(idf)[1])
        idf=idf[-1,]
      }
    }
    if(length(l2rm)>0) df[l2rm,1]=NA
  }
  
  ####################################
  ## Sum at same Id/Tp need to be revisited for future versions
  #  if(sumids & max(rowSums(table(df$IdoTp,df$Grp)>0))==1){
  #     df=data.frame(X=tapply(df$X,df$IdoTp,function(x) ifelse(all(is.na(x)),NA,sum(x,na.rm=T))),
  #                   Id=tapply(df$Idold,df$IdoTp,unique),
  #                   tp=tapply(df$tp,df$IdoTp,unique),Grp=tapply(df$Grp,df$IdoTp,unique),
  #                   stringsAsFactors = F)
  #     df=df[order(df$Grp,df$Id,df$tp),]
  #     names(grps)=names(use)=uidsold
  #   }
  
  ####################################
  ## Remove same values from the end
  if(trim){
    l2rm=NULL
    ## only use animals that do not  survive at the end: dfm$Surv=FALSE
    lmids2chk=unique(df$Id)
    lmids2chk=lmids2chk[lmids2chk%in%dfm$Id[!dfm$Surv]]
    for(ipid in lmids2chk){
      idf=df[df$Id==ipid,]
      idf=idf[order(-idf$tp),]
      if(all(is.na(idf$X))) next
      while(is.na(idf$X[1]) & nrow(idf)>0) idf=idf[-1,]
      if(!exclzer) l2excl=which(diff(idf$X)==0 & idf[-nrow(idf),1]>0)
      if(exclzer) l2excl=which(diff(idf$X)==0)
      if(length(l2excl)>0) l2excl=l2excl[l2excl==(1:length(l2excl))]
      if(length(l2excl)>0) l2rm=c(l2rm,rownames(idf)[l2excl])
    }
    if(length(l2rm)>0) df[l2rm,]$X=NA
  }
  return(df)

}

#############################################################################################
## Parse the orginal datafile
loadFile<-function(ifile,ndigit=4,trimzer=TRUE,trim=TRUE,setday0=NA,exclzer=FALSE,imputezer=TRUE,set2surv=FALSE){

  tmp=suppressMessages(strsplit(gsub("\"","",scan(ifile,sep="\n",what="raw",quiet=TRUE)),"\t"))
  
  if(!is.numeric(ndigit) | is.na(ndigit)) ndigit=4
  #ndigit=max(1,ndigit)
  
  whichtps=grep("^[0-9]+$",tmp[[1]])
 # if(length(whichgrp)==0) stop('No numerical time labels found on line 1')
  whichgrp=grep("^[group]+$",tolower(tmp[[1]]))[1]
#  if(length(whichgrp)==0) stop('No group label found on line 1')
  whichid=grep("^[MIDmid]+$",tmp[[1]])
  if(length(whichid)>1) whichid=whichid[1]
  whichsurv=grep("^[survSurv]+$",tmp[[1]])
  if(length(whichsurv)>1) whichsurv=whichsurv[1]
  whichuse=grep("^[USEuse]{3}$",tmp[[1]])
  if(length(whichuse)>1) whichuse=whichuse[1]
  
  grps=sapply(tmp,function(x) gsub(" ","",x[whichgrp]))
  if(length(whichid)==0){
    mgrps=gsub("[^a-zA-Z]","",grps)
    l1=which(nchar(mgrps)>6)
    if(length(l1)>0)
      mgrps[l1]=paste(substr(mgrps[l1],1,3),substr(mgrps[l1],nchar(mgrps[l1])-1,nchar(mgrps[l1])),sep=".")
    ids=paste(mgrps,".M",sep="")
    for(k in unique(ids)) ids[ids==k]=paste(ids[ids==k],1:sum(ids==k),sep="")
  } else ids=sapply(tmp,function(x) gsub(" ","",x[whichid]))

  isuse=rep(TRUE,length(grps))
  if(length(whichuse)==1) isuse=sapply(tmp,function(x) x[whichuse])!=""
  
  issurv=rep(FALSE,length(grps))
  if(length(whichsurv)==1){
    issurv[sapply(tmp,function(x) x[whichsurv])!=""]=TRUE
  }
  
  lmices=which(ids!="" & grps!="")
  lmices=lmices[lmices>2]
  
  uids=uidsold=gsub(" ","",ids[lmices])
  while(any(table(uids)>1)){
    iredu=names(which(table(uids)>1))[1]
    uids[uids==iredu]=paste(uids[uids==iredu],1:sum(uids==iredu),sep=".")
  }
  
  isuse=isuse[lmices]
  issurv=issurv[lmices]
  grps=grps[lmices]
  luniqgrps=unique(grps)
  names(grps)=names(issurv)=names(isuse)=uids
  ####################################################################################
  ## mice data frame
  ## double check if discondordant infos???
  dfm=data.frame(Id=tapply(uids,uids,unique),Grp=tapply(grps,uids,unique),
                 Use=tapply(isuse,uids,function(x) unique(x)[1]),
                 Surv=tapply(issurv,uids,function(x) unique(x)[1]),
                 stringsAsFactors=FALSE)
  ####################################################################################
  ## get the meas data
  if(length(whichtps)>1)  mat=t(sapply(lmices,function(x) as.numeric(gsub(",",".",tmp[[x]][whichtps]))))
  if(length(whichtps)==1)  mat=matrix(sapply(lmices,function(x) as.numeric(gsub(",",".",tmp[[x]][whichtps]))),ncol=1)
  
  lmeas=tmp[[2]][whichtps]=gsub(" ","",tmp[[2]][whichtps])
  umeas=unique(lmeas)
  
  ## is there any .log/.sqrt/.curt from previous parsing??
  llog=c(grep("\\.log$",umeas),grep("\\.sqrt$",umeas),grep("\\.curt$",umeas))
  if(length(llog)>0){
    umeas=umeas[-llog]
    whichtps=whichtps[lmeas%in%umeas]
  }
  lmeas=tmp[[2]][whichtps]
  ltps=as.numeric(tmp[[1]][whichtps])
  
  ####################################################################################
  imeas=unique(lmeas)[1]
  allmeas=list()
  for(imeas in unique(lmeas)){
    l=which(lmeas==imeas)
    df=data.frame(X=round(as.vector(mat[,l]),ndigit),Id=rep(uids,length(l)),tp=rep(ltps[l],each=nrow(mat)),
                  Grp=rep(grps,length(l)),Idold=rep(uidsold,length(l)),stringsAsFactors = F)
    df2=verifOne(df,dfm,trim,trimzer,exclzer)
    names(df2)[1]=imeas
    allmeas[[imeas]]=df2
   }

  #####################
  # Combine them all
  umeas=unique(unlist(lapply(allmeas,rownames)))
  
  df=data.frame(sapply(allmeas,function(x) x[match(umeas,rownames(x)),1]),stringsAsFactors = F)
  names(df)=unique(lmeas)
  rownames(df)=umeas
  df=df[rowSums(is.na(df[,unique(lmeas),drop=F]))<length(unique(lmeas)),,drop=F]
  df=df[which(!apply(is.na(df),1,all)),,drop=F]
  df$Tp=as.numeric(gsub(";;.*","",rownames(df)))
  df$Id=gsub("^[0-9]+;;","",rownames(df))
  
  lResp=unique(lmeas)
  if(exclzer) for(i in lResp) df[which(df[,i]==0),i]=NA
  df=df[!rowSums(is.na(df[,lResp,drop=FALSE]))==length(lResp),]
  
  dfm=dfm[dfm$Id%in%df$Id,]
  dfm$Grp=factor(dfm$Grp,levels=luniqgrps[dfm$Grp%in%luniqgrps])
  dfm=dfm[order(dfm$Grp,dfm$Id),]
  
  df=df[order(factor(df$Id,levels=dfm$Id),df$Tp),]
  rownames(df)=1:nrow(df)
  
  
  if(!is.na(setday0)) if(setday0>=0) {
    ftp=tapply(1:nrow(df),df$Id,function(x) min(sapply(lResp,function(y) min(df$Tp[x[min(which(df[x,y]>0))]],na.rm=T))))
    for(i in names(ftp)[!is.infinite(ftp)]){
      if(any(df$Id==i & df$Tp<ftp[i])) for(k in lResp) df[which(df$Id==i & df$Tp<ftp[i]),k]=NA
      df$Tp[df$Id==i]=df$Tp[df$Id==i]-ftp[i]+setday0
    }
    df=df[which(df$Tp>=0),]
  }
  
  lResp2=NULL
  for(i in lResp){
    v=round(log(df[,i]),max(3,ndigit-1))
    if(!imputezer) v[is.infinite(v)]=NA
    if(imputezer) v[is.infinite(v)]=round(log(min(df[df[,i]>0,i],na.rm=T)/2),max(3,ndigit-1))
    if(sum(is.na(v) | is.infinite(v))<(nrow(df)*.15)){
      df[,paste(i,"log",sep=".")]=v
      lResp2=c(lResp2,paste(i,"log",sep="."))
    }
    v=round(sqrt(df[,i]),ndigit)
    if(sum(is.na(v) | is.infinite(v))<(nrow(df)*.15)){
      df[,paste(i,"sqrt",sep=".")]=v
      lResp2=c(lResp2,paste(i,"sqrt",sep="."))
      df[,paste(i,"curt",sep=".")]=v^(2/3)
      lResp2=c(lResp2,paste(i,"curt",sep="."))
    }
    
  }
  df=df[,c(which(names(df)%in%lResp),which(!names(df)%in%lResp))]
  rownames(df)=paste(df$Id,df$Tp,sep='.')
  
  ## set Surv to TRUE for animal measured at the last experiment day
  if(length(whichsurv)==0 & set2surv){
    lnas=names(which(tapply(df$Tp,df$Id,max)==max(df$Tp)))
  if(length(lnas)>0) dfm[lnas,]$Surv=TRUE
  }
  

  list(data=df,dataM=dfm,Resp=lResp,RespTr=lResp2,Excl=unique(dfm$Id[!dfm$Use]))
}
########################################################################################################################
########################################################################################################################

########################################################################################################################
## Export the parsed datafile
downloadFile<-function(cdat,ndigit=4,trans=T){
  
  dat=cdat$data
  datm=cdat$dataM
  ltps=unique(dat$Tp)
  lmids=unlist(tapply(datm$Id,datm$Grp,unique))
  luse=unlist(tapply(datm$Use,datm$Grp,unique))
  lmids2=paste(rep(lmids,each=length(ltps)),ltps,sep=";;")
  dat$Ids2=paste(dat$Id,dat$Tp,sep=";;")
  dat=dat[match(lmids2,dat$Ids2),]
  dat$Ids2=lmids2
  dat$Id=rep(lmids,each=length(ltps))
  dat$Tp=rep(ltps,length(lmids))
  
  l=cdat$Resp
  if(trans) l=c(l,cdat$RespTr)
  
  m=do.call("cbind",lapply(l,function(x){
    tt=do.call("cbind",tapply(dat[,x],dat$Tp,round,max(3,ndigit-1)))
    colnames(tt)=paste(x,colnames(tt),sep=";;")
    tt
  }))
  m=m[,colSums(is.na(m))<nrow(m)]
  Mid=dat$Id[dat$Tp==dat$Tp[1]]
  Use=c('','x')[datm[Mid,]$Use+1]
  Surv=c('','x')[datm[Mid,]$Surv+1]
  Grp=as.character(datm[Mid,]$Grp)
  tow=cbind(Use,Grp,Mid,Surv,m)
  add=do.call("cbind",strsplit(colnames(m),";;"))
  tow=rbind(c("Use","Grp","Id","Surv",add[2,]),
            c("","","","",add[1,]),tow)
  
  exptxt=list()
  for(i in 1:nrow(tow)) exptxt=c(exptxt,list(unname(tow[i,])))
  # print(exptxt)
  dimnames(tow)=list(NULL,NULL)
  return(exptxt)
}

