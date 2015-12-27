downloadFile<-function(df){
  
  dat=df$data
  ltps=unique(dat$tp)
  lmids=unlist(tapply(dat$Id,dat$grp,unique))
  luse=unlist(tapply(dat$Use,dat$grp,unique))
  lmids2=paste(rep(lmids,each=length(ltps)),ltps,sep=";;")
  dat$Ids2=paste(dat$Id,dat$tp,sep=";;")
  dat=dat[match(lmids2,dat$Ids2),]
  dat$Ids2=lmids2
  dat$Id=rep(lmids,each=length(ltps))
  dat$tp=rep(ltps,length(lmids))
  
  l=c(df$Resp[-grep("\\.log$",df$Resp)],df$Resp[grep("\\.log$",df$Resp)])
  m=do.call("cbind",lapply(l,function(x){
    tt=do.call("cbind",tapply(dat[,x],dat$tp,round,4))
    colnames(tt)=paste(x,colnames(tt),sep=";;")
    tt
  }))
  m=m[,colSums(is.na(m))<nrow(m)]
  Mid=dat$Id[dat$tp==dat$tp[1]]
  Use=c('','x')[dat$Use[dat$tp==dat$tp[1]]+1]
  Grp=tapply(as.character(dat$grp),dat$Id,function(x) na.omit(unique(x)))[Mid]
  tow=cbind(Use,Grp,Mid,m)
  add=do.call("cbind",strsplit(colnames(m),";;"))
  tow=rbind(c("Use","Grp","Id",add[2,]),
            c("","","",add[1,]),tow)
  
  exptxt=list()
  for(i in 1:nrow(tow)) exptxt=c(exptxt,list(unname(tow[i,])))
  # print(exptxt)
  dimnames(tow)=list(NULL,NULL)
  return(exptxt)
}

#############################################################################################
loadFile<-function(ifile,ndigit=4,imputezer=TRUE,setday0=FALSE,trim=TRUE,trimzer=TRUE,exclzer=FALSE,sumids=FALSE){

  tmp=strsplit(gsub("\"","",scan(ifile,sep="\n",what="raw")),"\t")
  
  whichtps=grep("^[0-9]+$",tmp[[1]])
  whichgrp=grep("^[group]+$",tolower(tmp[[1]]))[1]
  whichid=grep("^[MIDmid]+$",tmp[[1]])
  if(length(whichid)>1) whichid=whichid[1]
  whichuse=grep("^[USEuse]{3}$",tmp[[1]])
  if(length(whichuse)>1) whichuse=whichuse[1]
  
  grps=sapply(tmp,function(x) gsub(" ","",x[whichgrp]))
  if(length(whichid)==0){
    ids=paste(substr(grps,1,3),".M",sep="")
    for(k in unique(ids)) ids[ids==k]=paste(ids[ids==k],1:sum(ids==k),sep="")
  } else ids=sapply(tmp,function(x) gsub(" ","",x[whichid]))

  use=rep(TRUE,length(grps))
  if(length(whichuse)==1) use=sapply(tmp,function(x) x[whichuse])!=""
  lmices=which(ids!="" & grps!="")
  lmices=lmices[lmices>2]
  
  uids=uidsold=gsub(" ","",ids[lmices])
  #if(!sumids & any(table(uids)>1)){
    while(any(table(uids)>1)){
      iredu=names(which(table(uids)>1))[1]
      uids[uids==iredu]=paste(uids[uids==iredu],1:sum(uids==iredu),sep=".")
    }
  #}
  use=use[lmices];grps=grps[lmices]
  names(grps)=names(use)=uids
  
  
  mat=t(sapply(lmices,function(x) as.numeric(gsub(",",".",tmp[[x]][whichtps]))))
  
  lmeas=tmp[[2]][whichtps]=gsub(" ","",tmp[[2]][whichtps])
  umeas=unique(lmeas)
  ## here chk .log
  if(length(grep("\\.log$",umeas))>0){
    llog=grep("\\.log$",umeas)
    llog=llog[sapply(llog,function(x) gsub("\\.log$","",umeas[x])%in%umeas)]
    if(length(llog)>0) umeas=umeas[-llog]
    whichtps=whichtps[lmeas%in%umeas]
  }
  lmeas=tmp[[2]][whichtps]
  ltps=as.numeric(tmp[[1]][whichtps])
  
  
  allmeas=list()
  for(imeas in unique(lmeas)){
    l=which(lmeas==imeas)
    df=data.frame(X=round(as.vector(mat[,l]),ndigit),Id=rep(uids,length(l)),tp=rep(ltps[l],each=nrow(mat)),
                  Grp=rep(grps,length(l)),Idold=rep(uidsold,length(l)),stringsAsFactors = F)
    df$IdTp=paste(df$tp,df$Id,sep=";;")
    df$IdoTp=paste(df$tp,df$Idold,sep=";;")
    if(any(table(df$IdTp)>1)){
      ldups=names(which(table(df$IdTp)>1))
      cat("Duplicated time points:",ldups,"\n")
      newx=tapply(df$X,df$IdTp,median,na.rm=T)
      df=df[match(names(newx),df$IdTp),]
      df$X=newx[df$IdTp]
    }
    rownames(df)=df$IdTp
    df=df[order(df$Grp,df$Id,df$tp),]
    
    ############
    ## Exclude trailing zeros/NAs
    if(trimzer){
      l2rm=NULL
      for(ipid in unique(df$Id)){
        idf=df[df$Id==ipid,]
        idf=idf[order(-idf$tp),]
        while((is.na(idf[1,1]) | idf[1,1]==0) & nrow(idf)>0){
          l2rm=c(l2rm,rownames(idf)[1])
          idf=idf[-1,]
        }
      }
      if(length(l2rm)>0){
        #cat("Excl in",imeas,":",l2rm,"\n",sep=" ")
        df[l2rm,1]=NA
      }
    }
    
    ############
    ## Sum at same Id/Tp
    if(sumids & max(rowSums(table(df$Idold,df$Grp)>0))==1){
      df=data.frame(X=tapply(df$X,df$IdoTp,function(x) ifelse(all(is.na(x)),NA,sum(x,na.rm=T))),
                    Id=tapply(df$Idold,df$IdoTp,unique),tp=tapply(df$tp,df$IdoTp,unique),Grp=tapply(df$Grp,df$IdoTp,unique),
                    stringsAsFactors = F)
    df=df[order(df$Grp,df$Id,df$tp),]
    names(grps)=names(use)=uidsold
    }
    
    df=df[,1:4]
    
    ############
    ## Remove same values from the end
    if(trim){
    l2rm=NULL
    for(ipid in unique(df$Id)){
      idf=df[df$Id==ipid,]
      idf=idf[order(-idf$tp),]
      if(all(is.na(idf[,1]))) next
      while(is.na(idf[1,1]) & nrow(idf)>0) idf=idf[-1,]
      
      if(!exclzer) l2excl=which(diff(idf[,1])==0 & idf[-nrow(idf),1]>0)
      if(exclzer) l2excl=which(diff(idf[,1])==0)
      if(length(l2excl)>0) l2excl=l2excl[l2excl==(1:length(l2excl))]
 #     if(exclzer & any(idf[,1]==0,na.rm = TRUE)) l2excl=c(l2excl,which(idf[,1]<=0)) 
      if(length(l2excl)>0) l2rm=c(l2rm,rownames(idf)[l2excl])
    }
    if(length(l2rm)>0){
      cat("Excl in",imeas,":",l2rm,"\n",sep=" ")
      df[l2rm,1]=NA
    }
  }
    
    names(df)[1]=imeas
    allmeas[[imeas]]=df
  }
  

  umeas=unique(unlist(lapply(allmeas,rownames)))
  
  df=data.frame(sapply(allmeas,function(x) x[match(umeas,rownames(x)),1]),stringsAsFactors = F)
  names(df)=unique(lmeas)
  rownames(df)=umeas
  df=df[rowSums(is.na(df[,unique(lmeas),drop=F]))<length(unique(lmeas)),,drop=F]
  df=df[which(!apply(is.na(df),1,all)),,drop=F]
  df$tp=as.numeric(gsub(";;.*","",rownames(df)))
  df$Id=gsub("^[0-9]+;;","",rownames(df))
  df$Use=use[df$Id]
  df$grp=factor(grps[df$Id],levels = unique(grps))
  rownames(df)=1:nrow(df)
  df=df[order(df$grp,!df$Use,df$Id,df$tp),]
  
  lResp=unique(lmeas)
  if(exclzer) for(i in lResp) df[which(df[,i]==0),i]=NA
  df=df[!rowSums(is.na(df[,lResp,drop=FALSE]))==length(lResp),]
  rownames(df)=1:nrow(df)
  
  
 if(setday0){
   ftp=tapply(1:nrow(df),df$Id,function(x) sapply(lResp,function(y) min(df$tp[x[min(which(df[x,y]>0))]],na.rm=T)))
  for(i in names(ftp)[!is.infinite(ftp)]){
    if(any(df$Id==i & df$tp<ftp[i])) for(k in lResp) df[which(df$Id==i & df$tp<ftp[i]),k]=NA
    df$tp[df$Id==i]=df$tp[df$Id==i]-ftp[i]+1
  }
  df=df[which(df$tp>=0),]
 }
  
  for(i in lResp){
    v=round(log(df[,i]),ndigit)
    if(!imputezer) v[is.infinite(v)]=NA
    if(imputezer) v[is.infinite(v)]=log(min(df[df[,i]>0,i],na.rm=T)/2)
    if(sum(is.na(v) | is.infinite(v))<(nrow(df)*.2)){
      df[,paste(i,"log",sep=".")]=v
      lResp=c(lResp,paste(i,"log",sep="."))
    }
  }
  df=df[,c(which(names(df)%in%lResp),which(!names(df)%in%lResp))]
  
  
  df$colorI=getCols(df$grp,as.character(df$Id))[as.character(df$Id)]
  df$colorG=getCols(df$grp)[as.character(df$grp)]
  df$colorL=getCols(df$grp,what=1)[as.character(df$grp)]
  rownames(df)=paste(df$Id,df$tp,sep='.')
  
  l2excl=unique(df$Id[!df$Use])
  print(l2excl)
  
  list(data=df,Grp=levels(df$grp),Resp=lResp,Excl=l2excl)
}
########################################################################################################################
########################################################################################################################

