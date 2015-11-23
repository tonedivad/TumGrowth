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
loadFile<-function(ifile){
  tmp=strsplit(gsub("\"","",scan(ifile,sep="\n",what="raw")),"\t")
  
  whichtps=grep("^[0-9]+$",tmp[[1]])
  whichgrp=which(tmp[[1]]=="Grp")
  whichid=grep("^[MIDmid]+$",tmp[[1]])
  if(length(whichid)>1) whichid=whichid[1]
  whichuse=grep("^[USEuse]{3}$",tmp[[1]])
  if(length(whichuse)>1) whichuse=whichuse[1]
  
  ids=sapply(tmp,function(x) gsub(" ","",x[whichid]))
  grps=sapply(tmp,function(x) gsub(" ","",x[whichgrp]))
  if(length(whichid)==0){
    ids=paste(substr(grps,1,3),".M",sep="")
    for(k in unique(ids)) ids[ids==k]=paste(ids[ids==k],1:sum(ids==k),sep="")
  }
  use=rep(TRUE,length(grps))
  if(length(whichuse)==1) use=sapply(tmp,function(x) x[whichuse])!=""
  lmices=which(ids!="" & grps!="")
  lmices=lmices[lmices>2]
  
  uids=gsub(" ","",ids[lmices])
  while(any(table(uids)>1)){
    iredu=names(which(table(uids)>1))[1]
    uids[uids==iredu]=paste(uids[uids==iredu],1:sum(uids==iredu),sep=".")
  }
  use=use[lmices]
  grps=grps[lmices]
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
  
  
  allmeas=lapply(unique(lmeas),function(imeas){
    l=which(lmeas==imeas)
    df=data.frame(X=as.vector(mat[,l]),Id=rep(uids,length(l)),
                  tp=rep(ltps[l],each=nrow(mat)),stringsAsFactors = F)
    rownames(df)=paste(df$tp,df$Id,sep=";;")
    names(df)[1]=imeas
    df
  })
  umeas=unique(unlist(lapply(allmeas,rownames)))
  
  df=data.frame(sapply(allmeas,function(x) x[match(umeas,rownames(x)),1]),stringsAsFactors = F)
  names(df)=unique(lmeas)
  rownames(df)=umeas
  df=df[rowSums(is.na(df[,unique(lmeas),drop=F]))<length(unique(lmeas)),,drop=F]
  df=df[which(!apply(is.na(df),1,all)),,drop=F]
  for(i in unique(lmeas)){
    v=log(df[,i]);v[is.infinite(v)]=NA
    if(sum(is.na(v) | is.infinite(v))<(nrow(df)*.3))  df[,paste(i,"log",sep=".")]=v
  }
  lResp=names(df)
  
  df$tp=as.numeric(gsub(";;.*","",rownames(df)))
  df$Id=gsub("^[0-9]+;;","",rownames(df))
  df$Use=use[df$Id]
  df$grp=factor(grps[df$Id],levels = unique(grps))
  rownames(df)=1:nrow(df)
  
  df=df[order(df$grp,!df$Use,df$Id,df$tp),]
  
  df$colorI=getCols(df$grp,as.character(df$Id))[as.character(df$Id)]
  df$colorG=getCols(df$grp)[as.character(df$grp)]
  rownames(df)=paste(df$Id,df$tp,sep='.')
  
  list(data=df,Grp=levels(df$grp),Resp=lResp)
}
########################################################################################################################
########################################################################################################################

