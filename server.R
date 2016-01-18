require(rCharts)
require(shiny)
library(shinyFiles)

source("helpers.R")
source("files.R")
source("long.R")
source("km.R")
source("cs.R")
source("plotexport.R")


options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {
  
  mypalette<-function(x) c("#61B270", "#397FB9","#D05043" ,"#8475B5", "#878787", "#EB7B36", "#A5691B" ,"#1B7E76",
                           '#C994C7','#E151A3','#D61967','#980043','#FC9272','#F75A40','#D72322','#A50F15',
                           '#9EBCDA','#8C87BF','#894FA3','#810F7C','#9ECAE1','#5DA4D0','#2C7CBA','#08519C',
                           '#A1D99B','#63BB6D','#2D954D','#006D2C','#BDBDBD','#8A8A8A','#5D5D5D','#252525',
                           '#FEC44F','#F88B22','#D65808','#993404','#C79141','#A5691B','#DFC27D','#7E4808','#543005')
  
#   paste(unlist(lapply(c("PuRd" ,"Reds","BuPu","Blues","Greens","Greys","YlOrBr","OrRd"),function(ix) colorRampPalette(brewer.pal(9,ix)[4:8])(4))),         collapse="','")
  
  ######## format the side bar
  lcols <- reactiveValues(data = NULL)
  fileData <- reactive({
  #  cat(input$sampleData)
    if(input$dataInput==1) inFile<-list(Ori=input$sampleData,Where=paste("./",input$sampleData,sep=""))
    if(input$dataInput==2){
      if (is.null(input$browse)) return(NULL)
      inFile <- list(Ori=input$browse$name,Where=input$browse$datapath)
    }
    inFile$Ori=gsub(".*/","",gsub("\\.[A-Za-z]+$","",inFile$Ori))
    if(nchar(inFile$Ori)<2) inFile$Ori=paste("TG_",Sys.Date(),format(Sys.time(), "%H:%M"),sep="")
    return(inFile)
  })
  
  dat <- reactive({
    ifile=fileData()
    if (is.null(ifile$Where)) return(NULL)
      cat("Loading",ifile$Where,"\n")
      shiny:::flushReact()
      lcols$data<-NULL
      setday0=suppressWarnings(as.numeric(input$setday0))
      res<-try(loadFile(ifile$Where,ndigit=as.numeric(input$ndigits),imputezer=input$imputezer,trim=input$trim, setday0=setday0,
               trimzer=input$trimzer,exclzer=input$exclzer,sumids=input$sumids),TRUE)
      if(class(res)%in%"try-error")
        res=paste("Oups, something went wrong with the format of",ifile$Ori)
      res
  })
  
  mresp <- reactive({cdat=dat();if(!is.list(cdat)) return(NULL);cdat$Resp})
  mgrps <- reactive({cdat=dat();if(!is.list(cdat)) return(NULL);levels(cdat$dataM$Grp)})
  l2Excl <- reactive({cdat=dat();if(!is.list(cdat)) return(NULL);cdat$Excl})
  refkm <- eventReactive(input$varskm,input$varskm)
  reflg <- eventReactive(input$varslg,input$varslg)
  refcs <- eventReactive(input$varscs,input$varscs)
  observeEvent(input$updateCols,{
    num2<-mgrps()
    newcols=sapply(1:length(num2),function(x) eval(parse(text=paste("input$colCustom",x,sep=""))))
    names(newcols)=num2
    lcols$data<-newcols
  })
  
  colGrps<-reactive({
    if(!is.null(lcols$data)) return(lcols$data)
    num2<-mgrps()
    newcols=mypalette()[1:length(num2)]
    names(newcols)=num2
    newcols
  })
  ##################################
  output$choicesdat<-renderUI({
    nums2=mresp()
    if(is.null(nums2)) return(NULL)
    radioButtons("responsedat",NULL, choices=nums2,selected=nums2[1],inline = T)
  })
  
  
  output$groupscols<-renderUI({
    nums2<-mgrps()
    if(is.null(nums2)) return(NULL)
    listcols0=mypalette()
    if(!is.null(lcols$data)){
      listcols1=lcols$data
      if(length(listcols1)==length(nums2)) if(all(names(listcols1)==nums2)) listcols0=listcols1
    }
    grpcols=list()
    # cat("chk",nums2,"\n",listcols0)
    for(i in 1:length(nums2))
      grpcols[[i]]=colourInput(paste("colCustom",i,sep=""), nums2[i], palette = "limited",value=listcols0[i],
                               allowedCols=unique(mypalette()))
    grpcols
  })
  
  output$groupsdat<-renderUI({
    nums2<-mgrps()
    if(is.null(nums2)) return(NULL)
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("groupsdat",NULL, choices=list,selected=list,inline = T)
  })
  
  
  output$groupsdat2<-renderUI({
    nums2<-mgrps()
    if(is.null(nums2)) return(NULL)
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("groupsdat2",NULL, choices=list,selected=list,inline = T)
  })
  
  output$choices<-renderUI({
    nums2=mresp()
    if(is.null(nums2)) return(NULL)
    radioButtons("response", 'Response', choices=nums2,selected=nums2[1],inline = T)
  })
  
  output$boxes<-renderUI({
    nums2<-mgrps()
    if(is.null(nums2)) return(NULL)
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("vars",'Treatment groups', choices=list,selected=list,inline = T)
  })
  
  ##################################
  
  output$choiceslg<-renderUI({
    nums2=mresp()
    if(is.null(nums2)) return(NULL)
    radioButtons("responselg", "Response", choices=nums2,selected=nums2[1],inline = TRUE)
  })
  
  output$boxeslg<-renderUI({
    nums2<-mgrps()
    if(is.null(nums2)) return(NULL)
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("varslg", "Treatment groups", choices=list,selected=list,inline = TRUE)
  })
  
  output$radiolong<-renderUI({
    nums2=c('All',reflg())
    if(is.null(nums2)) return(NULL)
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("radiolong", label =NULL,inline = T,choices =unname(nums2), selected = 'All')
  })
  
  ##################################
  output$choiceskm<-renderUI({
    nums2=mresp()
    if(is.null(nums2)) return(NULL)
    nums2=nums2[-grep("\\.log",nums2)]
    radioButtons("responsekm", 'Response', choices=nums2,selected=nums2[1],inline = TRUE)
  })
  
  output$boxeskm<-renderUI({
    nums2<-mgrps()
    if(is.null(nums2)) return(NULL)
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("varskm", 'Treatment groups', choices=list,selected=list,inline = TRUE)
  })
  
  output$radiokm<-renderUI({
    nums2=c('None',refkm())
    if(is.null(nums2)) return(NULL)
    radioButtons("radiokm", NULL, choices=nums2,selected=nums2[1],inline=TRUE)
  })
  
  
  output$slidekmui<-renderUI({
    if(is.null(input$responsekm)) return(NULL)
    cdat=dat()
    if(!input$responsekm%in%cdat$Resp | !is.list(cdat)) return(NULL)
    resp=gsub("\\.log","",input$responsekm)
    if(!resp%in%cdat$Resp) return(NULL)
    vraw=na.omit(cdat$data[,resp])
    valr=pretty(c(quantile(vraw,.05),max(vraw)),100)
    sliderInput(inputId="slidekm",label=NULL,
                min=min(valr),max=max(valr),value=max(valr),step =diff(valr)[1])
  })
  
  output$slidekmui2<-renderUI({
    if(is.null(input$responsekm)) return(NULL)
    cdat=dat()
    if(!input$responsekm%in%cdat$Resp | !is.list(cdat)) return(NULL)
    resp=gsub("\\.log","",input$responsekm)
    if(!resp%in%cdat$Resp) return(NULL)
    vraw=na.omit(cdat$data[,resp])
    valr=pretty(c(quantile(vraw,.95),0),100)
    sliderInput(inputId="slidekm2",label=NULL,
                min=min(valr),max=max(valr),value=min(valr),step =diff(valr)[1])
  })
  
  output$sliderkmtui<-renderUI({
    if(is.null(input$responsekm)) return(NULL)
    cdat=dat()
    if(!input$responsekm%in%cdat$Resp | !is.list(cdat)) return(NULL)
    valt=sort(unique(cdat$data$Tp))
    args       <- list(inputId="sliderkmt", label=NULL, ticks=valt, value=length(valt)-1)
    args$min   <- 1
    args$step   <- 1
    args$max   <- length(args$ticks)
    ticks <- paste(args$ticks, collapse=',')
    args$ticks <- TRUE
    htmlslider1  <- do.call('sliderInput', args)
    htmlslider1$children[[2]]$attribs[['data-values']] <- ticks
    htmlslider1
  })
  
  ##################################
  output$slidecsui<-renderUI({
    # if(is.null(input$responsecs)) return(NULL)
    cdat=dat()
    if(!is.list(cdat)) return(NULL)
    df=cdat$data
    df=df[df$Id%in%cdat$dataM$Id[cdat$dataM$Use],]
    
    valt=sort(unique(df$Tp))
    if(length(valt)==1){
      valt=c(valt,valt+1)
      args       <- list(inputId="slidercs", label=NULL, ticks=valt, value=0)
      args$min   <- 1
      args$step   <- 1
      args$max   <- 1
      ticks <- paste(args$ticks, collapse=',')
      args$ticks <- TRUE
      htmlslider  <- do.call('sliderInput', args)
      htmlslider$children[[2]]$attribs[['data-values']] <- ticks
      return(htmlslider)
      
    }
    
    
    valt0=range(tapply(df$Tp,df$Id,max,na.rm=T))
    if(length(valt)==2) valt0=valt
    if(valt0[1]==valt0[2]) valt0[1]=max(valt[valt<valt0[1]])
    args       <- list(inputId="slidercs", label=NULL, ticks=valt, value=match(valt0,valt)-1)
    args$min   <- 1
    args$max   <- length(args$ticks)
    ticks <- paste0(args$ticks, collapse=',')
    args$ticks <- T
    htmlslider  <- do.call('sliderInput', args)
    htmlslider$children[[2]]$attribs[['data-values']] <- ticks;
    htmlslider
  })
  
  output$choicescs<-renderUI({
    nums2=mresp()
    if(is.null(nums2)) return(NULL)
    radioButtons("responsecs", 'Response', choices=nums2,selected=nums2[1],inline = TRUE)
  })
  
  output$boxescs<-renderUI({
    nums2<-mgrps()
    if(is.null(nums2)) return(NULL)
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("varscs", 'Treatment groups', choices=list,selected=list,inline = TRUE)
  })
  
  output$grpcsvar<-renderUI({
    nums2=c('All',refcs())
    if(is.null(nums2)) return(NULL)
    #  radioButtons("grpcsvar", label =NULL,inline = T,choices =unname(nums2), selected = 'All')
    list<-list()
    for(i in nums2) list[[i]]<-i
    checkboxGroupInput("grpcsvar", label =NULL,inline = T,choices =unname(nums2), selected = 'All')
  })

  #######################################################################################################################
  modelLG<-eventReactive(input$goButton,{
    
    if(is.null(input$responselg) | is.null(input$varslg)) return(NULL)
    if(any(!input$responselg%in%mresp()) | any(!input$varslg%in%mgrps() )) return(NULL)
    
    cdat<-dat()
    df=cdat$data
    df$Resp=df[,input$responselg] 
    df=df[,which(!names(df)%in%mresp())]
    lmids=cdat$dataM$Id[cdat$dataM$Use & cdat$dataM$Grp%in%input$varslg]
    lgdf=df[df$Id%in%lmids & !is.na(df$Resp),]
    lgdf$Grp=factor(cdat$dataM[lgdf$Id,]$Grp)
    
    bfco=input$bfco
    if(bfco=='None') bfco=0 else bfco=as.numeric(gsub('p<','',bfco))
    res=compModLG(lgdf,bfco=bfco,checkvar = input$radiolongvar)
    res$Resp=input$responselg
    res
  })
  
  modelLGpw<-reactive({
    if(is.null(input$responselg) | is.null(input$varslg)| is.null(input$radiolong)) return(NULL)
    if(any(!input$responselg%in%mresp()) | any(!input$varslg%in%mgrps() )) return(NULL)
    objres=modelLG()
    ref=input$radiolong
    ref=ref[ref%in%levels(objres$data$Grp)]
    
    pwt=objres$pairTab[,1:4]
    if(length(ref)>0)  pwt=pwt[which(pwt[,1]%in%ref | pwt[,2]%in%ref),]
    pwt$PvalueAdj=.myf(p.adjust(pwt$Pvalue,"holm"))
    pwt$Pvalue=.myf(pwt$Pvalue)
    pwt$ChiSq=round(pwt$ChiSq,2)
    if(length(ref)==1){
      pwt[pwt[,1]==ref,1]=pwt[pwt[,1]==ref,2]
      pwt=pwt[,-2]
      names(pwt)[1]=ref
    }
    pwt
  })
  
  #######################################################################################################################
  ndfKM<-reactive({
    if(is.null(input$responsekm)) return(NULL)
    if(is.null(input$slidekm2) & is.null(input$slidekm)) return(NULL)
    if(any(!input$responsekm%in%mresp()) | any(!input$varskm%in%mgrps() )) return(NULL)
    
    cdat<-dat()
    ltps=sort(unique(cdat$data$Tp))
    resp=gsub("\\.log$","",input$responsekm)
    if(input$survTyp==1) 
      return(getOSTab(cdat,resp,input$varskm,lastT=ltps[input$sliderkmt+1],lastM=input$slidekm))
    if(input$survTyp==2) 
      return(getTFSTab(cdat,resp,input$varskm,firstM=input$slidekm2))
    
    
  })
  
  modelKM<-reactive({
    if(is.null(input$responsekm) |is.null(input$slidekm) |is.null(input$radiokm)) return(NULL)
    if(input$radiokm=='None') return(NULL)
    objres=ndfKM()
    if(is.null(objres)) return(NULL)
  #  cat("Ref",input$radiokm,input$radiokmFirth,"\n")
    compKM(objres,input$radiokm,input$radiokmFirth)
  })
  ############################################################################################################
  csectDF<-reactive({
    if(any(!input$responsecs%in%mresp()) | any(!input$varscs%in%mgrps() )) return(NULL)
    cdat<-dat()
    df=cdat$data
    dfm=cdat$dataM
    rangetp=sort(unique(df$Tp))
    getCSmat(df,dfm,resp=input$responsecs,lgrps=input$varscs,
             rangetp=rangetp[input$slidercs+1],usemax=input$radiocsMax)
  })
  
  modelCS<-reactive({
    objres<-csectDF()
    if(is.null(objres)) return(NULL)
    bfco=input$bfcocs
    if(bfco=='None') bfco=0 else bfco=as.numeric(gsub('p<','',bfco))
    res=compCS(objres,bfco=bfco,checkvar=input$radiocsvar)
    res
  })
  
  modelCSpw<-reactive({
    if(any(!input$responsecs%in%mresp()) | any(!input$varscs%in%mgrps()) | is.null(input$grpcsvar)) return(NULL)
    objres=modelCS()
    if(is.null(objres)) return(NULL)
    ref=input$grpcsvar
    ref=ref[ref%in%levels(objres$data$Grp)]
    if(length(ref)==0) ref=levels(objres$data$Grp)
    pwt=objres$pairTab
 #   print(pwt)
    if(length(ref)>0)  pwt=pwt[which(pwt[,1]%in%ref | pwt[,2]%in%ref),]
    if(length(ref)==1){
      if(any(pwt[,1]%in%ref)){
        for(i in which(pwt[,1]%in%ref)){
          pwt[i,1:2]=pwt[i,2:1]
          pwt[i,4:5]=-pwt[i,5:4]
          pwt[i,3]=-pwt[i,3]
        }
      }
    }
    top=data.frame(pwt[,1:2],Diff=apply(as.matrix(pwt[,3:5]),1,function(x) sprintf("%.3f [%.3f;%.3f]",x[1],x[2],x[3])),
    Pvalue=.myf(pwt$Pval),PvalueAdj=.myf(p.adjust(pwt$Pval,"holm")),
    WilcoxPvalue=.myf(pwt$Wil),WilcoxPvalueAdj=.myf(p.adjust(pwt$Wil,"holm")),stringsAsFactors=F)
    if(length(ref)==1){
      top=top[,-2]
      colnames(top)[1]=paste("Comp to ",ref)}
    return(top)
  })
  
  
 ############################################################################
  
  output$design <- renderTable({
    cdat=dat()
    if(!is.list(cdat)) return(as.table(matrix(cdat,dimnames=list("Error"," "))))
    
    lmids=cdat$dataM$Id[cdat$dataM$Use]
    lgdf=cdat$data[cdat$data$Id%in%lmids,]
    lgdf$Grp=factor(cdat$dataM[lgdf$Id,]$Grp)
    tab=table(lgdf$Grp,lgdf$Tp)
    return(tab)
  })
  
  output$filetableshort <- DT::renderDataTable({
    if(is.null(input$responsedat) | is.null(input$groupsdat)) return(NULL)
    if(!any(input$responsedat%in%mresp()) | !any(input$groupsdat%in%mgrps())) return(NULL)
    cdat=dat()
    if(!is.list(cdat)) return(as.table(matrix(cdat,dimnames=list("Error"," "))))
    
    top=cbind(cdat$dataM[cdat$data$Id,c("Id","Grp","Use","Surv"),],cdat$data[,c("Tp",input$responsedat)])
    top=top[top$Grp%in%input$groupsdat,] 
    
    ltps=sort(unique(top$Tp))
    add=do.call('rbind',tapply(1:nrow(top),top[,"Id"],function(x)
      round(top[x[match(ltps,top[x,'Tp'])],input$responsedat],2)))
    colnames(add)=ltps
    tow=cbind(Use=tapply(as.character(top$Use),top$Id,unique),
              Surv=tapply(as.character(top$Surv),top$Id,unique),
              Grp=tapply(as.character(top$Grp),top$Id,unique),
              add)
    rownames(tow)=tapply(as.character(top$Id),top$Id,unique)
    tow=tow[order(factor(tow[,"Grp"],levels=levels(top$Grp))),]
    tow
  },options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE),server=TRUE,
  selection = list(mode = 'multiple', selected = l2Excl()))

  ###########################################################
  
  output$plottcs<-renderChart({
    if(any(!input$response%in%mresp()) | any(!input$vars%in%mgrps() )) return(NULL)

    cdat=dat()
    lmids=cdat$dataM$Id[cdat$dataM$Use]
    cdat$data=cdat$data[cdat$data$Id%in%lmids,]
    cdat$dataM=cdat$dataM[cdat$dataM$Id%in%lmids,]

    df=cbind(cdat$data[,c(input$response,"Tp")],cdat$dataM[cdat$data$Id,])
    names(df)[1]=input$response
    df$color=colGrps()[as.character(df$Grp)]
    
    h1=plotLineC(df,input$response,input$vars,input$forcezero,type=input$tcfplottyp,
                 se=input$tcse=='SE',defzero=as.numeric(input$tcdef),
                 miny=as.numeric(input$tcminy),maxy=as.numeric(input$tcmaxy))$plot
    h1$addParams(dom = 'plottcs')
    return(h1)
  })
  
  
  ###########################################################
  output$plotkm<-renderChart({
    
    ndf=ndfKM()
    if(is.null(ndf)) return(NULL)
    p1=plotKM(ndf, gcols=colGrps()[levels(ndf$Df$Grp)],shift=as.numeric(input$kmshift),
              lwd=as.numeric(input$kmlwd),
              title=ifelse(ndf$Typ=="OS","Perc. surviving","Perc. tumour-free"))
    p1=p1$plot
    p1$addParams(dom = 'plotkm')
    return(p1)
  })
  
  output$sumKM <- renderTable({
    ndf<-ndfKM()$Df
    v=paste(ndf$Id," (",ndf$Time,c("","+")[ndf$Event+1],")",sep="");names(v)=NULL
    lso=order(ndf$Time,ndf$Event)
    v=tapply(v[lso],ndf$Grp[lso],c)
    data.frame(cbind(Group=names(v),"Censoring"=unlist(sapply(v,paste,collapse=" "))))
  },align="lll",include.rownames = F,include.colnames = T)
  
  output$modKM<-renderTable({
    if(is.null(input$responsekm) |is.null(input$slidekm) |is.null(input$radiokm)) return(NULL)
    if(input$radiokm=='None') return(NULL)
    data.frame(modelKM()$modTab)},align="llll")
  
  output$hrKM<-DT::renderDataTable({
    if(is.null(input$responsekm) |is.null(input$slidekm) |is.null(input$radiokm)) return(NULL)
    if(input$radiokm=='None') return(NULL)
    modelKM()$hrTab},
    options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE), rownames = FALSE)
  
  
  ###########################################################
  output$plotdiaglg<-renderChart({
    if(is.null(input$responselg)) return(NULL)
    mod<-modelLG()
    p1=plotDiagLG(mod,colGrps()[levels(mod$data$Grp)],typplot=input$radiodiaglg)
    p1$addParams(dom = 'plotdiaglg')
    return(p1)
    
  })
  output$modelLGsum<-renderTable(modelLG()$coefTab,align="lcccc")
  output$modelLGeffect<-renderTable(data.frame(modelLG()$AnovaTab),align="lll")
  output$modelLGpw<-DT::renderDataTable({modelLGpw()}, 
                options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE),
                rownames = FALSE)
  output$modelLGwei<-renderPrint(modelLG()$weightCoef)
  output$modelLGcor<-renderPrint(modelLG()$corrCoef)
  output$outLG<-renderPrint({
    idat=modelLG()$data
    if(!any((idat$out))) return('No outliers')
    idat=idat[idat$out,]
    louts=tapply(1:nrow(idat),factor(idat$Id),function(x) 
      paste(idat$Id[x[1]]," (",idat$Grp[x[1]],"): ",paste(idat$Tp[x],collapse=","),sep=""))
    c("Outliers: ",unname(louts))
  })
  
  ############################################################################
  output$plotdiagcs<-renderChart({
    if(is.null(input$responsecs)) return(NULL)
    mod<-modelCS()
    p1=plotDiagCS(mod,colGrps()[levels(mod$data$Grp)],typplot=input$radiodiagcs)
    p1$addParams(dom = 'plotdiagcs')
    return(p1)
    
  })
  output$sumCS <- renderTable({
    objres=csectDF()
    if(is.null(objres)) return(NULL)
    v=objres$Df$Id
    if(grepl("range",objres$Par)) v=paste(objres$Df$Id," (",objres$Df$Tp,")",sep="")
    names(v)=NULL
    lso=order(-objres$Df$Tp)
    v=tapply(v[lso],objres$Df$Grp[lso],sort)
    data.frame(cbind(Group=names(v),Animals=unlist(sapply(v,paste,collapse=" "))))
  },align="lll",include.rownames = F,include.colnames = T)
  
  output$plotcs<-renderChart({
    if(is.null(input$responsecs)) return(NULL)
    objres=csectDF()
    if(is.null(objres)) return(NULL)
    p1=plotCS(objres,gcols=colGrps()[levels(objres$Df$Grp)],
              miny=as.numeric(input$csminy),maxy=as.numeric(input$csmaxy))$plot
    p1$addParams(dom = 'plotcs')
    return(p1)
  })
  
  output$ctCS<-DT::renderDataTable(modelCSpw(),
            options = list(paging = F,searching = FALSE,autoWidth = TRUE), rownames = FALSE)
  output$modCSeffect<-renderTable(data.frame(modelCS()$AnovaTab),align="llll")
  output$modCS<-renderTable({data.frame(modelCS()$coefTab)},align="lllll")
  output$modCSwei<-renderPrint(modelCS()$weightCoef)
  output$outCS<-renderPrint({
    idat=modelCS()$data
    if(!any((idat$out))) return('No outliers')
    idat=idat[idat$out,]
    louts=tapply(1:nrow(idat),factor(idat$Id),function(x) 
      paste(idat$Id[x[1]]," (",idat$Grp[x[1]],"): ",paste(idat$Tp[x],collapse=","),sep=""))
    c("Outliers: ",unname(louts))
  })
  
  ############################################################################
  
  output$downloadCS <- downloadHandler(
    filename <- function() paste(fileData()$Ori,'-CS.',tolower(input$csplot),sep=""),
    content <- function(file) {
      if(input$csplot=='Svg'){
        require(svglite)
        svglite(file, width =as.numeric(input$cswidth), height =as.numeric(input$csheight))
      }
      if(input$csplot=='Png')
        png(file, width =as.numeric(input$cswidth)*96, height =as.numeric(input$csheight)*96)
      
      objres=csectDF()
      if(!is.null(objres)){
      p2=plotCS(objres,gcols=colGrps()[levels(objres$Df$Grp)],
                miny=as.numeric(input$csminy),maxy=as.numeric(input$csmaxy),retplot=FALSE)
      par(mar=c(3.4,4,1,.1),lwd=2,cex=input$cscex,cex.axis=input$cscex,cex.lab=input$cscex)
      exportCS(p2,dogrps=input$csplot=='Svg',cexpt=as.numeric(input$cscexpt))
      }
      dev.off()
      if(input$csplot=='Svg') trans(file)
    },
    contentType = ifelse(input$csplot=='Svg','image/svg','image/png')
  )
  
  
  output$downloadKM <- downloadHandler(
    filename <- function() paste(fileData()$Ori,'-KM.',tolower(input$kmfplot),sep=""),
    content <- function(file) {
      if(input$kmfplot=='Svg'){
        require(svglite)
        svglite(file, width =as.numeric(input$kmwidth), height =as.numeric(input$kmheight))
      }
      if(input$kmfplot=='Png')
        png(file, width =as.numeric(input$kmwidth)*96, height =as.numeric(input$kmheight)*96)
      
      ndf=ndfKM()
      if(!is.null(ndf)){
        p1=plotKM(ndf, gcols=colGrps()[levels(ndf$Df$Grp)],shift=as.numeric(input$kmshift),
                  lwd=as.numeric(input$kmlwd),retplot=FALSE)
        par(mar=c(5.5,5,.5,.5),lwd=2,cex=input$kmcex,cex.axis=input$kmcex,cex.lab=input$kmcex)
        exportKM(p1,lwd=as.numeric(input$kmlwd),cexpt=as.numeric(input$kmcexpt),dogrps=input$csplot=='Svg')
      }
      dev.off()
      if(input$kmfplot=='Svg') trans(file)
    },
    contentType = ifelse(input$kmfplot=='Svg','image/svg','image/png')
  )
  
  output$downloadTC <- downloadHandler(
    filename <- function() paste(fileData()$Ori,'-TC.',tolower(input$tcfplot),sep=""),
    content <- function(file) {
      if(input$tcfplot=='Svg'){
        require(svglite)
        svglite(file, width =as.numeric(input$tcwidth), height =as.numeric(input$tcheight),standalone = TRUE)
      }
      if(input$tcfplot=='Png')
        png(file, width =as.numeric(input$tcwidth)*96, height =as.numeric(input$tcheight)*96)
      
      par(mar=c(3.4,4,1,.1),lwd=2,lend=0,cex=input$tccex,cex.axis=input$tccex,cex.lab=input$tccex)
      cdat=dat()
      resp=input$response
      lgrps=input$vars
      
      lmids=cdat$dataM$Id[cdat$dataM$Use]
      cdat$data=cdat$data[cdat$data$Id%in%lmids,]
      cdat$dataM=cdat$dataM[cdat$dataM$Id%in%lmids,]

      df=cbind(cdat$data[,c(resp,"Tp")],cdat$dataM[cdat$data$Id,])
      names(df)[1]=resp
      df$color=colGrps()[as.character(df$Grp)]
      h1=plotLineC(df,resp,lgrps,input$forcezero,defzero=as.numeric(input$tcdef),
                   miny=as.numeric(input$tcminy),maxy=as.numeric(input$tcmaxy))[-1]
      exportTC(h1,dogrps=(input$tcfplot=='Svg'),type=input$tcfplottyp,se=input$tcse=='SE')
      dev.off()
      if(input$tcfplot=='Svg') trans(file)
    },  contentType = ifelse(input$tcfplot=='Svg','image/svg','image/png')
  )
  
  ############################################################################
 output$exporttxtDS<-downloadHandler(
    filename = function() paste(fileData()$Ori,"-data.tsv",sep=""),
    content = function(file) {
      what=sapply(downloadFile(dat(),ndigit=as.numeric(input$ndigits)),paste,collapse='\t')
      cat(what, file = file, sep = "\n")
    }
  )
  
  ############################################################################
  output$exporttxtLG <- downloadHandler(
    filename = function() {
      paste(fileData()$Ori,"-LG.", sep = '', switch(
        input$formatLG, PDF = 'pdf', HTML = 'html', DOCX = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('reportLG.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reportLG.Rmd',overwrite=T)
      
      ifile=fileData()$Ori
      objres=modelLG()
      formatpw=modelLGpw()
      plotdata=plotDiagLG(objres,colGrps()[levels(objres$data$Grp)],typplot='None')
      
      out <- render('reportLG.Rmd', switch(
        input$formatLG,PDF = pdf_document(), HTML = html_document(), DOCX = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  #############
  output$exporttxtKM <- downloadHandler(
    filename = function() {
      paste(fileData()$Ori,"-KM.", sep = '', switch(
        input$formatKM, PDF = 'pdf', HTML = 'html', DOCX = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('reportKM.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reportKM.Rmd',overwrite=T)
      
      ifile=fileData()$Ori
      objres=ndfKM()
      modKM=modelKM()
      p1=plotKM(objres, gcols=colGrps()[levels(objres$Df$Grp)],shift=as.numeric(input$kmshift),
                lwd=as.numeric(input$kmlwd),retplot=FALSE)
      
      out <- render('reportKM.Rmd', switch(
        input$formatKM,PDF = pdf_document(), HTML = html_document(), DOCX = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  #############
  output$exporttxtCS <- downloadHandler(
    filename = function() {
      paste(fileData()$Ori,"-CS.", sep = '', switch(
        input$formatCS, PDF = 'pdf', HTML = 'html', DOCX = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('reportCS.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reportCS.Rmd',overwrite=T)
      
      ifile=fileData()$Ori
      objres=csectDF()
      mod=modelCS()
      formatpw=modelCSpw()
      plotdata=plotDiagCS(mod,colGrps()[levels(mod$data$Grp)],typplot='None')
      p2=plotCS(objres,gcols=colGrps()[levels(objres$Df$Grp)],
                miny=as.numeric(input$csminy),maxy=as.numeric(input$csmaxy),retplot=FALSE)
      
      out <- render('reportCS.Rmd', switch(
        input$formatCS,PDF = pdf_document(), HTML = html_document(), DOCX = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
})
