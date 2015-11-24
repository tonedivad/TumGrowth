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
  
  
  ######## format the side bar
  fileData <- reactive({
    cat(input$sampleData)
    if(input$dataInput==1) inFile<-paste("./",input$sampleData,sep="")
    if(input$dataInput==2){
      if (is.null(input$browse)) return(NULL)
    inFile <- input$browse$datapath
    }
    return(inFile)
  })
  
 dat <- reactive({
   ifile=fileData()
   cat("Lets load the data",ifile,"\n")
    shiny:::flushReact()
    loadFile(ifile,imputezer=input$imputezer,trim=input$trim,exclzer=input$exclzer)
  })
  
#  dat <- eventReactive(input$filetableshort_rows_selected,dat0){
#    df=dat0()
#    lsel=input$filetableshort_rows_selected
#    print(lsel)
#    if(!is.null(lsel)){
#    if(lsel %in%df$Excl) df$Excl=df$Excl[!df$Excl%in%lsel]
#    else df$Excl=c(df$Excl,lsel)
#    df$Use=!(df$Id%in%df$Excl)
#    }
#    print(df$Excl)
#    df
#  })
#    
 mresp <- reactive(dat()$Resp)
 mgrps <- reactive(dat()$Grp)
 l2Excl <- reactive(dat()$Excl)
 refkm <- eventReactive(input$varskm,input$varskm)
 reflg <- eventReactive(input$varslg,input$varslg)
 refcs <- eventReactive(input$varscs,input$varscs)
 
 ##################################
 output$choicesdat<-renderUI({
   nums2=mresp()
   radioButtons("responsedat",NULL, choices=nums2,selected=nums2[1],inline = T)
 })
 
 output$groupsdat<-renderUI({
   nums2<-mgrps()
   list<-list()
   for(i in nums2) list[[i]]<-i
   checkboxGroupInput("groupsdat",NULL, choices=list,selected=list,inline = T)
 })
 output$groupsdat2<-renderUI({
   nums2<-mgrps()
   list<-list()
   for(i in nums2) list[[i]]<-i
   checkboxGroupInput("groupsdat2",NULL, choices=list,selected=list,inline = T)
 })
 
 output$choices<-renderUI({
   nums2=mresp()
   radioButtons("response", 'Response', choices=nums2,selected=nums2[1],inline = T)
 })

 output$boxes<-renderUI({
   nums2<-mgrps()
   list<-list()
   for(i in nums2) list[[i]]<-i
   checkboxGroupInput("vars",'Treatment groups', choices=list,selected=list,inline = T)
 })
 
 ##################################
 
 output$choiceslg<-renderUI({
   nums2=mresp()
   radioButtons("responselg", "Response", choices=nums2,selected=nums2[1],inline = TRUE)
 })
 
 output$boxeslg<-renderUI({
   nums2<-mgrps()
   list<-list()
   for(i in nums2) list[[i]]<-i
   checkboxGroupInput("varslg", "Treatment groups", choices=list,selected=list,inline = TRUE)
 })
 
 output$radiolong<-renderUI({
   nums2=c('All',reflg())
   radioButtons("radiolong", label =NULL,inline = T,choices =unname(nums2), selected = 'All')
 })
 
 ##################################
 output$choiceskm<-renderUI({
   nums2=mresp()
   nums2=nums2[-grep("\\.log",nums2)]
   radioButtons("responsekm", 'Response', choices=nums2,selected=nums2[1],inline = TRUE)
 })
 
 output$boxeskm<-renderUI({
   nums2<-mgrps()
   list<-list()
   for(i in nums2) list[[i]]<-i
   checkboxGroupInput("varskm", 'Treatment groups', choices=list,selected=list,inline = TRUE)
 })
 
 output$radiokm<-renderUI({
   nums2=c('None',refkm())
   radioButtons("radiokm", NULL, choices=nums2,selected=nums2[1],inline=TRUE)
 })
 

 output$slidekmui<-renderUI({
   if(is.null(input$responsekm)) return(NULL)
   df=dat()$data
  # df$Use=!(df$Id%in%l2excl())
   df=df[df$Use,]
   
   if(!input$responsekm%in%names(df)) return(NULL)
   resp=gsub("\\..*","",input$responsekm)
   f=2
   valr=unname(c(floor(quantile(df[,resp],.5,na.rm=T)/f),ceiling(max(df[,resp],na.rm=T)/f))*f)
   valrax=pretty(seq(valr[1],valr[2],length=7))
   
   sliderInput(inputId="slidekm",label=NULL,
               min=min(valr),max=max(valr),value=max(valr),step =round(diff(valr)/100))
 })
 
 output$sliderkmtui<-renderUI({
   if(is.null(input$responsekm)) return(NULL)
   df=dat()$data
   df=df[df$Use,]
   
   if(!input$responsekm%in%names(df)) return(NULL)
   valt=sort(unique(df$tp))
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
   df=dat()$data
   df=df[df$Use,]
   
   valt=sort(unique(df$tp))
   valt0=range(tapply(df$tp,df$Id,max,na.rm=T))
   print(valt0)
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
    radioButtons("responsecs", 'Response', choices=nums2,selected=nums2[1],inline = TRUE)
 })
 
 output$boxescs<-renderUI({
   nums2<-mgrps()
   list<-list()
   for(i in nums2) list[[i]]<-i
   checkboxGroupInput("varscs", 'Treatment groups', choices=list,selected=list,inline = TRUE)
 })
 
 output$grpcsvar<-renderUI({
   nums2=c('All',refcs())
   radioButtons("grpcsvar", label =NULL,inline = T,choices =unname(nums2), selected = 'All')
 })
 
 ############################################################################
 
# model<-reactive({
model<-eventReactive(input$goButton,{
     if(is.null(input$responselg) | is.null(input$varslg)) return(NULL)
   if(any(!input$responselg%in%mresp()) | any(!input$varslg%in%mgrps() )) return(NULL)
   data<-dat()$data
   data=data[data$Use,]
  # print(input$filetableshort_rows_selected)
   cat("Comp model",input$varslg,'\n')
   bfco=input$bfco
   if(bfco=='None') bfco=0 else bfco=as.numeric(gsub('p<','',bfco))
   compMod(data,input$varslg,input$responselg,bfco=bfco,checkvar = input$radiolongvar)
 })
 
 modelpw<-reactive({
   if(is.null(input$responselg) | is.null(input$varslg)| is.null(input$radiolong)) return(NULL)
   if(any(!input$responselg%in%mresp()) | any(!input$varslg%in%mgrps() )) return(NULL)
   objres=model()
   ref=input$radiolong
   cat('Ref LG:',ref,'\n')
   compMod2(objres,ref)
 })
 
 ##################################
 kmeier<-reactive({
   if(is.null(input$responsekm) | is.null(input$slidekm)) return(NULL)
   print(input$varskm)
   if(any(!input$responsekm%in%mresp()) | any(!input$varskm%in%mgrps() )) return(NULL)
   
   df=dat()$data
   df=df[df$Use,]
   cat("Comp KM",input$varskm,'\n')
   ltps=sort(unique(df$tp))
   print(ltps)
   p1=plotKM(df,resp=input$responsekm,lgrps=input$varskm,lastM = input$slidekm,lastT =ltps[input$sliderkmt+1],shift0=as.numeric(input$kmshift))
   return(p1)
   
 })
 
 kmeier2<-reactive({
   if(is.null(input$responsekm) |is.null(input$slidekm) |is.null(input$radiokm)) return(NULL)
   if(any(!input$responsekm%in%mresp()) | any(!input$varskm%in%mgrps())) return(NULL)
   if(input$radiokm=='None') return(NULL)
   objres=kmeier()
   cat("Ref",input$radiokm,input$radiokmFirth,"\n")
   compKM(objres,input$radiokm,input$radiokmFirth)
 })
 ####################################
 csect<-reactive({
 #  if(is.null(input$response) | input$upload<1) return(NULL)
   if(any(!input$responsecs%in%mresp()) | any(!input$varscs%in%mgrps() )) return(NULL)
   
   df=dat()$data
   df=df[df$Use,]
   rangetp=sort(unique(df$tp))
   cat("Comp CS",rangetp[input$slidercs+1],'\n')
   p2=plotCS(df,input$responsecs,input$varscs,rangetp=rangetp[input$slidercs+1],usemax=input$radiocsMax)
   return(p2)
   
 })
 
 csect2<-reactive({
   if(is.null(input$grpcsvar)) return(NULL)
   if(any(!input$responsecs%in%mresp()) | any(!input$varscs%in%mgrps() )) return(NULL)
   objres<-csect()
   bfco=input$bfcocs
   if(bfco=='None') bfco=0 else bfco=as.numeric(gsub('p<','',bfco))
   p2=compCS(objres,bfco=bfco,checkvar=input$radiocsvar,ref=input$grpcsvar)
   return(p2)
 })
 ############################################################################

 output$design <- renderTable({
   datM=dat()$data
   datM=datM[datM$Use,]
   tab=table(datM$grp,datM$tp[datM$Use])
   return(tab)
 })
#  
#  output$filetableori <- renderDataTable({
#     top=loadFile(fileData(),imputezer = FALSE,trim = FALSE)$data
#     top=top[,-grep('\\.log',top[2,]),drop=FALSE]
#      print(top)
#      return(data.frame(top,stringsAsFactors = FALSE))
#  },options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE))

 output$filetableshort <- DT::renderDataTable({
   if(is.null(input$responsedat) | is.null(input$groupsdat)) return(NULL)
   if(!any(input$responsedat%in%mresp()) | !any(input$groupsdat%in%mgrps())) return(NULL)
   top=dat()$data
   l2excl=dat()$Excl
   print(l2excl)
  top=top[,names(top)%in%c("Id","Use","grp","tp",input$responsedat )]
  top=top[top$grp%in%input$groupsdat,] 
   ltps=sort(unique(top$tp))
   add=do.call('rbind',tapply(1:nrow(top),top[,"Id"],function(x)
     round(top[x[match(ltps,top[x,'tp'])],input$responsedat],2)))
   colnames(add)=ltps
   tow=cbind(Use=tapply(as.character(top$Use),top$Id,unique),
             #Id=tapply(as.character(top$Id),top$Id,unique),
             Grp=tapply(as.character(top$grp),top$Id,unique),
             add)
   rownames(tow)=tapply(as.character(top$Id),top$Id,unique)
   tow=tow[order(factor(tow[,"Grp"],levels=levels(top$grp))),]
   tow
 },options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE),server=TRUE,
 selection = list(mode = 'multiple', selected = l2Excl()))
 
  output$filetablelong <-  DT::renderDataTable({
    if(is.null(input$groupsdat2)) return(NULL)
    if(!any(input$groupsdat2%in%mgrps())) return(NULL)
    top=dat()$data
    top=top[top$grp%in%input$groupsdat2,] 
    
     top=top[,!names(top)%in%c('colorI','colorG')]
     top=cbind(top[,c("Use","Id","grp","tp")],top[,!names(top)%in%c("Id","Use","grp","tp")])
     top=top[order(top$grp,top$tp,top$Id),]
     rownames(top)=NULL
       return(top)
  },options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE), rownames = FALSE)

  ###########################################################
output$plottc3a<-renderChart({
  if(any(!input$response%in%mresp()) | any(!input$vars%in%mgrps() )) return(NULL)
  df<-dat()$data
  df=df[df$Use,]
  resp=input$response
  lgrps=input$vars
  h1=plotTC3(df,resp,lgrps,input$forcezero,type='All',se=input$tcse=='SE',defzero=as.numeric(input$tcdef))
  h1$addParams(dom = 'plottc3a')
  return(h1)
  #  }
})
output$plottc3b<-renderChart({
  if(any(!input$response%in%mresp()) | any(!input$vars%in%mgrps() )) return(NULL)
  df<-dat()$data
  df=df[df$Use,]
  resp=input$response
  lgrps=input$vars
  h1=plotTC3(df,resp,lgrps,input$forcezero,type='Mean',se=input$tcse=='SE',defzero=as.numeric(input$tcdef))
    h1$addParams(dom = 'plottc3b')
  return(h1)
  #  }
})


  ###########################################################
  output$plotkm<-renderChart({
   # if(is.null(input$response)) return(NULL)
    p1=kmeier()$plot
    p1$addParams(dom = 'plotkm')
    return(p1)
  })
  
  output$sumKM <- renderTable(data.frame(kmeier()$sumids),align="lll",include.rownames = F,include.colnames = T)
  output$modKM<-renderTable({
    if(is.null(input$responsekm) |is.null(input$slidekm) |is.null(input$radiokm)) return(NULL)
    if(input$radiokm=='None') return(NULL)
    data.frame(kmeier2()$mod)},
    align="ll",include.rownames = F,include.colnames = F)
  output$hrKM<-DT::renderDataTable({
    if(is.null(input$responsekm) |is.null(input$slidekm) |is.null(input$radiokm)) return(NULL)
    if(input$radiokm=='None') return(NULL)
    kmeier2()$hr},
    options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE), rownames = FALSE)
  

  ###########################################################
  output$plottc4<-renderChart({
    if(is.null(input$responselg)) return(NULL)
    mod<-model()
    p1=plotDiag(mod,typplot=input$radiodiag)
    p1$addParams(dom = 'plottc4')
    return(p1)
    
  })
  output$modelsum<-renderTable(model()$coef,align="lcccc")
  output$modeleffect<-renderTable(data.frame(model()$antab),align="ll")
  output$modelpw<-DT::renderDataTable({modelpw()$pw}, options = list(paging = FALSE,searching = FALSE,autoWidth = TRUE), rownames = FALSE)
  output$modelwei<-renderPrint(model()$weightCoef)
  output$modelcor<-renderPrint(model()$corrCoef)
  output$modelout<-renderPrint({
    idat=model()$data
    if(!any((idat$out))) return('No outliers')
    idat=idat[idat$out,]
    c("Outliers:\n",unname(tapply(1:nrow(idat),factor(idat$Id),function(x) paste(idat$Id[x[1]]," (",idat$grp[x[1]],"): ",paste(idat$tp[x],collapse=","),sep=""))))
    })
  
  ############################################################################

output$sumCS <- renderTable(data.frame(csect()$sumids),align="lll",include.rownames = F,include.colnames = T)
output$ctCS<-DT::renderDataTable(data.frame(csect2()$ct), options = list(paging = F,searching = FALSE,autoWidth = TRUE), rownames = FALSE)
output$plotcs<-renderChart({
  if(is.null(input$responsecs)) return(NULL)
  p1=csect()$plot
  p1$addParams(dom = 'plotcs')
  return(p1)
})
output$modCS<-renderTable({
  if(is.null(input$responsecs) |is.null(input$grpcsvar)) return(NULL)
  data.frame(csect2()$mod)},
  align="ll",include.rownames = F,include.colnames = F)

output$csout<-renderPrint({
  idat=csect2()$df
  if(!any((idat$out))) return('No outliers')
  idat=idat[idat$out,]
  c("Outliers:\n",
    unname(tapply(1:nrow(idat),factor(idat$Id),function(x) paste(idat$Id[x[1]]," (",idat$grp[x[1]],"): ",paste(idat$tp[x],collapse=","),sep=""))))
})

output$downloadCS <- downloadHandler(
  filename <- function() paste(rev(strsplit(fileData(),"[\\/]")[[1]])[1],'-CS.',tolower(input$csplot),sep=""),
  content <- function(file) {
    if(input$csplot=='Svg'){
      require(svglite)
      svglite(file, width =as.numeric(input$cswidth), height =as.numeric(input$csheight))
    }
    if(input$csplot=='Png')
      png(file, width =as.numeric(input$cswidth)*96, height =as.numeric(input$csheight)*96)
    par(mar=c(3.4,4,1,.1),lwd=2,cex=input$cscex,cex.axis=input$cscex,cex.lab=input$cscex)
    exportCS(csect(),dogrps=input$csplot=='Svg',cexpt=as.numeric(input$cscexpt))
    dev.off()
    if(input$csplot=='Svg') trans(file)
  },
  contentType = ifelse(input$csplot=='Svg','image/svg','image/png')
)


output$downloadKM <- downloadHandler(
  filename <- function() paste(rev(strsplit(fileData(),"[\\/]")[[1]])[1],'-KM.',tolower(input$kmfplot),sep=""),
  content <- function(file) {
    if(input$kmfplot=='Svg'){
      require(svglite)
      svglite(file, width =as.numeric(input$kmwidth), height =as.numeric(input$kmheight))
    }
    if(input$kmfplot=='Png')
      png(file, width =as.numeric(input$kmwidth)*96, height =as.numeric(input$kmheight)*96)
    par(mar=c(5.5,5,.5,.5),lwd=2,cex=input$kmcex,cex.axis=input$kmcex,cex.lab=input$kmcex)
    exportKM(kmeier(),dinc= as.numeric(input$kmshift),lwd=as.numeric(input$kmlwd),cexpt=as.numeric(input$kmcexpt))
    dev.off()
    if(input$kmfplot=='Svg') trans(file)
  },
  contentType = ifelse(input$kmfplot=='Svg','image/svg','image/png')
)

output$downloadTC <- downloadHandler(
  filename <- function() paste(rev(strsplit(fileData(),"[\\/]")[[1]])[1],'-TC.',tolower(input$tcfplot),sep=""),
  content <- function(file) {
    if(input$tcfplot=='Svg'){
      require(svglite)
      svglite(file, width =as.numeric(input$tcwidth), height =as.numeric(input$tcheight),standalone = TRUE)
    }
    if(input$tcfplot=='Png')
      png(file, width =as.numeric(input$tcwidth)*96, height =as.numeric(input$tcheight)*96)
    
    par(mar=c(3.4,4,1,.1),lwd=2,lend=0,cex=input$tccex,cex.axis=input$tccex,cex.lab=input$tccex)
    exportTC(dat()$data,resp = input$response,lgrps = input$vars,
             force2zero=input$forcezero,dogrps=(input$tcfplot=='Svg'),
             type=input$tcfplottyp,se=input$tcse=='SE',defzero=as.numeric(input$tcdef))
    dev.off()
    if(input$tcfplot=='Svg') trans(file)
  },  contentType = ifelse(input$tcfplot=='Svg','image/svg','image/png')
)
############################################################################

output$exporttxtLG<-downloadHandler(
  filename = function() paste(rev(strsplit(fileData(),"[\\/]")[[1]])[1],"-LG.txt",sep=""),
  content = function(file) {
    what=sapply(modelpw()$exptxt,paste,collapse='\t')
    cat(what, file = file, sep = "\n")
  }
)
output$exporttxtCS<-downloadHandler(
  filename = function() paste(rev(strsplit(fileData(),"[\\/]")[[1]])[1],"-CS.txt",sep=""),
  content = function(file) {
    what=sapply(csect2()$exptxt,paste,collapse='\t')
    cat(what, file = file, sep = "\n")
  }
)
output$exporttxtKM<-downloadHandler(
  filename = function() paste(rev(strsplit(fileData(),"[\\/]")[[1]])[1],"-KM.txt",sep=""),
  content = function(file) {
        if(input$radiokm=='None') what=sapply(kmeier()$exptxt,paste,collapse='\t')
        if(input$radiokm!='None') what=sapply(kmeier2()$exptxt,paste,collapse='\t')
    cat(what, file = file, sep = "\n")
  }
)
output$exporttxtDS<-downloadHandler(
  filename = function() paste(rev(strsplit(fileData(),"[\\/]")[[1]])[1],"-data.tsv",sep=""),
  content = function(file) {
    what=sapply(downloadFile(dat()),paste,collapse='\t')
    cat(what, file = file, sep = "\n")
  }
)

############################################################################

})
