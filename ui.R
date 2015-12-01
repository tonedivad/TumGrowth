library(shiny)
library(rCharts)
library(shinythemes)
library(shinyBS)

textInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,...))
}

textInput2<-function (inputId, label, value = "",...) 
{
  tagList(tags$label(label, `for` = inputId), 
          tags$input(id = inputId,type = "text", value = value,...))
}

lastline<-function() 
  h6("This application was created at the ", a("Kroemer lab", href="http://www.kroemerlab.com/"), 
     ". Please send bugs and feature requests to me.")


# h5, h6 {
#   color: #428bca;
#     font-weight: 700;
# }
# 
# .control-label, .selectize-dropdown, .item, .btn, .uneditable-input, .sorting, .odd, .even, .dataTables-info, .fa, .fa-refresh  {
#   font-size: 12px;
# }

shinyUI( 
  fluidPage(
    #  theme = shinytheme("flatly"),
    theme = "mbootstrap.css",
    titlePanel("Analysis of tumour growth experiments"),
    headerPanel(tags$head(
      tags$style(type="text/css", "h4 { font-size: 16px; font-weight: 700; color: #666666}"),
      tags$style(type="text/css", ".control-label { font-weight: 500; color: #000000}"),
      tags$style(type="text/css", ".bg-danger { background-color: #000000}")
      #type="text/css", "label.radio { display: inline-block; }",
      #                                      ".radio input[type=\"radio\"] { float: none; }"),
      #                           tags$style(type="text/css", "select { max-width: 200px; }"),
      #                #           tags$style(type="text/css", "textarea { max-width: 185px; }"),
      #                           tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      #                           tags$style(type='text/css', ".well { max-width: 400px; }")
      #                #           tags$style(type='text/css', ".span4 { max-width: 330px; }")
    ) 
    ),
    sidebarPanel(
      #################
      conditionalPanel(condition="input.tabs1=='Data upload'",
                       #      h4("Enter data"),
                       radioButtons("dataInput", "Enter data", list("Load sample data"=1,"Upload file"=2)),
                       conditionalPanel(condition="input.dataInput=='1'",
                                        radioButtons("sampleData", "Load sample data", 
                                                     list("Longitudinal test"="Test1","Kaplan-Meier test"="Test2","Cross-sectional test"="TestCRM"))
                       ),
                       conditionalPanel(condition="input.dataInput=='2'",
                                        fileInput('browse', 'File to upload (tab separated/no UTF16)', multiple = FALSE),
                                        radioButtons("loadtyp", NULL,list("Long"="long","Wide"="wide"),inline=TRUE)
                       ),
                       checkboxInput("exclzer", label = 'Exclude zeros', value = FALSE),
                       checkboxInput("imputezer", label = 'Replace zeros for log trans.', value = TRUE),
                       checkboxInput("trim", label = 'Trim same values at end', value = TRUE),
                       div(align = "center", downloadButton("exporttxtDS", "Download data"))
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Line charts'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxes"),
                       uiOutput("choices"),#,
                       div(align = "center", h4("Visualisation"),
                           checkboxInput("forcezero", label = 'Join TC to zero', value = FALSE),
                           textInput3(inputId="tcdef", label="Response at t=0", value = '', class="input-small"),
                           radioButtons("tcse",NULL, list("SE"="SE","SD"="SD"),inline=TRUE)),
                       br(),
                       div(align = "center", h4("Image export"),
                           textInput3(inputId="tcwidth", label="Width", value = 5, class="input-small"),
                           textInput3(inputId="tcheight", label="Height", value = 4, class="input-small"),
                           textInput3(inputId="tccex", label="Cex", value = 1, class="input-small"),
                           tags$head(tags$style(type="text/css", "#tcdef {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tcwidth {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tcheight {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tccex {width: 35px}"))),
                       div(align = "center",
                           radioButtons("tcfplottyp",NULL, list("All TCs"="All","Mean/SE"="Mean"),inline=TRUE),
                           radioButtons("tcfplot", label =  NULL,inline = T,choices =c('Svg','Png'),selected = 'Svg'),
                           downloadButton("downloadTC", "Plot"))
      ),
     #################
      conditionalPanel(condition="input.tabs1=='Longitudinal'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxeslg"),
                       uiOutput("choiceslg"),#,
                       div(align = "center", h4("Mixed effect modelling"),
                       checkboxInput("radiolongvar", label = 'Check group-heteroscedasticity', value = TRUE),
                       radioButtons("bfco", label =  'Outlier detection threshold:',inline = T,
                                    choices =rev(c('p<0.2','p<0.1','p<0.05','p<0.01','p<0.001','None')), selected = 'p<0.1'),
                       actionButton("goButton", "Compute")),
                       br(),
                       div(align = "center", downloadButton("exporttxtLG", "Results"))
                       
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Cross-sectionnal'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxescs"),
                       uiOutput("choicescs"),#,
                       div(align = "center", h4("Time range")),
                       checkboxInput("radiocsMax", label = "Use maximum value rather than last measurement?", value = FALSE),
                       uiOutput("slidecsui"),
                       div(align = "center", h4("Linear modelling")),
                       radioButtons("bfcocs", label =  'Outlier detection threshold:',inline = T,
                                    choices =c('p<0.2','p<0.1','p<0.05','p<0.01','p<0.001','None'), 
                                    selected = 'None'),
                       checkboxInput("radiocsvar", label = 'Check group-heteroskedacity', value = TRUE),
                       br(),
                       
                       div(align = "center", h4("Image export"),
                       textInput3(inputId="cswidth", label="Width", value = 3.2, class="input-small"),
                       textInput3(inputId="csheight", label="Height", value = 3.5, class="input-small"),
                       textInput3(inputId="cscex", label="Cex", value = 1, class="input-small"),
                       textInput3(inputId="cscexpt", label="CexPt", value = 1.3, class="input-small"),
                       tags$head(tags$style(type="text/css", "#cswidth {width: 35px}")),
                       tags$head(tags$style(type="text/css", "#csheight {width: 35px}")),
                       tags$head(tags$style(type="text/css", "#cscex {width: 35px}")),
                       tags$head(tags$style(type="text/css", "#cscexpt {width: 35px}"))),
                       div(align = "center", radioButtons("csplot", label =  NULL,inline = T,
                                                          choices =c('Svg','Png'),selected = 'Svg'),
                           downloadButton("downloadCS", "Plot"),downloadButton("exporttxtCS", "Results"))
      ),
     #################
     conditionalPanel(condition="input.tabs1=='Survival'",
                      div(align = "center", h4("Data input")),
                      uiOutput("boxeskm"),
                      uiOutput("choiceskm"),#,
                      # br(),
                      div(align = "center", h4("Censoring on response/time")),
                      uiOutput("slidekmui"),
                      uiOutput("sliderkmtui"),                       
                      br(),
                      div(align = "center", h4("Visualisation"),
                          textInput3(inputId="kmshift", label="Shifting factor", value = 0.1, class="input-small")),    
                      br(),
                      div(align = "center", h4("Image export"),
                          textInput3(inputId="kmwidth", label="Width", value = 5, class="input-small"),
                          textInput3(inputId="kmheight", label="Height", value = 4, class="input-small"),
                          textInput3(inputId="kmcex", label="Cex", value = 1, class="input-small"),
                          textInput3(inputId="kmcexpt", label="CexPt", value = 1.2, class="input-small"),
                          textInput3(inputId="kmlwd", label="Lwd", value = 1.5, class="input-small"),
                          tags$head(tags$style(type="text/css", "#kmwidth {width: 35px}")),
                          tags$head(tags$style(type="text/css", "#kmheight {width: 35px}")),
                          tags$head(tags$style(type="text/css", "#kmshift {width: 35px}")),
                          tags$head(tags$style(type="text/css", "#kmlwd {width: 35px}")),
                          tags$head(tags$style(type="text/css", "#kmcex {width: 35px}")),
                          tags$head(tags$style(type="text/css", "#kmcexpt {width: 35px}"))),
                      div(align = "center", radioButtons("kmfplot", label =  NULL,inline = T,
                                                         choices =c('Svg','Png'),selected = 'Svg'),
                          downloadButton("downloadKM", "Plot"),downloadButton("exporttxtKM", "Results"))
     )
      #################
    ), ## sidepanel
    mainPanel(
      tabsetPanel(
        tabPanel("Data upload",
                 bsCollapse(id = "collapseDS", open = "Experimental design",
                            bsCollapsePanel("Experimental design",  tableOutput("design"),style = "info"),
                            bsCollapsePanel("Data in wide format", 
                                            uiOutput("choicesdat"),
                                            uiOutput("groupsdat"),
                                            DT::dataTableOutput("filetableshort"),style = "info"),
                            bsCollapsePanel("Data in long format",
                                            uiOutput("groupsdat2"),
                                            DT::dataTableOutput("filetablelong"),style = "info")
                            #bsCollapsePanel("Original data", dataTableOutput("filetableori"),style = "info")
                 ),
                 lastline()
        ),
        # Visualisation
        tabPanel("Line charts",
                 bsCollapse(id = "collapseTC", open = "All",
                            bsCollapsePanel("All",
                                            showOutput("plottc3a", "highcharts"),style = "info"),
                            bsCollapsePanel("Mean", showOutput("plottc3b", "highcharts"),style = "info")
                 ),
                 lastline()
        ),
        tabPanel("Longitudinal",
                 bsCollapse(id = "collapseLG", open = "ANOVA",
                            bsCollapsePanel("ANOVA", tableOutput("modeleffect"),style = "info"),
                            bsCollapsePanel("Pairwise comparisons", 
                                            uiOutput("radiolong"),
                                            DT::dataTableOutput("modelpw"), style = "info"),
                            bsCollapsePanel("Diagnostics plots",
                                            radioButtons("radiodiag", label = NULL,inline = T,
                                                         choices = c('QQ-plot','Fit','Resid/Mice','Resid/Fit'), selected = 'QQ-plot'),
                                            showOutput("plottc4", "highcharts"), style = "info"),
                            bsCollapsePanel("Model information",tableOutput("modelsum"),
                                            verbatimTextOutput("modelcor"),
                                            verbatimTextOutput("modelwei"), verbatimTextOutput("modelout"), style = "info")
                 ),
                 lastline()
        ),
        tabPanel("Cross-sectionnal",
                 bsCollapse(id = "collapseCS", open = "Distribution",
                            bsCollapsePanel("Distribution", showOutput("plotcs", "highcharts"),style = "info"),
                            bsCollapsePanel("Pairwise comparisons",uiOutput("grpcsvar"),
                                            DT::dataTableOutput("ctCS"), style = "info"),
                            bsCollapsePanel("ANOVA",tableOutput("modCS"), verbatimTextOutput("csout"), style = "info"),
                            bsCollapsePanel("Time point selection",tableOutput("sumCS"),
                                            h6('Time of sampling is specified for each animals if different in the specified time range.'),
                                            style = "info")),
                 lastline()
        ),
        tabPanel("Survival",
                 bsCollapse(id = "collapseKM", open = "Kaplan Meier",
                            bsCollapsePanel("Kaplan meier", showOutput("plotkm", "highcharts"),style = "info"),
                            bsCollapsePanel("Cox regression",
                                            uiOutput("radiokm"),
                                            checkboxInput("radiokmFirth", label = 'Use Firth penalised Cox regression', value = FALSE),                                          
                                            tableOutput("modKM"),
                                            DT::dataTableOutput("hrKM"), style = "info"),
                            bsCollapsePanel("Censoring information", tableOutput("sumKM"),
                                            h6('For each animal, the last measurement is given in parenthesis followed by +
                                               if death occured.'),
                                            style = "info")
                 ),
                 lastline()
        ),
        id="tabs1"
      )
    ) ### main panel
  ) ## fluid page
)


