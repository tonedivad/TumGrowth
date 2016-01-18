library(shiny)
library(rCharts)
library(shinythemes)
library(shinyBS)




textInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId,style="font-weight:normal"), 
      tags$input(id = inputId, type = "text", value = value,...))
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
                       div(align = "center", h4("Data input")),
                       radioButtons("dataInput", NULL, list("Load sample data"=1,"Upload file"=2)),
                       conditionalPanel(condition="input.dataInput=='1'",
                                        radioButtons("sampleData", "Example data", 
                                                     list("Longitudinal test"="Test1","Kaplan-Meier test"="Test2","Cross-sectional test"="TestCRM"))
                       ),
                       conditionalPanel(condition="input.dataInput=='2'",
                                        fileInput('browse', 'File to upload (tab separated/no UTF16)', multiple = FALSE)
                                        #radioButtons("loadtyp", NULL,list("Long"="long","Wide"="wide"),inline=TRUE)
                       ),
                       #  textInput3(inputId="ndigit", label="Rounding", value = '4', class="input-small"),
                       tags$head(tags$style(type="text/css", "#browse {font-weight: normal}")),
                       div(align = "center", h4("Parsing parameters")),
                       textInput3(inputId="ndigits", label="0: num of digits", value = 4, class="input-small"),
                       checkboxInput("sumids", label = '1: sum duplicated ids', value = FALSE),
                       checkboxInput("trimzer", label = '2: remove zeros from the end', value = FALSE),
                       checkboxInput("trim", label = '3: trim same values from the end', value = TRUE),
                       #    checkboxInput("setday0", label = '4: set day 1 at first detected', value = FALSE),
                       textInput3(inputId="setday0", label="4: set first detected at day", value = '', class="input-small"),
                       checkboxInput("exclzer", label = '5: exclude any other zeros', value = FALSE),
                       checkboxInput("imputezer", label = '6: replace zeros for log trans.', value = TRUE),
                       div(align = "center", h4("Graphics settings")),
                       checkboxInput('showPanel1', 'Custom colors', FALSE),
                       conditionalPanel(condition = 'input.showPanel1',
                                        div(align = "center", wellPanel(uiOutput("groupscols")),
                                            actionButton("updateCols", "Update colors",width='120px'))
                       ),
#                        checkboxInput('showPanel2', 'SVG export (not active!)', FALSE),
#                        conditionalPanel(condition = 'input.showPanel2',
#                                         div(align = "center", wellPanel(
#                                           textInput3(inputId="svgwidth", label="Width", value = 5, class="input-small"),
#                                           textInput3(inputId="svgheight", label="Height", value = 4, class="input-small"),
#                                           textInput3(inputId="svgcex", label="Cex", value = 1, class="input-small"),
#                                           textInput3(inputId="svgcexpt", label="Cex", value = 1, class="input-small")
#                                           ),
#                                           tags$head(tags$style(type="text/css", "#svgwidth {width: 35px}")),
#                                           tags$head(tags$style(type="text/css", "#svgheight {width: 35px}")),
#                                           tags$head(tags$style(type="text/css", "#svgcex {width: 35px}")),
#                                           tags$head(tags$style(type="text/css", "#svgcexpt {width: 35px}")),
#                                         actionButton("updateSVG", "Update SVG ",width='120px'))
#                        ),
                       br(),
                       div(align = "center", downloadButton("exporttxtDS", "Parsed dataset"))
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Line charts'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxes"),
                       uiOutput("choices"),#,
                       div(align = "center", h4("Visualisation"),
                           checkboxInput("forcezero", label = 'Join TC to zero', value = FALSE),
                           textInput3(inputId="tcdef", label="y(t=0)", value = '', class="input-small"),
                           textInput3(inputId="tcminy", label="Min y", value = '', class="input-small"),
                           textInput3(inputId="tcmaxy", label="Max y", value = '', class="input-small"),
                           tags$head(tags$style(type="text/css", "#tcminy {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tcmaxy {width: 35px}")),
                           radioButtons("tcse","Display", list("SE"="SE","SD"="SD"),inline=TRUE),
                           radioButtons("tcfplottyp",NULL, 
                                        list("TCs only"="All","Add SD/SE"="Both","SD/SE only"="Mean"),inline=TRUE)),
                       br(),
                       div(align = "center", h4("Image export"),
                           textInput3(inputId="tcwidth", label="Width", value = 5, class="input-small"),
                           textInput3(inputId="tcheight", label="Height", value = 4, class="input-small"),
                           textInput3(inputId="tccex", label="Cex", value = 1, class="input-small"),
                           tags$head(tags$style(type="text/css", "#ndigits {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#setday0 {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tcdef {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tcwidth {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tcheight {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#tccex {width: 35px}"))),
                       div(align = "center",
                           radioButtons("tcfplot", label =  NULL,inline = T,choices =c('Svg','Png'),selected = 'Svg'),
                           downloadButton("downloadTC", "Plot"))
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Longitudinal'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxeslg"),
                       uiOutput("choiceslg"),#,
                       br(),
                       div(align = "center", h4("Mixed effect modelling"),
                           checkboxInput("radiolongvar", label = 'Check group-heteroscedasticity', value = TRUE),
                           radioButtons("bfco", label =  'Outlier detection threshold:',inline = T,
                                        choices =rev(c('p<0.2','p<0.1','p<0.05','p<0.01','p<0.001','None')), selected = 'p<0.1'),
                           actionButton("goButton", "Compute model")),
                       br(),
                       div(align = "center", h4("Report format"),
                           radioButtons('formatLG',NULL, c('PDF', 'HTML', 'DOCX'),
                                        inline = TRUE),
                           downloadButton("exporttxtLG", "Report"))
                       
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Cross-sectionnal'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxescs"),
                       uiOutput("choicescs"),#,
                       h5("Time range to select data points"),
                       checkboxInput("radiocsMax", label = "Use maximum value rather than last measurement?", value = FALSE),
                       uiOutput("slidecsui"),
                      # br(),
                       div(align = "center", h4("Linear modelling")),
                       radioButtons("bfcocs", label =  'Outlier detection threshold:',inline = T,
                                    choices =c('p<0.2','p<0.1','p<0.05','p<0.01','p<0.001','None'), 
                                    selected = 'None'),
                       checkboxInput("radiocsvar", label = 'Check group-heteroskedacity', value = TRUE),
                       br(),
                       div(align = "center", h4("Visualisation and image export"),
                           textInput3(inputId="csminy", label="Min y", value = '', class="input-small"),
                           textInput3(inputId="csmaxy", label="Max y", value = '', class="input-small"),
                           textInput3(inputId="cswidth", label="Width", value = 4, class="input-small"),
                           textInput3(inputId="csheight", label="Height", value = 3.5, class="input-small"),
                           textInput3(inputId="cscex", label="Cex", value = 1, class="input-small"),
                           textInput3(inputId="cscexpt", label="CexPt", value = 1.2, class="input-small"),
                           tags$head(tags$style(type="text/css", "#csminy {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#csmaxy {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#cswidth {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#csheight {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#cscex {width: 35px}")),
                           tags$head(tags$style(type="text/css", "#cscexpt {width: 35px}")),
                           radioButtons("csplot", label =  NULL,inline = T,
                                        choices =c('Svg','Png'),selected = 'Svg'),
                           downloadButton("downloadCS", "Plot")),
                       br(),
                       div(align = "center", h4("Report format"),
                           radioButtons('formatCS', NULL, c('PDF', 'HTML', 'DOCX'),inline = TRUE),
                           downloadButton("exporttxtCS", "Report"))
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Survival'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxeskm"),
                       uiOutput("choiceskm"),#,
                       # br(),
                       radioButtons("survTyp", "Analysis type", list("Overall"=1,"Tumour-free"=2)),
                       conditionalPanel(condition="input.survTyp=='1'",
                                        h5("Censoring on response/time"),uiOutput("slidekmui"),uiOutput("sliderkmtui")),  
                       conditionalPanel(condition="input.survTyp=='2'",
                                        h5("Tumour detection level"),uiOutput("slidekmui2")),
                       br(),
                       div(align = "center", h4("Visualisation and image export"),
                           textInput3(inputId="kmshift", label="Shifting factor", value = 0.1, class="input-small"),    
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
                           tags$head(tags$style(type="text/css", "#kmcexpt {width: 35px}")),
                           radioButtons("kmfplot", label =  NULL,inline = T,
                                        choices =c('Svg','Png'),selected = 'Svg'),
                           downloadButton("downloadKM", "Plot")),
                       br(),
                       div(align = "center", h4("Report format"),
                           radioButtons('formatKM', NULL, c('PDF', 'HTML', 'DOCX'),inline = TRUE),
                           downloadButton("exporttxtKM", "Report"))
      )
      #################
    ), ## sidepanel
    mainPanel(
      tabsetPanel(
        tabPanel("Data upload",
                 bsCollapse(id = "collapseDS", open = "Experimental design",multiple=T,
                            bsCollapsePanel("Experimental design",  tableOutput("design"),
                                            style = "info"),
                            bsCollapsePanel("Data in wide format", 
                                            uiOutput("choicesdat"),
                                            uiOutput("groupsdat"),
                                            DT::dataTableOutput("filetableshort"),style = "info")
                 ),
                 lastline()
        ),
        # Visualisation
        tabPanel("Line charts",showOutput("plottcs", "highcharts"),lastline()),
        # Longitudinal
        tabPanel("Longitudinal",
                 bsCollapse(id = "collapseLG", open = c("Type II ANOVA","Pairwise comparisons"),multiple=T,
                            bsCollapsePanel("Type II ANOVA", tableOutput("modelLGeffect"),style = "info"),
                            bsCollapsePanel("Pairwise comparisons", 
                                            uiOutput("radiolong"),
                                            DT::dataTableOutput("modelLGpw"), style = "info"),
                            bsCollapsePanel("Diagnostics plots",
                                            radioButtons("radiodiaglg", label = NULL,inline = T,
                                                         choices = c('QQ-plot','Fit','Resid/Mice','Resid/Fit'), selected = 'QQ-plot'),
                                            showOutput("plotdiaglg", "highcharts"), style = "info"),
                            bsCollapsePanel("Model information",tableOutput("modelLGsum"),
                                            verbatimTextOutput("modelLGcor"),
                                            verbatimTextOutput("modelLGwei"),
                                            verbatimTextOutput("outLG"), style = "info")
                 ),
                 lastline()
        ),
        tabPanel("Cross-sectionnal",
                 bsCollapse(id = "collapseCS", open = c("Distribution","Pairwise comparisons"),multiple=T,
                            bsCollapsePanel("Distribution", showOutput("plotcs", "highcharts"),style = "info"),
                            bsCollapsePanel("Pairwise comparisons",uiOutput("grpcsvar"),
                                            DT::dataTableOutput("ctCS"), style = "info"),
                            bsCollapsePanel("Diagnostics plots",
                                            radioButtons("radiodiagcs", label = NULL,inline = T,
                                                         choices = c('QQ-plot','Resid/Grp','Resid/Fit'), selected = 'QQ-plot'),
                                            showOutput("plotdiagcs", "highcharts"), style = "info"),
                            bsCollapsePanel("Model information",tableOutput("modCSeffect"),
                                            tableOutput("modCS"),verbatimTextOutput("modCSwei"),
                                            verbatimTextOutput("outCS"), style = "info"),
                            bsCollapsePanel("Time point selection",tableOutput("sumCS"),
                                            h6('Time of sampling is specified for each animals if different in the specified time range.'),
                                            style = "info")),
                 lastline()
        ),
        tabPanel("Survival",
                 bsCollapse(id = "collapseKM", open = c("Kaplan Meier","Cox regression"),multiple=T,
                            bsCollapsePanel("Kaplan Meier", showOutput("plotkm", "highcharts"),style = "info"),
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


