library(shiny)
library(rCharts)
library(shinythemes)
library(shinyBS)


textInput3<-function (inputId, label, value = "",class='input-small',style='width:35px') 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId,style="font-weight:normal",float="left"), 
      tags$input(id = inputId, type = "text", value = value,class=class,style=style))
}

checkboxInput2<-function (inputId, label, value = FALSE, width = NULL) 
{
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), 
    div(class = "checkbox  pull-left",tags$label(tags$span(label),inputTag)))
}

# checkboxGroupInput2<-function (inputId, label, choices, selected = NULL, inline = FALSE, 
#           width = NULL,taglab=NULL) 
# {
#   choices <- shiny:::choicesWithNames(choices)
#   if (!is.null(selected)) 
#     selected <- shiny:::validateSelected(selected, choices, inputId)
#   options <- mygenerateOptions(inputId, choices, selected, inline,taglab=taglab)
#   divClass <- "form-group shiny-input-checkboxgroup shiny-input-container"
#   if (inline) 
#     divClass <- paste(divClass, "shiny-input-container-inline")
#   divClass=paste(divClass,"for='group'")
#   istyle=''
#   if (!is.null(width)) 
#     istyle=paste0("width: ", validateCssUnit(width), ";")
#   
#   tags$div(id = inputId, style = istyle, class = divClass, 
#            shiny:::controlLabel(inputId, label), options)
# }

lastline<-function() 
  h6("This application was created at the ", 
     a("Kroemer lab", href="http://www.kroemerlab.com/",style='color: #012f73'), 
     ". Source code available on", 
     a("Github.", href="https://github.com/kroemerlab/TumGrowth",style='color: #012f73'))



# h5, h6 {
#   color: #428bca;
#     font-weight: 700;
# }
# 
# .control-label, .selectize-dropdown, .item, .btn, .uneditable-input, .sorting, .odd, .even, .dataTables-info, .fa, .fa-refresh  {
#   font-size: 12px;
# }

## https://bootstrapbay.com/blog/bootstrap-button-styles/

shinyUI( 
  fluidPage(
     theme = shinytheme("flatly"),
    #theme = "mbootstrap.css",
    h2("Analysis of tumour growth experiments",style='color: #012f73'),
    includeCSS('www/styling.css'),
 #   includeCSS('www/check-radio.css'),
    sidebarPanel(
      #################
      conditionalPanel(condition="input.tabs1=='Data upload'",
                           div(align = "center", h4("Data input")),
                       radioButtons("dataInput", NULL, list("Load sample data"=1,"Upload file"=2)),
                       conditionalPanel(condition="input.dataInput=='1'",
                                        radioButtons("sampleData", "Example data", 
                                                     list("Longitudinal test"="Test1","Kaplan-Meier test"="Test2",
                                                          "Cross-sectional test"="TestCRM",'Toy dataset'='Toy'))
                       ),
                       conditionalPanel(condition="input.dataInput=='2'",
                                        h5('Tab separated format (see help)'),
                                        HTML("<button id='clearFile1' class='action-button clearButton'>Clear</button>"),
                                        fileInput('browse', NULL, multiple = FALSE)
                       ),
                       div(align = "center", h4("Parsing parameters")),
                       textInput3(inputId="ndigits", label="1: num of digits", value = 4),
                       checkboxInput("trimzer", label = '2: remove zeros from the end', value = FALSE),
                       checkboxInput("trim", label = '3: trim same values from the end', value = TRUE),
                       textInput3(inputId="setday0", label="4: set first detected at day", value = ''),
                       checkboxInput("exclzer", label = '5: exclude any other zeros', value = FALSE),
                       checkboxInput("imputezer", label = '6: replace zeros for log trans', value = TRUE),
                       checkboxInput("set2surv", label = '7: set to surviving at last experiment day.', value = FALSE),
                       checkboxInput('showPanel1', '8: change default colors', FALSE),
                       conditionalPanel(condition = 'input.showPanel1',
                                        div(align = "center", wellPanel(uiOutput("groupscols")),
                                            actionButton("updateCols", "Update colors",width='120px'))
                       ),
                       br(),
                       div(align = "center", 
                           h4("Export parsed dataset"),
                           checkboxInput("exporttrans", label = 'Export transformed data', value = FALSE),
                           downloadButton("exporttxtDS", "Download dataset"))
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Line charts'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxes"),
                       uiOutput("choices"),#,
                       uiOutput("choicestrans"),
                       br(),
                       div(align = "center", h4("Visualisation")),
                       checkboxGroupInput("tcfplottyp",NULL,#"Display", 
                                          choices=list("Time courses"="tc","Mean/SE"="mese","Mean/SD"="mesd"),
                                          selected='tc',inline=TRUE),
                  #     checkboxInput("btresplc", label = 'Back transform the data', value = TRUE),
                       textInput3(inputId="tcminy", label="y-axis: min.", value = ''),
                       textInput3(inputId="tcmaxy", label="max.", value = ''),
                       checkboxInput('showPanel2', 'force time courses to start at 0', FALSE),
                       conditionalPanel(condition = 'input.showPanel2',
                                        textInput3(inputId="tcdef", label="y at t0", value = '')
                       ),
                        br(),
                       div(align = "center", h4("Image export"),
                           textInput3(inputId="tcwidth", label="Width", value = 5),
                           textInput3(inputId="tcheight", label="Height", value = 4),
                           textInput3(inputId="tccex", label="Cex", value = 1),
                 #      div(align = "center",
                 br(),
                           downloadButton("downloadTCsvg", "SVG"),downloadButton("downloadTCpng", "PNG"))
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Longitudinal'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxeslg"),
                       uiOutput("choiceslg"),
                       uiOutput("choiceslgtrans"),
                       br(),
                       div(align = "center", h4("Analysis parameters")),
                       checkboxInput('showPanelLG1', 'Piecewise growth curve regression', FALSE),
                       conditionalPanel(condition = 'input.showPanelLG1',
                                        textInput3(inputId="tpcut", label="Common breakpoint at time=", value = '')),
                       checkboxInput('showPanelLG2', 'Automatic outlier detection', TRUE),
                       conditionalPanel(condition = 'input.showPanelLG2',
                                        radioButtons("bfcolg", label =  NULL,inline = TRUE,
                                    choices =rev(c('p<0.2','p<0.1','p<0.05','p<0.01','p<0.001')), selected = 'p<0.1')),
                       div(align = "center",actionButton("goButton", "Compute model")),
                       br(),
                       div(align = "center", h4("Analysis report"),
                           radioButtons('formatLG',NULL, c('PDF', 'HTML', 'DOCX'),
                                        inline = TRUE),
                           downloadButton("exporttxtLG", "Report"))
                       
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Cross-sectionnal'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxescs"),
                       uiOutput("choicescs"),#,
                       uiOutput("choicescstrans"),
                       h5("Data points selection:"),
                       uiOutput("slidecsui"),
                       radioButtons("radiocsMax", label = NULL,inline=TRUE,choices=c('Last','Max. measurement'),selected = 'Last'),
                       br(),
                       div(align = "center", h4("Analysis parameters")),
                       checkboxInput('showPanelCS1', 'Automatic outlier detection', TRUE),
                       conditionalPanel(condition = 'input.showPanelCS1',
                                        radioButtons("bfcocs", label =  NULL,inline = TRUE,
                                                     choices =rev(c('p<0.2','p<0.1','p<0.05','p<0.01','p<0.001')), selected = 'p<0.1')),
                       checkboxInput("radiocsvar", label = 'Check group-heteroskedacity', value = TRUE),
                       br(),
                       div(align = "center", h4("Visualisation")),
                #           checkboxInput("btrespcs", label = 'Back transform the data', value = TRUE),
                       textInput3(inputId="csminy", label="y-axis: min.", value = ''),
                           textInput3(inputId="csmaxy", label="max.", value = ''),
                       br(),
                       div(align = "center", h4("Image export"),
                           textInput3(inputId="cswidth", label="Width", value = 4),
                           textInput3(inputId="csheight", label="Height", value = 3.5),
                           textInput3(inputId="cscex", label="Cex", value = 1),
                           textInput3(inputId="cscexpt", label="CexPt", value = 1.2)),
                       div(align = "center",downloadButton("downloadCSsvg", "SVG"),downloadButton("downloadCSpng", "PNG")),
                       br(),
                       div(align = "center", h4("Analysis report"),
                           radioButtons('formatCS', NULL, c('PDF', 'HTML', 'DOCX'),inline = TRUE),
                           downloadButton("exporttxtCS", "Report"))
      ),
      #################
      conditionalPanel(condition="input.tabs1=='Survival'",
                       div(align = "center", h4("Data input")),
                       uiOutput("boxeskm"),
                       uiOutput("choiceskm"),#,
                       # br(),
                       radioButtons("survTyp", "Time to event definition (see help):",
                                    list("overall survival"=1,
                                         "tumour-free survival"=2)),
                       conditionalPanel(condition="input.survTyp=='1'",
                                        h5("Censoring response and time"),uiOutput("slidekmui"),uiOutput("sliderkmtui")),  
                       conditionalPanel(condition="input.survTyp=='2'",
                                        h5("Tumour detection level"),uiOutput("slidekmui2")),
                       br(),
                       div(align = "center", h4("Visualisation"),
                       textInput3(inputId="kmshift", label="Shifting factor", value = 0.1), 
                           textInput3(inputId="kmlwd", label="Lwd", value = 1.5)),
                       br(),
                       div(align = "center", h4("Image export"),
                           textInput3(inputId="kmwidth", label="Width", value = 5),
                           textInput3(inputId="kmheight", label="Height", value = 4),
                           textInput3(inputId="kmcex", label="Cex", value = 1),
                           textInput3(inputId="kmcexpt", label="CexPt", value = 1.2)),
                       div(align = "center",  downloadButton("downloadKMsvg", "SVG"),
                           downloadButton("downloadKMpng", "PNG")),
                       br(),
                       div(align = "center", h4("Analysis report"),
                           radioButtons('formatKM', NULL, c('PDF', 'HTML', 'DOCX'),inline = TRUE),
                           downloadButton("exporttxtKM", "Report"))
      ),
      conditionalPanel(condition="input.tabs1=='About'",
                       HTML('<p>Guided steps for using and perhaps understanding <code>Tumgrowth</code></p>')
      )
      #################
    ), ## sidepanel
    mainPanel(
      tabsetPanel(
        tabPanel("Data upload",
                 bsCollapse(id = "collapseDS", open = "Experimental design",multiple=TRUE,
                            bsCollapsePanel("Experimental design",
                                            uiOutput("choicesdatexp"),
                                            tableOutput("design"),
                                            style = "info"),
                            bsCollapsePanel("Data in wide format", 
                                            uiOutput("choicesdat"),
                                            uiOutput("groupsdat"),
                                            DT::dataTableOutput("filetableshort"),style = "info"),
                            bsCollapsePanel("Further information", 
                                            includeMarkdown("help/About-upload.md"),
                                            style = "info")
                 ),
                 lastline()
        ),
        # Visualisation
        tabPanel("Line charts",
                 bsCollapse(id = "collapseLC",open = c("Overlay"),multiple=TRUE,
                            bsCollapsePanel("Overlay", showOutput("plottcs", "highcharts"),style="info"),
                            bsCollapsePanel("Individual models" ,style="info"),
                            bsCollapsePanel("Further information", includeMarkdown("help/About-lc.md"),style = "info")
                 )
        ),
        # Longitudinal
        tabPanel("Longitudinal",
                 bsCollapse(id = "collapseLG", open = c("Type II ANOVA","Pairwise comparisons"),multiple=TRUE,
                            bsCollapsePanel("Type II ANOVA", tableOutput("modelLGeffect"),style = "info"),
                            bsCollapsePanel("Pairwise comparisons", 
                                            uiOutput("grplgvar"),
                                            DT::dataTableOutput("modelLGpw"), style = "info"),
                            bsCollapsePanel("Diagnostics plots",
                                            radioButtons("radiodiaglg", label = NULL,inline = TRUE,
                                                         choices = c('QQ-plot','Resid/Fit','Fit','FitBt','Resid/Mice','Resid/Grp','Resid/Tp'), selected = 'QQ-plot'),
                                            showOutput("plotdiaglg", "highcharts"), style = "info"),
                            bsCollapsePanel("Model information",tableOutput("modelLGsum"),
                                            verbatimTextOutput("modelLGcor"),
                                            verbatimTextOutput("modelLGwei"),
                                            verbatimTextOutput("outLG"), style = "info"),
                            bsCollapsePanel("Further information", 
                                            includeMarkdown("help/About-lg.md"),style = "info")
                            
                 ),
                 lastline()
        ),
        tabPanel("Cross-sectionnal",
                 bsCollapse(id = "collapseCS", open = c("Distribution","Pairwise comparisons"),multiple=TRUE,
                            bsCollapsePanel("Distribution", showOutput("plotcs", "highcharts"),style = "info"),
                            bsCollapsePanel("Pairwise comparisons",tableOutput("modCSeffect"),uiOutput("grpcsvar"),
                                            DT::dataTableOutput("ctCS"), style = "info"),
                            bsCollapsePanel("Diagnostics plots",
                                            radioButtons("radiodiagcs", label = NULL,inline = TRUE,
                                                         choices = c('QQ-plot','Resid/Grp','Resid/Fit'), selected = 'QQ-plot'),
                                            showOutput("plotdiagcs", "highcharts"), style = "info"),
                            bsCollapsePanel("Model information",
                                            tableOutput("modCS"),verbatimTextOutput("modCSwei"),
                                            verbatimTextOutput("outCS"), style = "info"),
                            bsCollapsePanel("Data points selection",tableOutput("sumCS"),
                                            h6('Time of sampling is specified for each animals if different in the specified time range.'),
                                            style = "info"),
                            bsCollapsePanel("Further information", 
                                            includeMarkdown("help/About-cs.md"),style = "info")
                            ),
                 lastline()
        ),
        tabPanel("Survival",
                 bsCollapse(id = "collapseKM", open = c("Kaplan Meier","Cox regression"),multiple=TRUE,
                            bsCollapsePanel("Kaplan Meier", showOutput("plotkm", "highcharts"),style = "info"),
                            bsCollapsePanel("Cox regression",
                                            uiOutput("radiokm"),
                                            checkboxInput("radiokmFirth", label = 'Use Firth penalised Cox regression', value = FALSE),                                          
                                            tableOutput("modKM"),
                                            DT::dataTableOutput("hrKM"), style = "info"),
                            bsCollapsePanel("Censoring information", tableOutput("sumKM"),
                                            h6('In parentheses, follow-up time together with +(event) or -(censoring).'),
                                            style = "info"),
                            bsCollapsePanel("Further information", 
                                            includeMarkdown("help/About-km.md"),style = "info")
                 ),
                 lastline()
        ),
        tabPanel("About",
                 bsCollapse(id = "about", open = NULL,multiple=TRUE,
                            bsCollapsePanel("Data input",includeMarkdown("help/About-upload.md"),style = "info"),
                            bsCollapsePanel("Line charts",includeMarkdown("help/About-lc.md"),style = "info"),
                            bsCollapsePanel("Longitudinal analysis",includeMarkdown("help/About-lg.md"),style = "info"),
                            bsCollapsePanel("Cross-sectional analysis",includeMarkdown("help/About-cs.md"),style = "info"),
                            bsCollapsePanel("Survival analysis",includeMarkdown("help/About-km.md"),style = "info")
                 ),
                 lastline()
        ),
        id="tabs1"
      )
    ),
    singleton(includeScript("www/active.js"))
    ### main panel
  ) ## fluid page
  
)


