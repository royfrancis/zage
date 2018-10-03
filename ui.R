# ZAGE SHINYAPP
# 2018 Roy Mathew Francis
# UI.R

source("functions.R")

shinyUI(
  fluidPage(theme=shinytheme("flatly"),
            includeCSS("www/styles.css"),
            tags$head(includeScript("google-analytics.js")),
            tags$title("Zage"),
            fluidRow(style="height:80px",
                     fluidRow(style="padding-left:4%;min-height:80px;",
                              HTML("<div><p><h2><b>Zage</b> | Plan your Sequencing</h2></p></div>"),
                              HTML("<div><img src='logo_scilifelab.svg' style='position:absolute;right:0;top:1%;padding-right:3%;height:55px;' class='img-responsive'</div>")
                     )
            ),
            fluidRow(style="padding-right:3%;padding-left:3%",
                     column(12,
                            fluidRow(
                              column(3,
                                     tags$br(),
                                     selectInput("in_select_gsize","Genome size selection",choices=c("From List","Custom"),selected=1,selectize=T,multiple=F),
                                     uiOutput("ui_gsize"),
                                     selectInput("in_ome","Genome type",choices=c("Genome","Transcriptome"),selected="Genome",selectize=T,multiple=F),
                                     numericInput("in_num_samples","Total number of samples",value=12,min=1,step=1),
                                     htmlOutput("out_ome_size"),
                                     tags$br(),
                                     uiOutput("ui_report"),
                                     tags$hr(),
                                     #verbatimTextOutput("out_display"),
                                     HTML(paste0("<small>Zage version: ",fnv$appversion,". Data updated: ",fnv$datadate,"</small></br>")),
                                     HTML("<small>2018 | Roy Francis</small>")
                                     
                              ),
                              column(9,
                                     tags$br(),
                                     tabsetPanel(id="tabset_main",
                                                 tabPanel("Overview",
                                                          fluidRow(
                                                            HTML('<div class="row" style="margin: 15px;">
    <div class="col-auto">
      <a class="btn btn-default" style="padding:2px 4px;" data-toggle="collapse" href="#ac2" role="button" aria-expanded="false" aria-controls="ac2">
      <i class="fa fa-info-circle fa-lg"></i>
      </a>
</div>
<div class="col-sm-11">
      <div class="collapse" id="ac2">
      <div class="card card-body" style="border:1px solid lightgrey;border-radius:4px;padding:10px;color:grey;">
      This section gives an overview over all protocols for a fixed pooling (samples per lane). Genome size and number of samples are used from the left panel.
      </div>
      </div>
      </div>
      </div>'),
                                                            column(3,style="padding-top:12px;",
                                                                   selectInput("in_criteria","Select criteria to set",choices=c("Set Number of pools","Set Samples per pool"),selected=1,selectize=TRUE,multiple=FALSE)),
                                                            column(2,style="padding-top:5px;",
                                                                   uiOutput("ui_criteria")
                                                            ),
                                                            column(7,
                                                                   tags$br(),
                                                                   tags$br(),
                                                                   textOutput("out_criteria")
                                                            )
                                                          ),
                                                          tags$hr(),
                                                          DTOutput("out_table_wide_dt"),
                                                          tags$br()
                                                 ),
                                                 tabPanel("Compare Protocols",
                                                          fluidRow(
                                                            HTML('<div class="row" style="margin: 15px;">
    <div class="col-auto">
                                                                 <a class="btn btn-default" style="padding:2px 4px;" data-toggle="collapse" href="#ac3" role="button" aria-expanded="false" aria-controls="ac3">
                                                                 <i class="fa fa-info-circle fa-lg"></i>
                                                                 </a>
                                                                 </div>
                                                                 <div class="col-sm-11">
                                                                 <div class="collapse" id="ac3">
                                                                 <div class="card card-body" style="border:1px solid lightgrey;border-radius:4px;padding:10px;color:grey;">
                                                                 This sections compares protocols based on various metrics for all possible poolings (samples per lane). Genome size and number of samples are used from the left panel. This section can take a few extra seconds to load.
                                                                 </div>
                                                                 </div>
                                                                 </div>
                                                                 </div>'),
                                                            column(12,
                                                                   tags$br(),
                                                                   htmlOutput("out_plot_container")
                                                            )
                                                          )
                                                 ),
                                                 tabPanel("Selected Protocol",
                                                          fluidRow(
                                                            HTML('<div class="row" style="margin: 15px;">
    <div class="col-auto">
                                                                 <a class="btn btn-default" style="padding:2px 4px;" data-toggle="collapse" href="#ac4" role="button" aria-expanded="false" aria-controls="ac4">
                                                                 <i class="fa fa-info-circle fa-lg"></i>
                                                                 </a>
                                                                 </div>
                                                                 <div class="col-sm-11">
                                                                 <div class="collapse" id="ac4">
                                                                 <div class="card card-body" style="border:1px solid lightgrey;border-radius:4px;padding:10px;color:grey;">
                                                                 This sections provides detailed insights for a selected protocol for all possible poolings (samples per lane). Genome size and number of samples are used from the left panel.
                                                                 </div>
                                                                 </div>
                                                                 </div>
                                                                 </div>'),
                                                            column(12,
                                                                   fluidRow(
                                                                     column(4,
                                                                            selectInput("in_protocol","Protocol",choices=choices_protocol,selected=1,selectize=T,multiple=F)
                                                                     ),
                                                                     column(8,
                                                                            tags$br(),
                                                                            checkboxInput("in_multiple","Show short list.",value=TRUE)
                                                                     )
                                                                   ),
                                                                   tags$hr(),
                                                                   DTOutput("out_table_long"),
                                                                   tags$br(),
                                                                   tags$hr(),
                                                                   tags$br(),
                                                                   highchartOutput("out_plot",height="600px",width="940px")
                                                            )
                                                          )
                                                 ),
                                                 tabPanel("Power Analysis",
                                                          fluidRow(
                                                            HTML('<div class="row" style="margin: 15px;">
    <div class="col-auto">
                                                                 <a class="btn btn-default" style="padding:2px 4px;" data-toggle="collapse" href="#ac5" role="button" aria-expanded="false" aria-controls="ac5">
                                                                 <i class="fa fa-info-circle fa-lg"></i>
                                                                 </a>
                                                                 </div>
                                                                 <div class="col-sm-11">
                                                                 <div class="collapse" id="ac5">
                                                                 <div class="card card-body" style="border:1px solid lightgrey;border-radius:4px;padding:10px;color:grey;">
                                                                 Power Analysis for RNA-Seq. This section is stand-alone (does not use values from other sections).  This section assumes comparison of two groups with equal number of samples. Multiple values can be entered using comma separation. Sequencing depth is input only and cannot be estimated.
                                                                 </div>
                                                                 </div>
                                                                 </div>
                                                                 </div>'),
                                                            column(4,
                                                              selectInput("in_pa_est","Variable to estimate",choices=choices_pa,selected=1,selectize=TRUE,multiple=FALSE),
                                                              uiOutput("ui_pa")
                                                            ),
                                                            column(8,
                                                              htmlOutput("out_pa_label"),
                                                              div(style="overflow-y:scroll;max-height:820px;margin-bottom:10px;margin-top:5px;",
                                                            verbatimTextOutput("out_pa")
                                                            )
                                                            )
                                                          )
                                                 ),
                                                 tabPanel("Guide",
                                                          fluidRow(
                                                            includeMarkdown("guide.md")
                                                          )
                                                 ),
                                                 tabPanel("Version",
                                                          fluidRow(
                                                            includeMarkdown("versions.md")
                                                          )
                                                 )
                                     )
                              )
                            )
                     )
            )
  )
)
