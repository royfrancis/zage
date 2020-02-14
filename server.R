# ZAGE SHINYAPP
# 2020 Roy Mathew Francis
# SERVER.R

shinyServer(function(input,output,session) {
  
  
  # UI: ui_gsize ---------------------------------------------------------------
  # conditional ui for genome size input from list or custom
  
  output$ui_gsize <- renderUI({
    shiny::req(input$in_select_gsize)
    
    if(tolower(input$in_select_gsize) == "from list")
    {
      div(
        selectInput("in_label","Organism",choices=choices_org,selected="Human (Homo sapiens)",selectize=T,multiple=F)
      )
    }else{
      div(
        textInput("in_label","Organism",value="Blank",placeholder="Enter organism label"),
        numericInput("in_size","Enter genome/transcriptome size in Gb",value=1,step=0.0001)
      )
    }
    
  })
  
  # UI: ui_criteria ---------------------------------------------------------------
  # conditional ui for set criteria
  
  output$ui_criteria <- renderUI({
    shiny::req(input$in_criteria)
    
    if(tolower(input$in_criteria)=="set number of pools")
    {
      numericInput("in_criteria_value","Number of pools (Lanes)",value=8,min=1,step=1)
    }else{
      numericInput("in_criteria_value","Samples per pool",value=input$in_num_samples,min=1,step=1)
    }
    
  })
  
  # RFN: fn_org -----------------------------------------------------------
  # get org, ome and ome size and returns data.frame
  
  fn_org <- reactive({
    shiny::req(input$in_select_gsize)
    shiny::req(input$in_label)
    shiny::req(input$in_ome)
    
    v_label <- input$in_label
    v_ome <- tolower(input$in_ome)
    
    if(input$in_select_gsize=="From List")
    {
      temp <- dfr_org %>% filter(.,label==get("v_label"))
      validate(fn_validate_equal(nrow(temp) < 2,TRUE,"Organism table not filtered correctly."))
      if(v_ome == "genome") {
        temp$size_gb <- temp$dna/(10^9)
      }else{
        temp$size_gb <- temp$cdna/(10^9)
      }
      
      return(temp)
    }
  })
  
  # RFN: fn_spp -----------------------------------------------------------
  # computes samples-per-pool or num-of-pools; returns an integer
  
  fn_spp <- reactive({
    shiny::req(input$in_num_samples)
    shiny::req(input$in_criteria)
    shiny::req(input$in_criteria_value)
    
    if(tolower(input$in_criteria)=="set samples per pool") {
      # check if num-of-samples and num-of-samples-per-pool match
      validate(fn_validate_equal(input$in_criteria_value > input$in_num_samples,FALSE,"'Number of samples per pool' must be less than the 'total number of samples'."))
      validate(fn_validate_equal(input$in_num_samples %% input$in_criteria_value,0,"'Number of samples' is not a multiple of 'number of samples per pool'."))
    } else {
      # check if num-of-samples and num-of-pools match
      validate(fn_validate_equal(input$in_criteria_value > input$in_num_samples,FALSE,"'Number of pools' must be less than the 'total number of samples'."))
      validate(fn_validate_equal(input$in_num_samples %% input$in_criteria_value,0,"'Number of samples' is not a multiple of 'number of samples per pool'."))
    }
    
    return(as.integer(ceiling(input$in_num_samples/input$in_criteria_value)))
  })
  
  # RFN: fn_oz -----------------------------------------------------------
  # gets ome size returns a numeric value
  
  fn_oz <- reactive({
    shiny::req(input$in_select_gsize)
    
    if(tolower(input$in_select_gsize)=="from list")
    {
      shiny::req(fn_org())
      v_size <- fn_org()$size_gb
    }else{
      shiny::req(input$in_size)
      v_size <- input$in_size
    }
    
    return(v_size)
  })
  
  # OUT: out_ome_size -------------------------------------------------------
  # display selected genome size
  
  output$out_ome_size <- renderText({
    shiny::req(fn_oz())
    
    return(paste0("Selected genome size (Gbp): <b>",round(fn_oz(),4),"</b>"))
  })
  
  # OUT: out_criteria -----------------------------------------------
  # displays criteria samples-per-pool or number of pools
  
  output$out_criteria <- renderText({
    shiny::req(fn_spp())
    shiny::req(input$in_criteria)
    
    if(tolower(input$in_criteria)=="set number of pools")
    {
      return(paste0("Number of samples per pool (samples per lane): ",fn_spp()))
    }else{
      return(paste0("Number of pools (Lanes): ",fn_spp()))
    }
  })
  
  # RFN: fn_ins_mul -----------------------------------------------------------
  # compute overview table with min unit limits
  
  
  fn_ins_mul <- reactive({
    shiny::req(input$in_criteria)
    shiny::req(fn_spp())
    shiny::req(fn_oz())
    
    if(tolower(input$in_criteria)=="set number of pools")
    {
      samples_per_pool <- fn_spp()
      num_of_pools <- input$in_criteria_value
    }else{
      samples_per_pool <- input$in_criteria_value
      num_of_pools <- fn_spp()
    }
    
    # get ome size
    v_size <- fn_oz()
    # read type multiplier
    v_rtm <- ifelse(dfr_ins$read_type=="se",1,2)
    
    dfr_ins <- dfr_ins %>%
      mutate(bases_per_lane=(reads_per_lane*read_length*v_rtm*10^6),
             bases_per_sample=bases_per_lane/samples_per_pool,
             reads_per_sample_1x=(v_size*10^9)/(read_length*v_rtm),
             reads_per_sample=reads_per_lane/samples_per_pool,
             coverage_ba=bases_per_sample/(v_size*10^9),
             coverage_rd=(reads_per_sample*10^6)/reads_per_sample_1x,
             coverage_lw=(reads_per_sample*v_rtm*read_length*10^6)/(v_size*10^9),
             cost_per_sample=cost_per_lane/samples_per_pool,
             cost_total=cost_per_sample*input$in_num_samples)
    
    # add min unit limit
    # Allowed protocols are T and disabled protocols are F
    dfr_ins$min_unit_limit <- ifelse(dfr_ins$min_unit=="Lane",T,ifelse((dfr_ins$min_unit=="Flow cell") & (num_of_pools >= dfr_ins$lanes) & (num_of_pools %% dfr_ins$lanes == 0),T,F))
    
    return(dfr_ins)
  })
  
  # RFN: fn_ins -----------------------------------------------------------
  # filters out mul
  
  fn_ins <- reactive({
    shiny::req(fn_ins_mul())
    
    # get dfr_ins
    dfr_ins <- fn_ins_mul() %>% dplyr::filter(min_unit_limit==TRUE)
    dfr_ins$min_unit_limit <- NULL
    
    if(nrow(dfr_ins)<1) stop("Empty dataframe.")
    return(dfr_ins)
  })
  
  # OBS ---------------------------------------------------------------------
  # Updates dropdown widget to reflect min unit limit
  
  observeEvent(fn_ins_mul(),{
    
    choices_protocol_opts1 <- ifelse(fn_ins_mul()$min_unit_limit,choices_protocol_opts,gsub("background: #[A-Z0-9]+","background: #DCDCDC",choices_protocol_opts))
    updatePickerInput(session,"in_protocol","Protocol",choices=choices_protocol,
                      selected=fn_ins_mul()[fn_ins_mul()$min_unit_limit==T,]$protocol[1],
                      choicesOpt=list(disabled=!fn_ins_mul()$min_unit_limit,
                                      content=choices_protocol_opts1))
  })
  
  # OUT: out_mul -----------------------------------------------
  # displays number of protocols removed due to min unit limit
  
  output$out_mul <- renderText({
    shiny::req(fn_ins_mul())
    
    n <- sum(!fn_ins_mul()$min_unit_limit)
    
    if(n>0)
    {
      return(paste0("<small><b>Note:</b> ",n," protocol",ifelse(n>1,"s","")," not displayed due to minimum unit limit. See guide.</small>"))
    }else{
      return("")
    }
  })
  
  # FN: fn_table_wide_colnames -------------------------------------------------
  # column names for the wide table
  # this is used in many places and editing it here in one place
  # makes it easy to update
  
  fn_table_wide_colnames <- function(){
    
    dfr <- data.frame(Header=c("Instrument","Min Unit","Lanes","Read Type",
                               "Read Length",
                               "Reads Per Sample (Millions)",
                               "Coverage (Lander-Waterman) X",
                               "Cost Per Lane (USD)",
                               "Cost Per Sample (USD)",
                               "Total Cost (USD)"),
                      Description=c("Sequencing instrument",
                                    "Minimum units required for a sequencing run",
                                    "Number of lanes",
                                    "Read type (single-end/paired-end)",
                                    "Read length (base pairs)",
                                    "Number of reads per sample (millions)",
                                    "Lander-Waterman coverage (X)",
                                    "Cost per lane (USD)",
                                    "Cost per sample (USD)",
                                    "Total sequencing cost for all samples (USD)"),
                      stringsAsFactors=F)
    return(dfr)
  }
  
  # RFN: fn_table_wide -----------------------------------------------------------
  # formats wide table and creates formattable
  
  fn_table_wide <- reactive({
    shiny::req(fn_ins_mul())
    
    # reorder columns and round values
    v_dfr_otw <- fn_ins_mul() %>% 
      select(ins,min_unit,lanes,read_type,read_length,
             -reads_per_lane,
             reads_per_sample,
             coverage_lw,
             cost_per_lane,
             cost_per_sample,
             cost_total,
             min_unit_limit) %>%
      mutate(read_type=toupper(read_type),
             reads_per_sample=round(reads_per_sample,0),
             coverage_lw=round(coverage_lw,0),
             cost_per_sample=ceiling(cost_per_sample),
             cost_per_lane=ceiling(cost_per_lane),
             cost_total=ceiling(cost_total))
    
    # rename headers
    colnames(v_dfr_otw) <- c(fn_table_wide_colnames()$Header,"min_unit_limit")
    
    v_dfr_otw <- v_dfr_otw[v_dfr_otw$min_unit_limit==TRUE,]
    v_dfr_otw$min_unit_limit <- NULL
    
    col_ins2 <- col_ins[names(col_ins) %in% sort(unique(v_dfr_otw$Instrument))]
    col_read_type2 <- col_read_type[names(col_read_type) %in% sort(unique(v_dfr_otw$`Read Type`))]
    col_read_length2 <- col_read_length[names(col_read_length) %in% sort(unique(v_dfr_otw$`Read Length`))]
    col_min_unit2 <- col_min_unit[names(col_min_unit) %in% sort(unique(v_dfr_otw$`Min Unit`))]
    
    color_bar_ins <- formatter("span",style=function(x) style(
      color="white",font.weight="bold",
      border.radius="4px",padding.left="3px",padding.right="3px",
      background=col_ins2[factor(v_dfr_otw$Instrument)]))
    color_bar_read_type <- formatter("span",style=function(x) style(
      color="white",font.weight="bold",
      border.radius="4px",padding.left="3px",padding.right="3px",
      background=col_read_type2[factor(v_dfr_otw$`Read Type`)]))
    color_bar_read_length <- formatter("span",style=function(x) style(
      color="white",font.weight="bold",
      border.radius="4px",padding.left="3px",padding.right="3px",
      background=col_read_length2[factor(v_dfr_otw$`Read Length`)]))
    color_bar_min_unit <- formatter("span",style=function(x) style(
      color="white",font.weight="bold",
      border.radius="4px",padding.left="3px",padding.right="3px",
      background=col_min_unit2[factor(v_dfr_otw$`Min Unit`)]))
    
    v_dfr_otw <- formattable(v_dfr_otw,list(
      "Instrument"=color_bar_ins,
      "Read Type"=color_bar_read_type,
      "Read Length"=color_bar_read_length,
      "Min Unit"=color_bar_min_unit,
      "Reads Per Sample (Millions)"=color_tile("#dfecbb","#95c11e"),
      "Coverage (Lander-Waterman) X"=color_tile("#dfecbb","#95c11e"),
      "Total Cost (USD)"=color_tile("#dfecbb","#95c11e")))
    
    return(v_dfr_otw)
  })
  
  # RFN: fn_dt_wide -----------------------------------------------------------
  # converts formattable to dt and adds number formatting
  
  fn_dt_wide <- reactive({
    shiny::req(fn_table_wide())
  
    tmp <- formattable::as.datatable(fn_table_wide(),
      selection="none",
      class="stripe hover",
      style="bootstrap",
      options=list(
        dom="ft",
        paging=FALSE,
        searchHighlight=TRUE,
        autoWidth=FALSE,
        columnDefs=list(list(className="dt-right",targets=c(8:10)),
                        list(className="dt-center",targets=c(3:7)),
                        list(className="dt-left",targets=c(1:2)),
                        list(width="160px",targets=1))),
      callback = JS(paste0("var tips=['','",paste(fn_table_wide_colnames()$Description,collapse="','"),"'],
                           header=table.columns().header();
                           for (var i=0; i<tips.length; i++) {
                           $(header[i]).attr('title', tips[i]);
                           };"))
    )
    
    tmpnum <- as.integer(stringr::str_replace_all(stringr::str_extract(tmp$x$data$`Total Cost (USD)`,">[0-9]+<"),">|<",""))
    tmp$x$data$`Total Cost (USD)` <- stringr::str_replace_all(tmp$x$data$`Total Cost (USD)`,
                                                              ">[0-9]+<",paste0(">",prettyNum(tmpnum,
                                                                                              big.mark="<span style='padding:3px;'>",
                                                                                              big.interval=3,scientific=F),"<"))
    
    tmp$x$data$`Cost Per Sample (USD)` <- prettyNum(tmp$x$data$`Cost Per Sample (USD)`,
                                                    big.mark="<span style='padding:3px;'>",
                                                    big.interval=3,scientific=F)
    
    tmp$x$data$`Cost Per Lane (USD)` <- prettyNum(tmp$x$data$`Cost Per Lane (USD)`,
                                                  big.mark="<span style='padding:3px;'>",
                                                  big.interval=3,scientific=F)
    return(tmp)
  })
  
  # OUT: out_table_wide -----------------------------------------------------
  # exports wide table
  
  output$out_table_wide_dt <- DT::renderDT(
    fn_dt_wide()
  )

  # UI: ui_report ------------------------------------------------------------
  # conditional ui for button to generate report
  
  output$ui_report <- renderUI({
    shiny::req(fn_table_long())
    
    downloadButton("btn_report","Download Report")
  })
  
  # RFN: fn_compare_data -------------------------------------------------------
  # reactive function to generate data for compare protocols plot
  
  fn_compare_data <- reactive({
    shiny::req(fn_ins())
    shiny::req(fn_oz())
    
    v_size <- fn_oz()
    dfr_ins <- fn_ins()
    n <- nrow(dfr_ins)
    ap_list <- vector("list",length=n)
    for(i in 1:n)
    {
      v_rtm <- ifelse(dfr_ins$read_type[i]=="se",1,2)
      dfr_ap <- data.frame(samples_per_lane=1:input$in_num_samples,protocol=dfr_ins$protocol[i],stringsAsFactors=F)
      dfr_ap <- dfr_ap %>% mutate(reads_per_sample=dfr_ins$reads_per_lane[i]/samples_per_lane,
                                  lanes_req=input$in_num_samples/samples_per_lane,
                                  lane_usage=paste0(samples_per_lane,"x",floor(input$in_num_samples/samples_per_lane),"L",ifelse(input$in_num_samples %% samples_per_lane > 0,paste0("+",input$in_num_samples %% samples_per_lane),"")),
                                  coverage=(dfr_ins$read_length[i]*v_rtm*reads_per_sample*(10^6))/(v_size*(10^9)),
                                  cost_per_sample=dfr_ins$cost_per_lane[i]/samples_per_lane,
                                  cost_total=cost_per_sample*samples_per_lane*lanes_req)
      ap_list[[i]] <- dfr_ap
    }
    
    dfr <- bind_rows(ap_list) %>%
      mutate(reads_per_sample=round(reads_per_sample,0),
             lanes_req=ceiling(lanes_req),
             coverage=round(coverage,0),
             cost_per_sample=ceiling(cost_per_sample),
             cost_total=ceiling(cost_total),
             popup=paste('<br>Protocol: ',protocol,
                         '<br>Samples Per Lane: ',samples_per_lane,
                         '<br>Reads Per Sample: ',reads_per_sample,
                         '<br>Lanes req: ',lanes_req,
                         '<br>Lane usage: ',lane_usage,
                         '<br>Coverage: ',coverage,
                         '<br>Cost Per Sample: ',cost_per_sample,
                         '<br>Total Cost: ',cost_total)) %>%
      select(protocol,samples_per_lane,reads_per_sample,coverage,cost_per_sample,cost_total,popup) %>%
      gather(key=metric,value=value,-samples_per_lane,-popup,-protocol) %>%
      arrange(metric) %>%
      mutate(metric=factor(as.character(metric))) %>%
      mutate(ylab=plyr::mapvalues(metric,from=c("cost_per_sample","cost_total",
                                                "coverage","reads_per_sample"),
                                  to=c("Cost Per Sample (USD)","Cost Total (USD)",
                                       "Coverage (X)","Reads Per Sample (Millions)")))
    
    rtlist <- split(dfr,dfr$metric)
    names(rtlist) <- levels(dfr$metric)
    return(rtlist)
  })
  
  # FN: fn_compare_plot --------------------------------------------------------
  # highchart plot function
  
  fn_compare_plot <- function(dfr){
    
    hc <- dfr %>%
      hchart(.,"line",hcaes(x="samples_per_lane",y="value",group="protocol")) %>%
      hc_xAxis(title=list(text="Samples Per Lane"),type="category") %>%
      hc_yAxis(title=list(text=dfr$ylab[1])) %>%
      #hc_yAxis_multiples(create_yaxis(5)) %>%
      hc_chart(zoomType='xy',panKey="shift",panning=TRUE) %>%
      #hc_tooltip(shared=TRUE) %>%
      hc_exporting(enabled=TRUE,url="https://export.highcharts.com",
                   fallbackToExportServer=TRUE,buttons=list(
                     contextButton=list(align='right',verticalAlign='top'))) %>%
      hc_size(height=400)
    #hc_tooltip(borderWidth=1,followPointer=TRUE,followTouchMove=TRUE,shared=TRUE,
    #           headerFormat="",pointFormat="{point.popup}")
    
    return(column(width=12,hc))
  }
  
  # OUT: out_plot_container ----------------------------------------------------
  # compare plot container
  
  output$out_plot_container <- renderUI({
    shiny::req(fn_compare_data())
    
    charts <- lapply(fn_compare_data(),fn_compare_plot)
    do.call(tagList,charts)
  })
  
  # RFN: fn_long ----------------------------------------------------------
  # computes depth-cost table
  
  fn_long <- reactive({
    shiny::req(fn_ins())
    shiny::req(fn_oz())
    shiny::req(input$in_protocol)
    
    v_size <- fn_oz()
    v_in_protocol <- input$in_protocol
    v_dfr_long <- fn_ins() %>% dplyr::filter(protocol==get("v_in_protocol"))
    v_in_read_type <- v_dfr_long$read_type
    v_in_read_length <- v_dfr_long$read_length
    v_rtm <- ifelse(v_dfr_long$read_type=="se",1,2)
    
    validate(fn_validate_equal(nrow(v_dfr_long)==0,FALSE,"Number of rows of filtered ins table is zero."))
    validate(fn_validate_equal(nrow(v_dfr_long)>1,FALSE,"Number of rows of filtered ins table >1."))

    dfr_long <- data.frame(samples_per_lane=1:input$in_num_samples,stringsAsFactors=F)
    
    dfr_long <- dfr_long %>% mutate(reads_per_sample=v_dfr_long$reads_per_lane/samples_per_lane,
                                    lanes_req=input$in_num_samples/samples_per_lane,
                                    lane_usage=paste0(samples_per_lane,"x",floor(input$in_num_samples/samples_per_lane),"L",ifelse(input$in_num_samples %% samples_per_lane > 0,paste0("+",input$in_num_samples %% samples_per_lane),"")),
                                    coverage=(v_in_read_length*v_rtm*reads_per_sample*(10^6))/(v_size*(10^9)),
                                    cost_per_sample=v_dfr_long$cost_per_lane/samples_per_lane,
                                    cost_total=cost_per_sample*samples_per_lane*lanes_req)
    
    if(input$in_multiple){
      pos <- dfr_long$samples_per_lane[(input$in_num_samples %% dfr_long$samples_per_lane) == 0]
      dfr_long <- dfr_long[pos,]
    }
    
    return(dfr_long)
  })
  
  # FN: fn_table_long_colnames -------------------------------------------------
  # column names for the long table
  # this is used in many places and editing it here in one place
  # makes it easy to update
  
  fn_table_long_colnames <- function(){
    
    dfr <- data.frame(Header=c("Samples Per Lane","Reads Per Sample (Millions)",
                               "Lanes Required","Lane Usage","Coverage",
                               "Cost Per Sample (USD)","Total Cost (USD)"),
                      Description=c("Number of samples pooled per lane",
                                    "Number of reads per sample (millions)",
                                    "Number of lanes required",
                                    "Exact usage of lanes",
                                    "Lander-Waterman coverage (X)",
                                    "Cost per sample (USD)",
                                    "Total sequencing cost for all samples (USD)"),
                      stringsAsFactors=F)
    return(dfr)
  }
  
  # RFN: fn_table_long ----------------------------------------------------------
  # formats depth-cost table
  
  fn_table_long <- reactive({
    shiny::req(fn_long())
    
    dfr_long <- fn_long() %>%
      mutate(reads_per_sample=round(reads_per_sample,0),
             lanes_req=ceiling(lanes_req),
             coverage=round(coverage,0),
             cost_per_sample=ceiling(cost_per_sample),
             cost_total=ceiling(cost_total))
    
    colnames(dfr_long) <- fn_table_long_colnames()$Header
    
    dfr_long <- formattable(dfr_long,list(
      "Reads Per Sample (Millions)"=color_tile("#dfecbb","#95c11e"),
      "Coverage"=color_tile("#dfecbb","#95c11e"),
      "Total Cost (USD)"=color_tile("#dfecbb","#95c11e")))
    
    return(dfr_long)
  })
  
  # RFN: fn_dt_long -----------------------------------------------------------
  # converts formattable to dt and adds number formatting
  
  fn_dt_long <- reactive({
    shiny::req(fn_table_long())
    
    tmp <- formattable::as.datatable(fn_table_long(),
                        selection="none",
                        class="stripe hover",
                        style="bootstrap",
                        options=list(
                          dom="ft",
                          paging=FALSE,
                          searchHighlight=TRUE,
                          autoWidth=FALSE,
                          columnDefs=list(list(className="dt-center",targets=c(1:5)),
                                          list(className="dt-right",targets=c(6:7)))),
                        callback = JS(paste0("var tipslong=['','",paste(fn_table_long_colnames()$Description,collapse="','"),"'],
                                  header=table.columns().header();
                                  for (var i=0; i<tipslong.length; i++) {
                                  $(header[i]).attr('title', tipslong[i]);
                                  }")))
    
    tmpnum <- as.integer(stringr::str_replace_all(stringr::str_extract(tmp$x$data$`Total Cost (USD)`,">[0-9]+<"),">|<",""))
    tmp$x$data$`Total Cost (USD)` <- stringr::str_replace_all(tmp$x$data$`Total Cost (USD)`,
                                                              ">[0-9]+<",paste0(">",prettyNum(tmpnum,
                                                                                              big.mark="<span style='padding:3px;'>",
                                                                                              big.interval=3,scientific=F),"<"))
    tmp$x$data$`Cost Per Sample (USD)` <- prettyNum(tmp$x$data$`Cost Per Sample (USD)`,
                                                    big.mark="<span style='padding:3px;'>",
                                                    big.interval=3,scientific=F)
    return(tmp)
  })
  
  # OUT: out_table_long -----------------------------------------------------
  # exports long table
  
  output$out_table_long<- DT::renderDT({
    fn_dt_long()
  })
  
  # OUT: out_plot -----------------------------------------------------------
  # displays interactive plot
  
  output$out_plot <- renderHighchart({
    shiny::req(fn_long())
    
    dfr_long <- fn_long() %>%
      mutate(reads_per_sample=round(reads_per_sample,0),
             lanes_req=ceiling(lanes_req),
             coverage=round(coverage,0),
             cost_per_sample=ceiling(cost_per_sample),
             cost_total=ceiling(cost_total),
             popup=paste('<br>Samples Per Lane: ',samples_per_lane,
                         '<br>Reads Per Sample: ',reads_per_sample,
                         '<br>Lanes req: ',lanes_req,
                         '<br>Lane usage: ',lane_usage,
                         '<br>Coverage: ',coverage,
                         '<br>Cost Per Sample: ',cost_per_sample,
                         '<br>Total Cost: ',cost_total)) %>%
      select(samples_per_lane,reads_per_sample,lanes_req,coverage,cost_per_sample,cost_total,popup) %>%
      gather(key=metric,value=value,-samples_per_lane,-popup)
    
    # plot_ly(dfr_long,x=~samples_per_lane,y=~cost_total,height=500,
    #         hoverinfo='text',text=~paste('</br>Samples Per Lane: ',samples_per_lane,
    #                                      '</br>Reads Per Sample: ',reads_per_sample,
    #                                      '</br>Lanes req: ',lanes_req,
    #                                      '</br>Lane usage: ',lane_usage,
    #                                      '</br>Coverage: ',coverage,
    #                                      '</br>Cost Per Sample: ',cost_per_sample,
    #                                      '</br>Total Cost: ',cost_total)) %>%
    #   add_markers(color=~coverage,size=~reads_per_sample,marker=list(sizeref=0.1)) %>%
    #   layout(xaxis=list(title="Samples Per Lane"),yaxis=list(title="Total Cost (USD)"))
    
    #saveRDS(dfr_long,"temp-long.Rds")
    #dfr_long <- readRDS("temp-long.Rds")
    
    hc <- dfr_long %>%
      hchart(.,"column",hcaes(x="samples_per_lane",y="value",group="metric"),yAxis=c(0,1,2,3,4)) %>%
      hc_xAxis(title=list(text="Samples Per Lane")) %>%
      #hc_yAxis(title=list(text=NULL)) %>%
      hc_yAxis_multiples(create_yaxis(5)) %>%
      hc_chart(zoomType='x',panKey="shift",panning=TRUE) %>%
      hc_tooltip(shared=TRUE) %>%
      hc_exporting(enabled=TRUE,url="https://export.highcharts.com",
                   fallbackToExportServer=TRUE,buttons=list(
                     contextButton=list(align='right',verticalAlign='top')))
    #hc_size(height=940,width=800)
    #hc_tooltip(borderWidth=1,followPointer=TRUE,followTouchMove=TRUE,shared=TRUE,
    #           headerFormat="",pointFormat="{point.popup}")
    
    hc
  })
  
  # UI: ui_pa ---------------------------------------------------------------
  # conditional ui for power analysis
  
  output$ui_pa <- renderUI({
    #shiny::req(input$in_pa_est)

    if(input$in_pa_est=="n") {
      div(
        textInput("in_pa_depth","Sequencing depth",value="4"),
        shinyBS::bsTooltip("in_pa_depth",title="Number of reads mapped to a feature. Usually a value between 5-20.",placement="top",trigger="hover"),
        textInput("in_pa_cv","Coefficient of variation",value="0.4"),
        shinyBS::bsTooltip("in_pa_cv",title="Biological coefficient of variation between replicates within a group. A value between 0-1.",placement="top",trigger="hover"),
        textInput("in_pa_effect","Effect",value="1.25,1.5,1.75,2"),
        shinyBS::bsTooltip("in_pa_effect",title="Target effect size. Like fold-change. Usually values like 0.25, 0.5, 1, 1.25, 2 etc.",placement="top",trigger="hover"),
        textInput("in_pa_alpha","Alpha",value="0.05"),
        shinyBS::bsTooltip("in_pa_alpha",title="The false positive rate. A value between 0 and 1.",placement="top",trigger="hover"),
        textInput("in_pa_power","Power",value="0.8,0.9"),
        shinyBS::bsTooltip("in_pa_power",title="The fraction of true positives to be detected. A value between 0 and 1.",placement="top",trigger="hover")
      )
    }else if(input$in_pa_est=="effect") {
      div(
        textInput("in_pa_depth","Sequencing depth",value="4"),
        shinyBS::bsTooltip("in_pa_depth",title="Number of reads mapped to a feature. Usually a value between 5-20.",placement="top",trigger="hover"),
        textInput("in_pa_n","Number of samples",value="3"),
        shinyBS::bsTooltip("in_pa_n",title="The number of samples per group.",placement="top",trigger="hover"),
        textInput("in_pa_cv","Coefficient of variation",value="0.4"),
        shinyBS::bsTooltip("in_pa_cv",title="Biological coefficient of variation between replicates within a group. A value between 0-1.",placement="top",trigger="hover"),
        textInput("in_pa_alpha","Alpha",value="0.05"),
        shinyBS::bsTooltip("in_pa_alpha",title="The false positive rate. A value between 0 and 1.",placement="top",trigger="hover"),
        textInput("in_pa_power","Power",value="0.8,0.9"),
        shinyBS::bsTooltip("in_pa_power",title="The fraction of true positives to be detected. A value between 0 and 1.",placement="top",trigger="hover")
      )
    } else if(input$in_pa_est=="alpha") {
      div(
        textInput("in_pa_depth","Sequencing depth",value="4"),
        shinyBS::bsTooltip("in_pa_depth",title="Number of reads mapped to a feature. Usually a value between 5-20.",placement="top",trigger="hover"),
        textInput("in_pa_n","Number of samples",value="3"),
        shinyBS::bsTooltip("in_pa_n",title="The number of samples per group.",placement="top",trigger="hover"),
        textInput("in_pa_cv","Coefficient of variation",value="0.4"),
        shinyBS::bsTooltip("in_pa_cv",title="Biological coefficient of variation between replicates within a group. A value between 0-1.",placement="top",trigger="hover"),
        textInput("in_pa_effect","Effect",value="1.25,1.5,1.75,2"),
        shinyBS::bsTooltip("in_pa_effect",title="Target effect size. Like fold-change. Usually values like 0.25, 0.5, 1, 1.25, 2 etc.",placement="top",trigger="hover"),
        textInput("in_pa_power","Power",value="0.8,0.9"),
        shinyBS::bsTooltip("in_pa_power",title="The fraction of true positives to be detected. A value between 0 and 1.",placement="top",trigger="hover")
      )
    } else if(input$in_pa_est=="cv") {
      div(
        textInput("in_pa_depth","Sequencing depth",value="4"),
        textInput("in_pa_n","Number of samples",value="3"),
        shinyBS::bsTooltip("in_pa_n",title="The number of samples per group.",placement="top",trigger="hover"),
        textInput("in_pa_effect","Effect",value="1.25,1.5,1.75,2"),
        shinyBS::bsTooltip("in_pa_effect",title="Target effect size. Like fold-change. Usually values like 0.25, 0.5, 1, 1.25, 2 etc.",placement="top",trigger="hover"),
        textInput("in_pa_alpha","Alpha",value="0.05"),
        shinyBS::bsTooltip("in_pa_alpha",title="The false positive rate. A value between 0 and 1.",placement="top",trigger="hover"),
        textInput("in_pa_power","Power",value="0.8,0.9"),
        shinyBS::bsTooltip("in_pa_power",title="The fraction of true positives to be detected. A value between 0 and 1.",placement="top",trigger="hover")
      )
    } else if(input$in_pa_est=="power") {
      div(
        textInput("in_pa_depth","Sequencing depth",value="4"),
        shinyBS::bsTooltip("in_pa_depth",title="Number of reads mapped to a feature. Usually a value between 5-20.",placement="top",trigger="hover"),
        textInput("in_pa_n","Number of samples",value="3"),
        shinyBS::bsTooltip("in_pa_n",title="The number of samples per group.",placement="top",trigger="hover"),
        textInput("in_pa_cv","Coefficient of variation",value="0.4"),
        shinyBS::bsTooltip("in_pa_cv",title="Biological coefficient of variation between replicates within a group. A value between 0-1.",placement="top",trigger="hover"),
        textInput("in_pa_effect","Effect",value="1.25,1.5,1.75,2"),
        shinyBS::bsTooltip("in_pa_effect",title="Target effect size. Like fold-change. Usually values like 0.25, 0.5, 1, 1.25, 2 etc.",placement="top",trigger="hover"),
        textInput("in_pa_alpha","Alpha",value="0.05"),
        shinyBS::bsTooltip("in_pa_alpha",title="The false positive rate. A value between 0 and 1.",placement="top",trigger="hover")
      )
    }
    
  })
  
  # OUT: out_pa_label ----------------------------------------------------------
  # label for power analysis
  
  output$out_pa_label <- renderText({
    shiny::req(input$in_pa_est)
    
    labeller <- function(type) {
      switch(type,
             n="Number of Samples",
             cv="Coefficient of Variation",
             effect="Relative Expression Effect",
             alpha="False Positive Rate",
             power="Power")
    }
    
    txt <- labeller(input$in_pa_est)
    paste0("Estimated <b>",txt,"</b>")
  })
  
  # OUT: out_pa ----------------------------------------------------------------
  # print output for power analysis
  
  output$out_pa <- renderPrint({
    #shiny::req(input$in_pa_est)
    
    tryCatch({
    depth <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_depth),",")))
    
    if(input$in_pa_est=="n") {
      cv <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_cv),",")))
      effect <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_effect),",")))
      alpha <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_alpha),",")))
      power <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_power),",")))
      
      validate(fn_validate_equal(any(cv>1|cv<0),FALSE,"Coefficient of variation must be between 0-1."))
      validate(fn_validate_equal(any(alpha>1|alpha<0),FALSE,"Alpha must be between 0-1."))
      validate(fn_validate_equal(any(power>1|power<0),FALSE,"Power must be between 0-1."))
      
      RNASeqPower::rnapower(depth=depth,cv=cv,effect=effect,alpha=alpha,power=power)
    }else if(input$in_pa_est=="effect") {
      n <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_n),",")))
      cv <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_cv),",")))
      alpha <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_alpha),",")))
      power <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_power),",")))
      
      validate(fn_validate_equal(any(cv>1|cv<0),FALSE,"Coefficient of variation must be between 0-1."))
      validate(fn_validate_equal(any(alpha>1|alpha<0),FALSE,"Alpha must be between 0-1."))
      validate(fn_validate_equal(any(power>1|power<0),FALSE,"Power must be between 0-1."))
      
      RNASeqPower::rnapower(depth=depth,n=n,cv=cv,alpha=alpha,power=power)
    }else if(input$in_pa_est=="alpha") {
      n <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_n),",")))
      cv <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_cv),",")))
      effect <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_effect),",")))
      power <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_power),",")))
      
      validate(fn_validate_equal(any(cv>1|cv<0),FALSE,"Coefficient of variation must be between 0-1."))
      validate(fn_validate_equal(any(power>1|power<0),FALSE,"Power must be between 0-1."))
      
      RNASeqPower::rnapower(depth=depth,n=n,cv=cv,effect=effect,power=power)
    }else if(input$in_pa_est=="cv") {
      n <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_n),",")))
      effect <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_effect),",")))
      alpha <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_alpha),",")))
      power <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_power),",")))

      validate(fn_validate_equal(any(alpha>1|alpha<0),FALSE,"Alpha must be between 0-1."))
      validate(fn_validate_equal(any(power>1|power<0),FALSE,"Power must be between 0-1."))
      
      RNASeqPower::rnapower(depth=depth,n=n,effect=effect,alpha=alpha,power=power)
    } else if(input$in_pa_est=="power") {
      n <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_n),",")))
      cv <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_cv),",")))
      effect <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_effect),",")))
      alpha <- as.numeric(unlist(strsplit(gsub(" ","",input$in_pa_alpha),",")))
      
      validate(fn_validate_equal(any(cv>1|cv<0),FALSE,"Coefficient of variation must be between 0-1."))
      validate(fn_validate_equal(any(alpha>1|alpha<0),FALSE,"Alpha must be between 0-1."))
      
      RNASeqPower::rnapower(depth=depth,n=n,cv=cv,effect=effect,alpha=alpha)
    }
    }, error=function(e) {
      validate(fn_validate_equal(T,F,"Power analysis error. Check if input values and/or delimiters are correct. All input must be numeric. Coefficient of variation, Alpha and Power must be values between 0 and 1. If error persists, contact us."))
    })
  })
  
  # OUT: out_display -----------------------------------------------------------
  # debug display print output
  
  #output$out_display <- renderPrint({
    
    # uncomment below to see currently active variables
    # if(tolower(input$in_select_gsize)=="from list")
    # {
    #   req(fn_org())
    #   v_dfr <- fn_org()
    #   v_label <- v_dfr$label
    #   v_name <- v_dfr$name
    #   v_sci_name <- v_dfr$sci_name
    #   v_ome <- v_dfr$ome
    #   v_size <- v_dfr$size_gb
    #   v_size_custom <- NA
    # }else{
    #   req(input$in_size)
    #   v_size_custom <- input$in_size
    #   v_label <- NA
    #   v_name <- NA
    #   v_sci_name <- NA
    #   v_ome <- NA
    #   v_size <- NA
    # }
    # 
    # cat(paste0("COMMON VARIABLES\n",
    #            "Size selection: ",input$in_select_gsize,"\n",
    #            "Custom Size (Gbp): ",v_size_custom,"\n",
    #            "Label: ",v_label,"\n",
    #            "Common name: ",v_name,"\n",
    #            "Sci name: ",v_sci_name,"\n",
    #            "Genome type: ",v_ome,"\n",
    #            "Size (Gbp): ",v_size,"\n",
    #            "Num samples: ",input$in_num_samples,"\n",
    #            "\nOVERVIEW VARIABLES\n",
    #            "Num pools: ",input$in_num_pools,"\n",
    #            "\nDEPTH-COST VARIABLES\n",
    #            "Instrument: ",input$in_ins,"\n",
    #            "Read type: ",input$in_read_type,"\n",
    #            "Read length: ",input$in_read_length))
    
    # uncomment below to see report variables
   # params <- list(
  #    label=input$in_label,
  #    ome=input$in_ome,
  #    size=fn_oz(),
  #    num_samples=input$in_num_samples,
  #    num_pools=input$in_num_pools,
  #    samples_per_pool=fn_spp(),
  #    protocol=input$in_protocol,
  #    table_wide=fn_ins(),
  #    table_long=fn_long(),
  #    appversion=fn_version()$appversion,
  #    datadate=fnv$datadate)
    
  #  print(str(params))
  #})
  
  # DHL report ---------------------------------------------------------------
  # download handler for report
  
  output$btn_report <- downloadHandler(
    
    filename = "report.html",
    content = function(file) {
      
      if(tolower(input$in_criteria)=="set number of pools")
      {
        samples_per_pool <- fn_spp()
        num_of_pools <- input$in_criteria_value
      }else{
        samples_per_pool <- input$in_criteria_value
        num_of_pools <- fn_spp()
      }
      
      tdir <- tempdir()
      path_report <- file.path(tdir,"report.Rmd")
      file.copy(from="./report.Rmd",to=path_report,overwrite=TRUE)
      file.copy(from="./www/styles.css",tdir,overwrite=TRUE)
      file.copy(from="./www/logo.svg",tdir,overwrite=TRUE)
      
      # params
      params <- list(
        label=input$in_label,
        ome=input$in_ome,
        size=fn_oz(),
        num_samples=input$in_num_samples,
        num_pools=num_of_pools,
        samples_per_pool=samples_per_pool,
        protocol=input$in_protocol,
        table_wide=fn_table_wide(),
        table_long=fn_table_long(),
        table_long_raw=fn_long(),
        table_wide_colnames=fn_table_wide_colnames(),
        table_long_colnames=fn_table_long_colnames(),
        appversion=fn_version()$appversion,
        datadate=fnv$datadate)
      
      # knit document
      rmarkdown::render(path_report,output_file=file,
                        params=params,envir=new.env(parent=globalenv()))
    })
})
