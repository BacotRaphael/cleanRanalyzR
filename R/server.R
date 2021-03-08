server <- function(input, output, session) {
  shinyjs::disable("dwclog")
  shinyjs::disable("dwclean")
  shinyjs::disable("dwres")
  shinyjs::disable("dwconv")
  shinyjs::disable("dwdap")
  shinyjs::disable("runchecks")
  shinyjs::disable("runclean")
  shinyjs::disable("runres")
  shinyjs::disable("runconv")
  shinyjs::disable("rundap")
  output$ui.db<-renderUI({
    if(input$operation%!in%c(NULL,"","makedap")){
      shiny::fileInput("data", "DATASET/ DONNEES (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.questionnaire<-renderUI({
    if(input$operation%!in%c(NULL,"")){
      shiny::fileInput("questionnaire", "EXCEL workbook Questionnaire", accept = c(".xlsx",".xls"))
    }
  })
  output$ui.choiceslabel<-renderUI({
    req(input$questionnaire)
      selectInput("choiceslabel","Select Choices Label column", choices = names(choices()))
    
  })
  output$ui.surveylabel<-renderUI({
    req(input$questionnaire)
    selectInput("surveylabel","Select Survey Label column", choices = names(survey()))
    
  })
  output$ui.clog<-renderUI({
    if(input$operation=="clean"){
      shiny::fileInput("clog", "Cleaning LOG (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.dap<-renderUI({
    if(input$operation=="analysis"){
      shiny::fileInput("dap", "Data Analysis PLAN (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.sf<-renderUI({
    req(input$sampling)
    if(input$sampling=="weighted"){
      shiny::fileInput("sf", "Sampling Frame (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.clogdef<-renderUI({
    if(input$operation=="clog"){
      shiny::fileInput("clogdef", "CleaningLog definition (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.cloglabel<-renderUI({
    if(input$operation=="clog"){
      shiny::selectInput('cloglabel', "Do you want to label question/answers in the cleaning LOG", choices = setNames(c("no","yes"),c("No","Yes")))
    }
  })
  output$ui.sampling<-renderUI({
    if(input$operation=="analysis"){
      shiny::selectizeInput(
        'sampling', "Weighted or non weighted analysis", choices = setNames(c("nonweighted","weighted"),c("No Weights","Use weights"))
      )
    }
  })
  output$ui.cluster<-renderUI({
    req(input$sampling)
    if(input$sampling=="weighted"){
      selectInput("cluster","Are you using a Cluster sampling?", choices = setNames(c("no","yes"),c("No","Yes")))
      # shiny::selectizeInput(
      #   'cluster', "Are you using a Cluster sampling?", choices = setNames(c("yes","no"),c("Yes","No")),
      #   options = list(
      #     placeholder = 'Please choose one of the following options',
      #     onInitialize = I('function() { this.setValue(""); }')
      #   )
      # )
    }
  })
  output$ui.data.stratum.column<-renderUI({
    req(input$sampling)
    if(input$sampling=="weighted"){
      shiny::selectizeInput(
        'data.stratum.column', "data stratum column", choices = names(db()),
        options = list(
          placeholder = 'Please choose one of the following options',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })
  output$ui.sampling.frame.population.column<-renderUI({
    req(input$sampling)
    if(input$sampling=="weighted"){
      shiny::selectizeInput(
        'sampling.frame.population.column', "sampling frame population column", choices = names(sf()),
        options = list(
          placeholder = 'Please choose one of the following options',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })
  output$ui.sampling.frame.stratum.column<-renderUI({
    req(input$sampling)
    if(input$sampling=="weighted"){
      shiny::selectizeInput(
        'sampling.frame.stratum.column', "sampling frame stratum column", choices = names(sf()),
        options = list(
          placeholder = 'Please choose one of the following options',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })
  output$ui.cl<-renderUI({
    # req(input$sampling)
    # if(input$sampling=="weighted"){}
    if(input$operation=="analysis"){
      shiny::numericInput("cl", "Confidence LEVEL [0-1]", value = 0.95, min = 0,max = 1)
    }
  })
  output$ui.cluster_variable_name<-renderUI({
    req(input$cluster)
    if(input$cluster=="yes"){
      shiny::selectizeInput(
        'cluster_variable_name', "cluster variable name", choices = names(db()),
        options = list(
          placeholder = 'Please choose one of the following options',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })
  db <- shiny::reactive({
    shiny::req(input$data)
    if(input$operation=="label_toxml"){
      db<-load_file(input$data$name,input$data$datapath) %>% prepdata(.,F)
    } else {
      db<-load_file(input$data$name,input$data$datapath) %>% prepdata(.,T)
    }
  })
  survey <- shiny::reactive({
    shiny::req(input$questionnaire)
    df<-readxl::read_excel(input$questionnaire$datapath,"survey",col_types = "text")
  })
  choices <- shiny::reactive({
    shiny::req(input$questionnaire)
    df<-readxl::read_excel(input$questionnaire$datapath,"choices",col_types = "text")
  })
  dap <- shiny::reactive({
    shiny::req(input$dap)
    load_file(input$dap$name,input$dap$datapath)
  })
  sf <- shiny::reactive({
    shiny::req(input$sf)
    load_file(input$sf$name,input$sf$datapath)
  })
  clog <- shiny::reactive({
    shiny::req(input$clog)
    load_file(input$clog$name,input$clog$datapath)
  })
  clogdef <- shiny::reactive({
    shiny::req(input$clogdef)
    load_file(input$clogdef$name,input$clogdef$datapath)
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    stateclogdef<-input$clogdef
    if(stateoperation=="clog"&!is.null(statedata)&!is.null(statequestionnaire)&!is.null(stateclogdef)){enable("runchecks")} else{disable("runchecks")}
  })
  observe({
    stateoperation<-input$operation
    statequestionnaire<-input$questionnaire
    if(stateoperation=="makedap"&!is.null(statequestionnaire)){enable("rundap")} else{disable("rundap")}
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    stateclog<-input$clog
    if(stateoperation=="clean"&!is.null(statedata)&!is.null(statequestionnaire)&!is.null(stateclog)){enable("runclean")} else{disable("runclean")}
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    if(stateoperation%in%c("label_toxml","xml_tolabel")&!is.null(statedata)&!is.null(statequestionnaire)){enable("runconv")} else{disable("runconv")}
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    statesf<-input$sampling
    statedap<-input$dap
    if(stateoperation=="analysis"&!is.null(statedata)&!is.null(statequestionnaire)&!is.null(statedap)){enable("runres")} else{disable("runres")}
  })
  ex_logdef<-reactive(read_excel("inst/ext/log_logdef.xlsx",2))
  output$ex_logdef<-renderTable(ex_logdef())
  ex_log<-reactive(read_excel("inst/ext/log_logdef.xlsx",1))
  output$ex_log<-renderTable(ex_log())
  
  weights<-reactive({
    req(input$sampling)
    if(input$sampling=="weighted"){
      map_to_weighting(sampling.frame = sf(),
                       data.stratum.column = input$data.stratum.column,
                       sampling.frame.population.column = input$sampling.frame.population.column,
                       sampling.frame.stratum.column = input$sampling.frame.stratum.column,
                       data = db())
    } else {NULL}
    
  })
  cluster_variable_name<-reactive({
    req(input$sampling)
    if(input$sampling=="nonweighted"){
      NULL
    } else if(input$cluster=="yes"){
      input$cluster_variable_name
    } else {NULL}
  })
  questionnaire<-reactive({
    load_questionnaire(data = db(),
                       questions = survey(),
                       choices = choices(),
                       choices.label.column.to.use = input$choiceslabel)
  })
  questionnaire_nodata<-reactive({
    load_questionnaire_without_data(
                       survey = survey(),
                       choices = choices(),
                       choices.label.column.to.use = input$choiceslabel)
  })
  analysis<-reactive({
    # req(input$sampling)
    # req(input$cluster)
    results<-hypegrammaR::from_analysisplan_map_to_output(data = db(),
                                                 analysisplan = dap(),
                                                 weighting = weights(),
                                                 cluster_variable_name =cluster_variable_name(),
                                                 questionnaire = questionnaire(),
                                                 labeled = F,
                                                 confidence_level = input$cl)
    final_result<- results$results %>%
      lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)
    shinyjs::enable("dwres")
    shinyjs::html("dwres", "Download results")
    final_result
  })
  forout_res <- reactiveValues()
  observeEvent(input$runres, {
    withConsoleRedirect("console", {
      x<-analysis()
      forout_res$x=x
    })
  })
  check<-reactive({
    autre_check<-other_check(db(),survey())
    logbook<-apply_checks(db(),clogdef())
    clog<-bind_rows(logbook,autre_check)
    if(input$cloglabel=="yes"){clog<-label_clog(clog,survey(),choices(),input$surveylabel,input$choiceslabel)}
    shinyjs::enable("dwclog")
    if(input$cloglabel=="yes"){
      shinyjs::html("dwclog", "Download labeled Cleaning LOG")
    } else{shinyjs::html("dwclog", "Download Cleaning LOG")}
    clog
    
  })
  forout_clog <- reactiveValues()
  observeEvent(input$runchecks, {
    x<-check()
    forout_clog$x=x
  })
  clean<-reactive({
    db<-db()[which(!is.na(db()$uuid)),]
    db<- cleaning_data(db(),clog(),survey(),choices())
    shinyjs::enable("dwclean")
    shinyjs::html("dwclean", "Download Clean data")
    db
    
  })
  forout_clean <- reactiveValues()
  observeEvent(input$runclean, {
    x<-clean()
    forout_clean$x=x
  })
  makedap<-reactive({
    dap<-make_analysisplan_all_vars(questionnaire_nodata())
    shinyjs::enable("dwdap")
    shinyjs::html("dwdap", "Download DAP")
    dap
  })
  forout_dap <- reactiveValues()
  observeEvent(input$rundap, {
    x<-makedap()
    forout_dap$x=x
  })
  conv<-reactive({
    if(input$operation=="xml_tolabel"){
      db<-from_xml_tolabel(db(),choices(),survey(),input$choiceslabel,input$surveylabel)
    } else {
      db<-from_label_toxml(db(),choices(),survey(),input$choiceslabel,input$surveylabel) %>% sm_label_toxml(.,survey())
    }
    shinyjs::enable("dwconv")
    shinyjs::html("dwconv", "Download Converted DATA")
    db
    
  })
  forout_conv <- reactiveValues()
  observeEvent(input$runconv, {
    x<-conv()
    forout_conv$x=x
  })
  output$dwclog <- shiny::downloadHandler(
    filename = function() {
      if(input$cloglabel=="no"){
        paste0("cleaninglog-",humanTime(),".csv")
      }else {paste0("labeled-cleaninglog-",humanTime(),".csv")}
    },
    content = function(file) {
      xout<-forout_clog$x
      write.csv(xout, file, row.names = FALSE)
    }
  )
  output$dwclean <- shiny::downloadHandler(
    filename = function() {
      paste0("cleandata-",humanTime(),".csv")
    },
    content = function(file) {
      xout<-forout_clean$x
      write.csv(xout, file, row.names = FALSE)
    }
  )
  output$dwdap <- shiny::downloadHandler(
    filename = function() {
      paste0("dap-",humanTime(),".csv")
    },
    content = function(file) {
      xout<-forout_dap$x
      write.csv(xout, file, row.names = FALSE)
    }
  )
  output$dwconv <- shiny::downloadHandler(
    filename = function() {
      paste0("Converted_data-",humanTime(),".xlsx")
    },
    content = function(file) {
      xout<-forout_conv$x
      write_xlsx(xout, file)
    }
  )
  output$dwres <- shiny::downloadHandler(
    filename = function() {
      paste0("Analysis_results-",humanTime(),".csv")
    },
    content = function(file) {
      xout<-forout_res$x
      write.csv(xout, file, row.names = FALSE)
    }
  )
  output$full_logdef <- shiny::downloadHandler(
    filename = function() {
      paste0("log_definition_sample.csv")
    },
    content = function(file) {
      xout<-read_excel("inst/ext/log_logdef.xlsx",4)
      write.csv(xout, file, row.names = FALSE)
    }
  )
  output$full_log <- shiny::downloadHandler(
    filename = function() {
      paste0("cleaningLOG_sample.csv")
    },
    content = function(file) {
      xout<-read_excel("inst/ext/log_logdef.xlsx",3)
      write.csv(xout, file, row.names = FALSE)
    }
  )
}