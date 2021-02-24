cleanRanalyzR<-function(...){
    options(shiny.maxRequestSize =200 * 1024^2)
    old <- options(shiny.autoload.r = FALSE)
    on.exit(options(old), add = TRUE)
    shiny::shinyApp(ui, server,...)
}
