load_questionnaire_without_data<-function(survey, choices, choices.label.column.to.use = NULL){
  data_str<-matrix(NA,0,length(survey$name[which(!is.na(survey[["name"]]))]))
  colnames(data_str)<-survey$name[which(!is.na(survey[["name"]]))]
  data_str<-as.data.frame(data_str)
  loaded_questionnaire<-koboquest::load_questionnaire(data = data_str, questions = survey, choices = choices, choices.label.column.to.use = choices.label.column.to.use)
  return(loaded_questionnaire)
}

map_to_datatypes<-function(df,questionnaire){
  
  types <- sapply(names(df),questionnaire$question_type,
                  from.questionnaire = T,
                  from.data = T,
                  data = df)
  
  sm_choices <- purrr::map(names(df),
                           questionnaire$question_is_sm_choice) %>% unlist
  
  raw_q <- questionnaire$raw_questionnaire()$questions
  
  types_raw <- raw_q[match(names(df),raw_q$name),"type"]   
  types[types %in% c("select_one","select_multiple")]<-"categorical"
  types[sm_choices]<-"sm_choice"
  types[types=="numeric"] <- "numerical"
  types[types_raw == "text"] <- "text"
  types[types_raw == "calculate"] <- "numerical"
  types[types_raw == "calculate"] <- "numerical"
  types[str_detect(types_raw$type,"deviceid|start|today|note|begin|end|group")] <-NA
  
  types
}

make_analysisplan_all_vars <- function(questionnaire,
                                       repeat.for.variable=NA,
                                       independent.variable = NA,
                                       hypothesis.type = "direct_reporting"){
  survey<-questionnaire$raw_questionnaire()[1] %>% as.data.frame()
  df<-matrix(NA,0,length(survey$questions.name[which(!is.na(survey[["questions.name"]]))]))
  colnames(df)<-survey$questions.name[which(!is.na(survey[["questions.name"]]))]
  df<-as.data.frame(df)
  
  
  if(!is.data.frame(df)){stop(" df must be a data frame")}
  
  types <- map_to_datatypes(df,questionnaire)
  types <- types[!is.na(types)]
  
  if(!is.na(independent.variable)){
    if(any(types[names(types)==independent.variable]!="categorical")){stop("independent variable must be categorical (and can not be select_multiple TRUE/FALSE column")}
    independent.variable.type<-types[names(types)==independent.variable]
  }else{
    independent.variable.type<-NA
  }
  good_dependent_variables<-names(types)[types %in% c("numerical","numeric","categorical")]
  
  analysisplan<-data.frame(research.question = "RQ: not specified (automatic analysisplan)",
                           sub.research.question = "sub RQ not specified (automatic analysisplan)",
                           repeat.for.variable=repeat.for.variable,
                           independent.variable=independent.variable,
                           independent.variable.type=independent.variable.type,
                           dependent.variable=good_dependent_variables,
                           dependent.variable.type = types[good_dependent_variables],
                           hypothesis.type = hypothesis.type,
                           stringsAsFactors = F)
  return(analysisplan)
  
}
