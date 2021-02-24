apply_checks<-function(db,clogdef){
  logbook<-data.frame(
    uuid= character(),
    question.name = character(),
    old.value = character(),
    parent.other.question = character(),
    parent.other.answer = character(),
    new.value = character(),
    probleme = character(),
    action=character()
  )
  names(clogdef)<-tolower(names(clogdef))
  for(i in 1:nrow(clogdef)){
    qname<-as.character(clogdef[["name"]][i])
    problem<-as.character(clogdef[["problem"]][i])
    if(qname%in%names(db)){
      index<-pulluuid(db,eval(parse(text=clogdef[["condition"]][[i]]),envir = db))
      logbook<- makeslog(db,logbook,index, qname, problem)
    }
  }
  
return(logbook)
}
