# Development-only capture from the unchanged baseline's synthetic teaching examples.
args <- commandArgs(TRUE)
stopifnot(length(args)==2L)
source_root<-normalizePath(args[1L],mustWork=TRUE)
run_root<-normalizePath(args[2L],mustWork=TRUE)
.libPaths(c(file.path(run_root,'libraries/integration'),file.path(run_root,'baseline/library'),.libPaths()))
files<-list.files(file.path(source_root,'inst/advisor/templates'),'[.]R$',full.names=TRUE)
fixtures<-list(); failures<-list()
for(file in files) {
 id<-sub('^tpl[.]','',sub('[.]v1[.]R$','',basename(file)))
 env<-new.env(parent=globalenv());env$alternative<-if(id=='boschloo_one_sided')'greater' else 'two.sided'
 env$confidence_level<-.95;env$example_group_count<-3L
 path<-tempfile(fileext='.pdf');grDevices::pdf(path)
 error<-tryCatch({invisible(capture.output(sys.source(file,env)));NULL},error=function(e)conditionMessage(e))
 grDevices::dev.off();unlink(path)
 if(!is.null(error)) {failures[[id]]<-error;cat('BLOCKED',id,error,'\n');next}
 expressions<-as.list(parse(file))
 calls<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('example_result')),expressions)
 stopifnot(length(calls)==1L)
 call<-match.call(definition=env$analyse,call=calls[[1]][[3]],expand.dots=FALSE)
 inputs<-lapply(as.list(call)[-1L],eval,envir=env)
 fixtures[[id]]<-list(arguments=inputs,expected=env$example_result)
 if(id=='efa_common_factor_ml') fixtures[[paste0(id,'.parallel_analysis')]]<-list(
   arguments=list(data=env$example_data,variables=paste0('v',1:6),independent_units=TRUE,continuous_measurements=TRUE,B=100L),expected=env$example_parallel)
 if(grepl('^prediction_model_',id)) {
  newdata<-env$example_data[1:3,,drop=FALSE]
  interval<-if(id=='prediction_model_linear')'prediction' else 'risk'
  fixtures[[paste0(id,'.predict_new')]]<-list(arguments=list(newdata=newdata,interval=interval),expected=env$predict_new(env$example_result,newdata,interval=interval))
 }
 cat('CAPTURED',id,'\n')
}
saveRDS(fixtures,'inst/methods/synthetic-baseline.rds',version=3)
write.csv(data.frame(method_id=names(failures),reason=unlist(failures)),file.path(run_root,'handoffs/P05-fixture-failures.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(run_root,'handoffs/P05-fixture-session.txt'))
cat('Captured',length(fixtures),'of 71 mandatory computational capabilities; failures',length(failures),'\n')
