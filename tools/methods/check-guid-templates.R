# Development verification of generated teaching examples against frozen synthetic results.
args<-commandArgs(TRUE);stopifnot(length(args)==1L)
fixtures<-readRDS(system.file('methods','synthetic-baseline.rds',package='infercalcR'))
files<-list.files(args[1L],'[.]R$',full.names=TRUE)
checked<-character()
for(file in files){
 id<-sub('^tpl[.]','',sub('[.]v1[.]R$','',basename(file)))
 env<-new.env(parent=globalenv());env$alternative<-if(id=='boschloo_one_sided')'greater' else 'two.sided'
 env$confidence_level<-.95;env$example_group_count<-3L
 path<-tempfile(fileext='.pdf');grDevices::pdf(path)
 invisible(capture.output(sys.source(file,env)));grDevices::dev.off();unlink(path)
 value<-env$example_result;expected<-fixtures[[id]]$expected
 if(grepl('^boschloo_',id)){
  value$observed_result<-NULL;expected$observed_result<-NULL
 }else if(grepl('^(classifier_|diagnostic_)',id)){
  value[c('specification','result')]<-NULL;expected[c('specification','result')]<-NULL
  # Public-call provenance changes from inferguidR to infercalcR; scientific rule fields must agree.
  if(!is.null(value$selected_rule)){
   value$selected_rule<-unclass(value$selected_rule);expected$selected_rule<-unclass(expected$selected_rule)
   value$selected_rule$r_call<-NULL;expected$selected_rule$r_call<-NULL
  }
 }
 comparison<-all.equal(value,expected,tolerance=1e-12,check.environment=FALSE)
 if(!isTRUE(comparison))stop(id,': ',paste(comparison,collapse='; '))
 if(grepl('^prediction_model_',id)){
  helper<-fixtures[[paste0(id,'.predict_new')]]
  prediction<-do.call(env$predict_new,c(list(result=env$example_result),helper$arguments))
  stopifnot(isTRUE(all.equal(prediction,helper$expected,tolerance=1e-12)))
  # The preserved default is the original vector form, resolved before delegation.
  stopifnot(is.numeric(env$predict_new(env$example_result,helper$arguments$newdata)))
 }
 checked<-c(checked,id);cat('PASS',id,'\n')
}
stopifnot(length(checked)==68L)
cat('PASS all 68 regenerated teaching examples and both prediction wrappers\n')
