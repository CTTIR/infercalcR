library(infercalcR)
fixtures<-readRDS(system.file('methods','synthetic-baseline.rds',package='infercalcR'))
stored<-function(id)new_inference_result(data.frame(),list(method_id=id),payload=list(native=fixtures[[id]]$expected))
linear<-stored('prediction_model_linear');before<-serialize(linear,NULL)
newdata<-fixtures[['prediction_model_linear.predict_new']]$arguments$newdata
pred<-predict_inference(linear,newdata,interval='prediction')
mean<-predict_inference(linear,newdata,interval='mean')
none<-predict_inference(linear,newdata,interval='none')
pd<-result_data(pred);md<-result_data(mean);nd<-result_data(none)
stopifnot(all(pd$estimand=='one independent new outcome'),all(grepl('prediction interval',pd$interval.method)),
 all(grepl('residual variation',pd$interval.method)),all(pd$interval.scope=='pointwise'),
 result_metadata(pred)$interval_target=='new_outcome',result_metadata(pred)$interval_type=='prediction',
 identical(result_metadata(pred)$interval_interpretation,pred$payload$native$interpretation),
 all(md$estimand=='conditional mean of a new outcome'),all(grepl('confidence interval',md$interval.method)),
 result_metadata(mean)$interval_target=='conditional_mean',result_metadata(mean)$interval_type=='confidence',
 identical(pd$estimate,md$estimate),identical(pd$estimate,nd$estimate),
 all(pd$conf.low<md$conf.low),all(pd$conf.high>md$conf.high),
 all(is.na(nd$conf.low)),all(is.na(nd$interval.scope)),result_metadata(none)$interval_type=='none',
 identical(serialize(linear,NULL),before),
 isTRUE(all.equal(pred$payload$native,fixtures[['prediction_model_linear.predict_new']]$expected,tolerance=1e-12)))
# Canonical metadata follows the native argument matching behavior.
stopifnot(identical(result_data(predict_inference(linear,newdata,interval='p')),pd))
logistic<-stored('prediction_model_logistic')
risk<-predict_inference(logistic,fixtures[['prediction_model_logistic.predict_new']]$arguments$newdata,interval='risk')
rd<-result_data(risk)
stopifnot(all(rd$estimand=='event probability for a new case'),all(grepl('bootstrap',rd$interval.method)),
 result_metadata(risk)$interval_target=='event_probability',result_metadata(risk)$interval_type=='confidence',
 isTRUE(all.equal(risk$payload$native,fixtures[['prediction_model_logistic.predict_new']]$expected,tolerance=1e-12)))
unresolved<-linear;unresolved$payload$native$model$residual_variance<-0
ud<-result_data(predict_inference(unresolved,newdata,interval='prediction'))
stopifnot(all(ud$status=='not_applicable'),all(is.na(ud$conf.low)),all(is.na(ud$interval.scope)),
 all(ud$estimand=='one independent new outcome'))
cat('PASS mean-confidence versus new-outcome prediction labels, metadata, scope and native numerical parity\n')
