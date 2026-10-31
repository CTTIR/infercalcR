library(infercalcR)
p<-Sys.getenv('INFER_SUITE_BASELINE_FIXTURES')
if(nzchar(p)) {
 stopifnot(file.exists(p),requireNamespace('jsonlite',quietly=TRUE),requireNamespace('digest',quietly=TRUE))
 baseline<-readRDS(p) # Trusted synthetic local test artifact, never an untrusted decoder.
 now<-list()
 for(v in c('less','greater','minlike','central')) {
   now[[paste0('boschloo_',v)]]<-boschloo_test(1,5,4,7,variant=v,groups=c('control','treatment'),event='response')
   now[[paste0('boschloo_region_',v)]]<-boschloo_region(5,7,.05,v)
 }
 now$boschloo_boundary<-boschloo_test(0,5,0,7,variant='central')
 now$counts<-evaluate_cutoff(classification_spec(counts=c(tp=40,fn=10,fp=20,tn=130)))
 now$zero_alert<-evaluate_cutoff(classification_spec(counts=c(tp=0,fn=5,fp=0,tn=95)))
 d<-data.frame(id=sprintf('unit%02d',1:10),truth=c('yes','no','yes','no','no','yes','no','no','yes','no'),score=c(.9,.2,.7,.4,.4,.6,.1,.3,.4,.8))
 s<-classification_spec(d,truth='truth',positive_label='yes',scores='score',cutoff=.5,id='id',evaluation_split='held_out_test',score_scale='probability')
 now$scored<-evaluate_cutoff(s);now$auc<-classification_auc(s)
 now$sweep<-cutoff_sweep(s,metrics=c('ppv','tpr','mcc'));now$selected<-select_cutoff(s,'max_youden')
 now$scenario<-prevalence_scenario(now$scored,.1);now$binormal<-binormal_model(.8,1.2)
 now$binormal_rates<-binormal_rates(cutoff=c(-1,0,1),auc=.8,sigma=1.2)
 now$metric_zero<-classification_metrics(c(tp=0,fn=5,fp=0,tn=95))
 now$metric_infinite<-classification_metrics(c(tp=7,fn=3,fp=0,tn=10))
 normalize<-function(x){
  if(is.character(x)){x<-gsub('inferguidR::','infercalcR::',x,fixed=TRUE);x<-gsub('inferguidR Boschloo engine','infercalcR Boschloo engine',x,fixed=TRUE);return(x)}
  if(is.list(x)){a<-attributes(x);x<-lapply(x,normalize);attributes(x)<-a}
  x
 }
 for(n in names(now)) {
   ok<-all.equal(normalize(baseline[[n]]),normalize(now[[n]]),tolerance=1e-13,check.attributes=TRUE)
   if(!isTRUE(ok))stop(n,': ',paste(ok,collapse='; '))
 }
 # Historical specification identities are unaffected by numerical extraction.
 stopifnot(identical(now$scored$spec_hash,baseline$scored$spec_hash),
  identical(now$boschloo_minlike$spec_hash,baseline$boschloo_minlike$spec_hash),
  identical(now$boschloo_region_minlike$region_hash,baseline$boschloo_region_minlike$region_hash))
 cat('PASS baseline:',length(now),'complete API objects; all fields/attributes compared, only producing namespace/engine label normalized.\n')
} else cat('SKIP external historical fixture lane: set INFER_SUITE_BASELINE_FIXTURES to the trusted baseline RDS.\n')
