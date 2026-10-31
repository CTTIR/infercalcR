library(infercalcR)
# Count arithmetic is independent of legacy identity dependencies.
t<-classification_metrics(c(tp=8,fn=2,fp=3,tn=7))
stopifnot(t$value[t$metric_id=='tpr']==.8,t$value[t$metric_id=='ppv']==8/11,
  t$value[t$metric_id=='accuracy']==.75)
z<-classification_metrics(c(tp=0,fn=5,fp=0,tn=95))
stopifnot(z$status[z$metric_id=='ppv']=='undefined',is.na(z$value[z$metric_id=='ppv']))
stopifnot(length(classification_dictionary()$metrics)>40,nrow(metric_dictionary())>40)
# These are explicit optional legacy paths; the minimal lane separately checks absence.
if (requireNamespace('digest',quietly=TRUE)&&requireNamespace('jsonlite',quietly=TRUE)) {
  d<-data.frame(id=1:8,y=c(1,1,1,1,0,0,0,0),s=c(.9,.8,.8,.6,.4,.3,.3,.1))
  s<-classification_spec(d,truth='y',positive_label=1,scores='s',id='id',
    rule=cutoff_rule(cutoff=.5,selection_data='predeclared_rule'),score_scale='probability',evaluation_split='held_out_test')
  before<-serialize(s,NULL)
  x<-evaluate_cutoff(s)
  stopifnot(identical(serialize(s,NULL),before))
  m<-classification_table(x)
  stopifnot(m$value[m$metric_id=='tpr']==1,m$value[m$metric_id=='tnr']==1,
    classification_auc(s)$value==1)
  rev<-classification_spec(d,truth='y',positive_label=1,scores='s',cutoff=.5,
    direction='lower_is_positive',evaluation_split='held_out_test')
  stopifnot(classification_auc(rev)$value==0)
  const<-d;const$s<-.5
  cs<-classification_spec(const,truth='y',positive_label=1,scores='s',cutoff=.5)
  stopifnot(classification_auc(cs)$value==.5,nrow(cutoff_sweep(cs))==2)
  n<-d;n$s[1]<-NA
  ns<-classification_spec(n,truth='y',positive_label=1,scores='s',cutoff=.5,missing='exclude')
  nx<-evaluate_cutoff(ns);stopifnot(nx$diagnostics$n_excluded_rows==1)
  common<-as_inference(x);rd<-result_data(common)
  stopifnot(identical(as_inference(common),common),is.data.frame(rd),
    result_metadata(common)$evaluation$evaluation_split=='held_out_test',
    is.null(attr(common$payload$native,'ig_cls_evaluated_spec')),
    is.null(attr(common$payload$native,'ig_cls_data')),
    any(rd$interval.method=='wilson',na.rm=TRUE),any(rd$interval.method=='clopper_pearson',na.rm=TRUE))
  # Raw rows and subject identifiers are omitted recursively from portable payloads.
  attrs<-function(q){if(is.list(q)) c(names(attributes(q)),unlist(lapply(q,attrs))) else names(attributes(q))}
  stopifnot(!any(c('ig_cls_data','ig_cls_evaluated_spec')%in%attrs(common$payload)))
  names_all<-function(q)if(is.list(q))c(names(q),unlist(lapply(q,names_all)))else character()
  stopifnot(!'id'%in%names_all(common$payload))
  # Seed restoration and deterministic bootstrap.
  us<-classification_uncertainty(bootstrap_replicates=100,seed=123)
  su<-update(s,uncertainty=us)
  set.seed(19);seed<-.Random.seed
  a<-evaluate_cutoff(su);stopifnot(identical(seed,.Random.seed))
  b<-evaluate_cutoff(su);stopifnot(identical(a,b))
  # Matching reattachment works; mutation is rejected by a specific condition.
  stripped<-s;attr(stripped,'ig_cls_data')<-NULL
  stopifnot(identical(attach_classification_data(stripped,d)$spec_hash,s$spec_hash))
  bad<-d;bad$s[1]<-.1
  err<-tryCatch(attach_classification_data(stripped,bad),infercalcR_invalid_input=function(e)e)
  stopifnot(inherits(err,'infercalcR_invalid_input'))
  legacy<-legacy_canonical_json(legacy_unclass_frozen(s))
  stopifnot(is.list(legacy_parse_json(legacy)),identical(legacy_hash_value(legacy_unclass_frozen(s)),legacy_sha256(legacy)))
  cat('PASS classification: count/status edges; direction, ties, exclusions; portable common view; seed restoration and fingerprint mutation.\n')
} else cat('SKIP legacy classifier identities: optional digest/jsonlite absent; core count tests passed.\n')
