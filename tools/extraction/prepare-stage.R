# Build a temporary source candidate with explicit coordinator metadata proposals.
args<-commandArgs(TRUE);stopifnot(length(args) %in% c(1L,2L))
stage<-args[1L];dir.create(stage,recursive=TRUE,showWarnings=FALSE)
files<-list.files('R',full.names=TRUE,pattern='[.]R$');objects<-list();locations<-character()
for(f in files)for(e in parse(f))if(is.call(e)&&as.character(e[[1]])%in%c('<-','=')&&(is.symbol(e[[2]])||is.character(e[[2]]))){n<-as.character(e[[2]]);locations[n]<-basename(f);if(is.call(e[[3]])&&identical(e[[3]][[1]],as.name('function')))objects[[n]]<-eval(e[[3]])}
map<-read.csv('tools/extraction/source-map.csv',stringsAsFactors=FALSE)
source_repo<-if(length(args)==2L)args[2L] else Sys.getenv('INFER_SUITE_SOURCE',file.path('..','inferguidR'))
oldns<-readLines(file.path(source_repo,'NAMESPACE'))
oldexp<-sub('export\\((.*)\\)','\\1',oldns[grepl('^export\\(',oldns)])
exports<-unique(c(intersect(oldexp,map$symbol),names(locations)[locations=='legacy-providers.R'],'boschloo_inference'))
s3<-oldns[grepl('^S3method',oldns)]
s3key<-sub('^S3method\\((.*),(.*)\\)$','\\1.\\2',s3);s3key<-gsub('"','',s3key,fixed=TRUE)
s3<-s3[s3key%in%names(objects)]
newS3<-c('S3method(as_inference,boschloo_test)','S3method(result_data,boschloo_test)','S3method(result_metadata,boschloo_test)','S3method(as_inference,classification_result)','S3method(result_data,classification_result)','S3method(result_metadata,classification_result)')
nsadd<-c(paste0('export(',exports,')'),s3,newS3,'importFrom(stats,update)')
writeLines(nsadd,'tools/extraction/NAMESPACE.additions')
writeLines(c('Imports additions: none','Suggests retained: digest, jsonlite','Production references to inferguidR/infereportR namespaces: none','Historical identity features use optional digest/jsonlite; numeric core and Boschloo common results do not.'),'tools/extraction/DESCRIPTION.proposal')
# One topic per group documents every new public call with actual signatures.
groups<-list('legacy-computation'=exports[!exports%in%names(locations)[locations=='legacy-providers.R']], 'kernel-providers'=names(locations)[locations=='legacy-providers.R'])
param<-function(nm)switch(nm,
 x='Input data, finalized result, or numeric values as specified by the selected function.',
 spec='A classification specification returned by classification_spec().',
 data='A data frame containing observations, with explicit truth and score columns.',
 n1='Number of independent binomial observations in group 1.',n2='Number of independent binomial observations in group 2.',
 x1='Number of events in group 1.',x2='Number of events in group 2.',
 p1='Event probability in group 1.',p2='Event probability in group 2.',
 variant='Explicit Boschloo variant: minlike, central, less, or greater. Classifier variants are retained in their records.',
 alpha='Decision level strictly between zero and one; NULL omits a decision where supported.',
 control='Numerical settings from boschloo_control().',
 groups='Two distinct group labels in group 1, group 2 order.',event='Explicit event label.',
 indices='Distinct indices of rejected tables in x1-fastest two-binomial table order.',
 truth='Name of the observed truth column.',positive_label='Explicit positive-class value; never inferred.',
 scores='Score column name or named score-column vector.',
 direction='Declared score direction, higher_is_positive or lower_is_positive.',
 comparator='Declared cutoff comparison, consistent with direction.',
 missing='Explicit missingness policy; see the preserved specification settings.',
 level='Confidence level strictly between zero and one.',
 lang='Display-label language, en or de.',
 text='A data-only canonical JSON document or decimal string, according to the function.',
 lower_tail='Logical: use the lower tail when TRUE.',log_p='Logical: return or interpret probabilities on the logarithmic scale.',
 kernel='Unmodified design kernel returned by boschloo_design_kernel().',
 'Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.')
for(g in names(groups)) {
 nms<-groups[[g]];usage<-character();params<-character()
 for(n in nms){f<-objects[[n]];if(is.null(f))next;fm<-formals(f);params<-union(params,names(fm));dummy<-as.call(c(list(as.name(n)),fm));usage<-c(usage,paste(deparse(dummy,width.cutoff=90),collapse='\n'))}
 esc<-function(z)gsub('%','\\%',z,fixed=TRUE)
 txt<-c(paste0('\\name{',g,'}'),paste0('\\alias{',nms,'}'),
  paste0('\\title{',if(g=='legacy-computation')'Diagnostic and Exact Numerical Calculations' else 'Explicit Shared Numerical and Compatibility Providers','}'),
  '\\description{Preserved numerical procedures extracted from inferguidR, with explicit inputs, typed statuses and method settings. Calculation and display are separate.}',
  paste0('\\usage{\n',esc(paste(usage,collapse='\n\n')),'\n}'),
  '\\arguments{',vapply(params,function(n)paste0('\\item{',n,'}{',esc(param(n)),'}'),''),'}',
  '\\details{Boschloo tests use Fisher ordering and bounded nuisance maximization. The central variant doubles the smaller separately maximized one-sided value. Rejection uses the numerical upper bound; this bound is not a sampling confidence interval. The shared Boschloo inference path requires only standard R. Historical hashed specifications and the legacy calculator decimal parser require the optional packages digest and/or jsonlite and give an explicit missing-dependency error when absent.\n\nClassifier specifications preserve the positive class, direction, cutoff origin, missing-value exclusions, evaluation split and raw-data fingerprint. Raw observations are not retained in the common reporting payload. Basic count metrics, binormal rates and static dictionary access do not require contributed packages. Undefined metrics retain statuses and denominator reasons.\n\nProvider functions are public interfaces for validated package consumers. Historical canonical primitives accept already converted plain data; guidance-owned schema dispatch remains in inferguidR. No provider evaluates code from input strings. Calculator expressions use a restricted parser.\n\nThe inherited validation metadata records prior scoped checks; package extraction and adapter parity do not certify all scientific designs.}',
  '\\value{Computational functions return typed lists or ordinary data frames; common adapters return inference_result objects. Metadata providers return plain lists or vectors, probability kernels return numeric values, and validators signal typed errors for invalid inputs.}',
  if(g=='legacy-computation')c('\\examples{','r <- boschloo_inference(1, 5, 4, 7, variant = "less")','result_data(r)','classification_metrics(c(tp = 8, fn = 2, fp = 3, tn = 7))','}') else c('\\examples{','k <- boschloo_design_kernel(3, 4, variant = "minlike")','binomial_region_probability(k$idx, 3, 4, .2, .8)','t_probability(2, df = 10)','}'))
 writeLines(txt,file.path('man',paste0(g,'.Rd')))
}
# Clone only source package inputs, never the worktree .git file or build state.
for(p in c('R','man','tests','inst','DESCRIPTION','NAMESPACE','LICENSE','LICENSE.md','README.md','NEWS.md','CONTRACT.md'))if(file.exists(p)){dest<-file.path(stage,basename(p));if(dir.exists(p)){dir.create(dest,showWarnings=FALSE);file.copy(list.files(p,full.names=TRUE,all.files=TRUE,no..=TRUE),dest,recursive=TRUE,overwrite=TRUE)}else file.copy(p,dest,overwrite=TRUE)}
writeLines(unique(c(readLines('NAMESPACE'),nsadd)),file.path(stage,'NAMESPACE'))
writeLines('^CONTRACT[.]md$',file.path(stage,'.Rbuildignore'))
cat('Staged',length(exports),'exports and',length(s3)+length(newS3),'S3 registrations at',stage,'\n')
