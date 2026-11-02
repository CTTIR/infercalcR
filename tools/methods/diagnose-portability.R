# Diagnostic only: record all differences; acceptance remains in the installed tests.
args<-commandArgs(TRUE);output<-if(length(args))args[1L] else tempfile('portability-')
expected_library<-if(length(args)>1L)normalizePath(args[2L],mustWork=TRUE) else NULL
if(!is.null(expected_library)).libPaths(c(expected_library,.libPaths()))
library(infercalcR)
if(!is.null(expected_library))stopifnot(identical(normalizePath(find.package('infercalcR')),normalizePath(file.path(expected_library,'infercalcR'))))
dir.create(output,recursive=TRUE,showWarnings=FALSE)
writeLines(capture.output(sessionInfo()),file.path(output,'session-start.txt'))
fixtures<-readRDS(system.file('methods','synthetic-baseline.rds',package='infercalcR'))
actual<-list();results<-list();deltas<-list()
compare<-function(a,b,path,descend=TRUE){
 if(descend&&is.list(a)&&is.list(b)&&identical(names(a),names(b))&&length(a)==length(b)){
  compare(attributes(a),attributes(b),paste(path,'attributes'),FALSE)
  for(i in seq_along(a))compare(a[[i]],b[[i]],paste0(path,'$',if(is.null(names(a)))i else names(a)[i]))
 }else{
  comparison<-all.equal(a,b,tolerance=1e-12,check.environment=FALSE)
  if(!isTRUE(comparison)){
   err<-if(is.numeric(a)&&is.numeric(b)&&length(a)==length(b))abs(as.numeric(a)-as.numeric(b)) else numeric()
   err<-err[is.finite(err)]
   detail<-paste(comparison,collapse='; ')
   deltas[[length(deltas)+1L]]<<-data.frame(field=path,max_absolute_delta=if(length(err))max(err) else NA_real_,detail=detail)
   cat('DELTA',path,if(length(err))format(max(err),digits=17) else '',detail,'\n')
  }
 }
}
for(id in names(fixtures)){
 if(grepl('[.]',id))next
 f<-fixtures[[id]];a<-f$arguments;alt<-if(is.null(a$alternative))'two.sided' else a$alternative
 level<-if(is.null(a$conf.level)).95 else a$conf.level;a[c('alternative','conf.level')]<-NULL
 cat('METHOD',id,'\n')
 input_before<-serialize(a,NULL);input_copy<-unserialize(input_before)
 value<-tryCatch(do.call(run_inference,c(list(spec=method_spec(id,alt,level)),a)),error=identity)
 cat('INPUT',id,'BYTES',identical(input_before,serialize(a,NULL)),'VALUES',isTRUE(all.equal(a,input_copy)),'\n')
 if(inherits(value,'error')){cat('ERROR',id,conditionMessage(value),'\n');next}
 results[[id]]<-value
 actual[[id]]<-value$payload$native
 compare(actual[[id]],f$expected,id)
}
for(id in names(fixtures)[grepl('[.]',names(fixtures))]){
 f<-fixtures[[id]];cat('METHOD',id,'\n')
 value<-tryCatch({
  if(grepl('[.]parallel_analysis$',id))do.call(parallel_analysis,f$arguments)
  else if(grepl('[.]predict_new$',id)){
   parent<-sub('[.]predict_new$','',id)
   if(is.null(results[[parent]]))stop('Prediction prerequisite unavailable')
   do.call(predict_inference,c(list(result=results[[parent]]),f$arguments))
  }else stop('Unknown diagnostic helper')
 },error=identity)
 if(inherits(value,'error')){cat('ERROR',id,conditionMessage(value),'\n');next}
 actual[[id]]<-value$payload$native
 compare(actual[[id]],f$expected,id)
}
saveRDS(actual,file.path(output,'runtime-native.rds'))
if(length(deltas))utils::write.csv(do.call(rbind,deltas),file.path(output,'field-deltas.csv'),row.names=FALSE)
# Distinguish mutation of the source object's data, family closures, and environments.
d<-data.frame(x=rep(c(-1,0,1),10),y=rep(c(0,1,1,1,0,0,0,0,1,1),3))
g<-stats::glm(y~x,d,family=stats::binomial());before<-unserialize(serialize(g,NULL))
raw_before<-serialize(g,NULL);converted<-as_inference(g,population='synthetic portability diagnostic')
cat('GLM_SERIALIZATION_UNCHANGED',identical(raw_before,serialize(g,NULL)),'\n')
for(nm in names(g))if(!identical(serialize(before[[nm]],NULL),serialize(g[[nm]],NULL)))cat('GLM_CHANGED_COMPONENT',nm,'\n')
for(nm in names(g$family))if(!identical(serialize(before$family[[nm]],NULL),serialize(g$family[[nm]],NULL)))cat('GLM_CHANGED_FAMILY_FIELD',nm,'\n')
cat('GLM_DATA_IDENTICAL',identical(before$model,g$model),'COEFFICIENTS_IDENTICAL',identical(before$coefficients,g$coefficients),'\n')
saveRDS(list(before=before,after=g,result=converted),file.path(output,'glm-conversion.rds'))

writeLines(capture.output(sessionInfo()),file.path(output,'session.txt'))
