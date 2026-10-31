# Preserved numerical assertions moved from the original teaching-script tests.
library(infercalcR)
ns<-asNamespace('infercalcR')
helper<-function(name)get(name,envir=ns,inherits=FALSE)
refs<-readRDS(system.file('methods','helper-oracles.rds',package='infercalcR'))
perms<-function(x)if(length(x)==1L)matrix(x,nrow=1L) else do.call(rbind,lapply(seq_along(x),function(i)cbind(x[i],perms(x[-i]))))
jt<-helper('.obs_jonckheere_terpstra_jt_twice_statistic')
for(ref in refs$trend$cases){
 y<-unlist(ref$values);g<-rep(seq_along(ref$group_sizes),unlist(ref$group_sizes));orders<-perms(seq_along(y))
 actual<-apply(orders,1,function(i)jt(y[i],g));tab<-table(actual)
 stopifnot(identical(as.numeric(tab),as.numeric(unlist(ref$distribution))),identical(names(tab),names(ref$distribution)),
  jt(y,g)==ref$twice_observed,sum(tab)==ref$labelled_permutations)
}
for(kind in c('cluster_hierarchical_ward','cluster_kmeans')){
 ari<-helper(paste0('.obs_',kind,'_adjusted_rand'))
 stopifnot(ari(c(1,1,2,2),c(2,2,1,1))==1,isTRUE(all.equal(ari(c(1,1,2,2),c(1,2,1,2)),-.5,tolerance=1e-15)))
}
measure<-helper('.obs_prediction_model_logistic_measure_prediction')
failure<-tryCatch(measure(rep(1,10),rep(.5,10)),error=identity)
stopifnot(inherits(failure,'error'),grepl('Both outcome classes',conditionMessage(failure),fixed=TRUE),
 identical(measure(c(0,1,0,1,0,1),c(.2,.2,.5,.5,.8,.8))['auc'],c(auc=.5)))
if(requireNamespace('cluster',quietly=TRUE)){
 ref<-refs$gower
 d<-data.frame(amount=c(0,2,4,6,NA,3),kind=factor(c('a','a','b','b','c',NA)),grade=ordered(c('low','middle','high','middle','high','low'),levels=c('low','middle','high')),
  symmetric=c(0,1,0,1,1,0),asymmetric=c(0,0,1,1,0,1))
 types<-c(amount='continuous',kind='nominal',grade='ordinal',symmetric='symmetric_binary',asymmetric='asymmetric_binary')
 fit<-helper('.obs_cluster_gower_pam_gower_fit')
 r<-fit(d,2,types,unlist(ref$weights));expected<-do.call(rbind,lapply(ref$distance,unlist))
 stopifnot(isTRUE(all.equal(unname(r$distance),expected,tolerance=1e-12)),
  isTRUE(all.equal(fit(d,2,types,unlist(ref$weights)*100)$distance,r$distance,tolerance=1e-12)))
}else if(identical(Sys.getenv('INFER_SUITE_FULL'),'true'))stop('Required Gower backend unavailable') else cat('SKIP optional Gower helper assertions: cluster absent\n')
cat('PASS preserved independent JT enumeration, ARI, prediction metric and Gower helper assertions\n')
