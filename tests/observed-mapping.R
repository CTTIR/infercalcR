# Views are checked against the original synthetic outputs, independently of payload parity.
library(infercalcR)
fixtures<-readRDS(system.file('methods','synthetic-baseline.rds',package='infercalcR'))
scalar<-c(t_mean_one_sample='mean_difference',t_means_paired='mean_difference',
 t_means_two_independent_student='mean_difference',t_means_two_independent_welch='mean_difference',
 binomial_exact_one_sample='proportion',prop_one_sample_score='proportion',
 prop_two_independent_pearson_chisq='proportion_difference',fisher_exact_2x2='conditional_odds_ratio',
 sign_test_one_sample='positive_probability',sign_test_paired='positive_probability',
 signed_rank_wilcoxon='pseudomedian',signed_rank_wilcoxon_one_sample='pseudomedian',
 brunner_munzel='estimate',mcnemar_exact_conditional='conditional_positive_probability',mcnemar_midp='conditional_positive_probability',
 variance_f_test_two='variance_ratio',correlation_pearson='estimate',correlation_spearman='estimate',correlation_kendall='estimate',
 regression_linear_simple='estimate',glmm_logistic_random_intercept='odds_ratio',gee_logistic_exchangeable='odds_ratio')
same<-function(x,y)stopifnot(isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-12)))
checked<-character()
for(id in names(fixtures)){
 if(grepl('[.]|^(boschloo_|classifier_|diagnostic_)',id))next
 cat<-method_catalogue();pkg<-strsplit(cat$backend[match(id,cat$method_id)],',',fixed=TRUE)[[1L]]
 if(!all(vapply(pkg,requireNamespace,logical(1),quietly=TRUE))){
  if(identical(Sys.getenv('INFER_SUITE_FULL'),'true'))stop('Missing required backend for ',id)
  next
 }
 f<-fixtures[[id]];a<-f$arguments;alt<-if(is.null(a$alternative))'two.sided' else a$alternative
 level<-if(is.null(a$conf.level)).95 else a$conf.level;a[c('alternative','conf.level')]<-NULL
 r<-do.call(run_inference,c(list(spec=method_spec(id,alt,level),result_id=paste0('fixture:',id)),a));d<-result_data(r);n<-f$expected
 stopifnot(identical(result_metadata(r)$result_id,paste0('fixture:',id)),nrow(d)>0)
 if(id%in%names(scalar)){
  same(d$estimate[1],n[[scalar[[id]]]]);same(d$p.value[1],n$p_value)
  if(length(n$confidence_interval)==2)same(c(d$conf.low[1],d$conf.high[1]),n$confidence_interval)
  stopifnot(nzchar(d$estimand[1]))
 }else if(id=='anova_factorial'){
  same(d$p.value,n$terms$p_value);same(d$p.adjusted,n$terms$p_holm);same(d$statistic,n$terms$F)
  stopifnot(all(is.na(d$conf.low)),all(d$family_size==nrow(n$terms)))
 }else if(id=='ancova_baseline_adjusted'){
  same(d$p.value[d$component=='omnibus'],n$group_test$p_value)
  same(d$estimate[d$component=='contrast'],n$differences$estimate)
  same(d$conf.low[d$component=='contrast'],n$differences$lower)
  same(d$estimate[d$component=='adjusted_mean'],n$adjusted_means$estimate)
 }else if(grepl('^regression_',id)){
  tab<-if(is.data.frame(n$coefficient_table))n$coefficient_table else n$coefficient_table_two_sided
  rows<-d[d$component=='coefficient',]
  same(rows$estimate,tab$estimate);same(rows$p.value,tab$p_value);same(rows$conf.low,tab$lower);same(rows$conf.high,tab$upper)
  same(d$p.value[d$component=='omnibus'][1],if(is.null(n$omnibus$p_value))n$p_value else n$omnibus$p_value)
  if(id=='regression_logistic_binary'){
   stopifnot(all(grepl('profile',rows$interval.method)),is.na(rows$conf.low[rows$contrast=='(Intercept)']))
   same(d$conf.low[d$component=='ratio'],tab$ratio_lower[-1])
  }
 }else if(grepl('^lmm_',id)){
  same(d$p.value[d$component=='omnibus'],n$p_value)
  same(d$estimate[d$component=='factor_mean'],n$factor_means$estimate)
  same(d$conf.low[d$component=='factor_mean'],n$factor_means$lower)
 }else if(grepl('^prediction_model_',id)){
  same(d$estimate[d$component=='apparent'],n$apparent);same(d$estimate[d$component=='corrected'],n$corrected)
  stopifnot(all(is.na(d$p.value)),all(is.na(d$conf.low)))
 }else if(id=='rank_sum_mann_whitney'){
  same(d$estimate[1],n$probability_superiority);same(d$p.value[1],n$p_value);stopifnot(is.na(d$conf.low[1]))
 }else if(id=='descriptive_summary'){
  same(d$estimate[d$estimand=='population mean'],n$mean);same(d$conf.low[d$estimand=='population mean'],n$confidence_interval[1]);stopifnot(all(is.na(d$p.value)))
 }else if(id=='pca_components'){
  same(d$estimate,n$eigenvalues);stopifnot(all(is.na(d$p.value)))
 }else if(id=='efa_common_factor_ml'){
  same(d$estimate[d$component=='loading'],as.vector(n$loadings));same(d$p.value[d$component=='omnibus'],n$goodness_of_fit$p_value)
 }else if(grepl('^cluster_',id)){
  same(d$estimate[1],n$mean_silhouette);same(d$estimate[2],n$stability$mean);stopifnot(all(is.na(d$p.value)))
 }else{
  same(d$p.value[1],n$p_value)
  if(length(n$statistic)==1)same(d$statistic[1],n$statistic)
  stopifnot(all(is.na(d$conf.low)),all(is.na(d$conf.high)))
 }
 if(length(n$n_used)==1)stopifnot(all(d$n==n$n_used))
 if(length(n$n_excluded)==1)stopifnot(all(d$n.excluded==n$n_excluded))
 checked<-c(checked,id)
}
stopifnot(length(checked)>=50L)
cat('PASS common analytical view matrix:',length(checked),'primary methods\n')
