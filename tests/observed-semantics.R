library(infercalcR)
err <- function(expr, class='infercalcR_invalid_input') {
  value<-tryCatch(force(expr),error=identity)
  stopifnot(inherits(value,class))
}
x<-c(2,4,5,7,9);y<-c(1,3,4,6,8,10)
fit<-run_inference(method_spec('t_means_two_independent_welch'),x=x,y=y)
d<-result_data(fit);ref<-stats::t.test(x,y,var.equal=FALSE)
stopifnot(isTRUE(all.equal(d$p.value,ref$p.value,tolerance=1e-14)),
 isTRUE(all.equal(d$estimate,mean(x)-mean(y),tolerance=1e-14)),d$n==11,
 isTRUE(all.equal(c(d$conf.low,d$conf.high),as.numeric(ref$conf.int),tolerance=1e-14)))
reverse<-result_data(run_inference(method_spec('t_means_two_independent_welch'),x=y,y=x))
stopifnot(isTRUE(all.equal(reverse$estimate,-d$estimate)),isTRUE(all.equal(reverse$p.value,d$p.value)))
one<-result_data(run_inference(method_spec('t_mean_one_sample','greater',.9),x=x,mu0=3))
stopifnot(is.infinite(one$conf.high),one$conf.high>0,one$null.value==0,one$estimate==mean(x)-3)
paired<-data.frame(id=c('c','a','b','d','e'),x=x,y=y[1:5]+c(0,1,0,2,0))
p1<-result_data(run_inference(method_spec('t_means_paired'),data=paired))
p2<-result_data(run_inference(method_spec('t_means_paired'),data=paired[5:1,]))
stopifnot(isTRUE(all.equal(p1,p2,tolerance=1e-14)))
err(method_spec('made_up'))
err(method_spec('rank_sum_mann_whitney',options=list(exact='TRUE')))
err(method_spec('gee_logistic_exchangeable',options=list(missing_outcomes='available_case_MAR')))
err(method_spec('chisq_goodness_of_fit_mc',options=list(seed=-1)))
err(method_spec('t_means_paired',options=list(var.equal=TRUE)))
err(run_inference(method_spec('t_means_paired'),data=paired,unknown=1))
err(run_inference(method_spec('t_means_paired',missing='omit'),data=paired))
err(run_inference(method_spec('prediction_model_linear','greater'),data=data.frame()))
err(run_inference(method_spec('variance_chisq_one_sample'),x=x),'infercalcR_unsupported')
rank<-run_inference(method_spec('rank_sum_mann_whitney'),x=c(2,4,6,8),y=c(1,3,5,7))
stopifnot(all(is.na(result_data(rank)$conf.low)),result_data(rank)$estimate==.625)
shift<-run_inference(method_spec('rank_sum_mann_whitney',options=list(location_shift=TRUE)),x=c(2,4,6,8,10),y=c(1,3,5,7,9))
stopifnot(nrow(result_data(shift))==2L,is.na(result_data(shift)$conf.low[1L]),is.finite(result_data(shift)$conf.low[2L]))
# Sign-probability intervals must not be associated with the marginal difference.
sign<-run_inference(method_spec('sign_test_one_sample'),x=c(-2,-1,0,1,2,3),mu0=0)
sd<-result_data(sign);stopifnot(sd$estimate==3/5,sd$n==5,sd$n.excluded==1,grepl('conditional',sd$estimand),sign$payload$native$n_ties==1)
mc<-result_data(run_inference(method_spec('mcnemar_exact_conditional'),counts=c(n00=20,n01=3,n10=9,n11=15)))
stopifnot(mc$estimate==.75,mc$n==12,grepl('discordance',mc$estimand),mc$null.value==.5)
bin<-result_data(run_inference(method_spec('binomial_exact_one_sample'),events=0,n=10,p0=.5))
stopifnot(bin$estimate==0,isTRUE(all.equal(bin$p.value,2/1024,tolerance=1e-15)),bin$conf.low==0)
# Same source probabilities, explicitly separate Monte Carlo and sampling uncertainty.
set.seed(715);rng<-.Random.seed
sim<-run_inference(method_spec('chisq_goodness_of_fit_mc'),observed=c(a=1,b=3,c=6),probabilities=c(a=.2,b=.3,c=.5),seed=42,B=999L)
stopifnot(identical(rng,.Random.seed),all(is.na(result_data(sim)$conf.low)),length(sim$payload$native$monte_carlo_interval)==2L)
# Exact nuisance optimization bounds cannot enter CI columns.
z<-run_inference(method_spec('unconditional_exact_z_pooled'),table=matrix(c(4,1,1,4),2,byrow=TRUE),independent_binomial_groups=TRUE)
zd<-result_data(z);stopifnot(all(is.na(zd$conf.low)),all(is.na(zd$conf.high)),is.finite(zd$p.upper[1L]),zd$p.lower[1L]<=zd$p.upper[1L])
# Descriptions retain numeric precision and suppress any internal test p-values.
desc<-run_inference(method_spec('descriptive_summary'),x=factor(c('a','a','b'),levels=c('a','b')),measurement='binary',event='a')
stopifnot(result_data(desc)$estimate==2/3,all(is.na(result_data(desc)$p.value)),result_data(desc)$n==3)
fixtures<-readRDS(system.file('methods','synthetic-baseline.rds',package='infercalcR'))
invoke<-function(id) {a<-fixtures[[id]]$arguments;a[c('alternative','conf.level')]<-NULL;do.call(run_inference,c(list(spec=method_spec(id)),a))}
factorial<-invoke('anova_factorial');fd<-result_data(factorial)
stopifnot(nrow(fd)==3,all(fd$family_size==3),all(fd$p.method=='holm'),
 isTRUE(all.equal(fd$p.adjusted,stats::p.adjust(fd$p.value,'holm'),tolerance=1e-14)))
err(adjust_inference(factorial,family_id='again'))
ancova<-invoke('ancova_baseline_adjusted');ad<-result_data(ancova)
stopifnot(sum(ad$component=='omnibus')==1,sum(ad$component=='contrast')==3,sum(ad$component=='adjusted_mean')==3,
 all(is.na(ad$p.adjusted)),all(is.na(ad$p.value[ad$component=='adjusted_mean'])))
logit<-invoke('regression_logistic_binary');ld<-result_data(logit)
slopes<-ld[ld$component=='coefficient' & ld$contrast!='(Intercept)',]
stopifnot(all(grepl('profile',slopes$interval.method)),all(is.finite(slopes$conf.low)),
 all(is.na(ld$conf.low[ld$component=='omnibus'])),nrow(slopes)==2L)
no_variance<-run_inference(method_spec('brunner_munzel'),x=rep(1,10),y=rep(1,10))
stopifnot(result_data(no_variance)$status=='not_applicable',is.na(result_data(no_variance)$p.value),
 identical(result_metadata(no_variance)$source_status,'unavailable'))
cat('PASS observed method semantics, known values, matched stats adapters and family/interval invariants\n')
