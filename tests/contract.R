library(infercalcR)
invalid <- function(expr) tools::assertError(expr, classes = "infercalcR_invalid_input")
d <- data.frame(row_id = c("a", "b"), estimate = c(2, 3), p.value = c(.01, .04),
                conf.low = c(-Inf, 1), conf.high = c(3, Inf), conf.level = .95)
x <- new_inference_result(d, list(method_id = "fixture", validation = "synthetic contract fixture"))
stopifnot(inherits(x, "inference_result"), is.data.frame(result_data(x)),
  identical(result_data(x)$p.value, d$p.value), identical(result_metadata(x)$kind, "inference"),
  is.infinite(result_data(x)$conf.low[1]), identical(as_inference(x), x))
copy <- x
y <- adjust_inference(x, "holm", family_id = "prespecified", rows = c("a", "b"))
stopifnot(identical(x, copy), isTRUE(all.equal(result_data(y)$p.adjusted, c(.02, .04))),
  identical(result_data(y)$family_size, c(2L, 2L)), identical(result_data(y)$conf.low, d$conf.low))
invalid(adjust_inference(y, "holm", family_id = "again"))
family <- new_inference_result(data.frame(row_id=c("a","b","c"),p.value=c(.01,.02,.03)),
  list(method_id="three-prespecified-tests"))
first <- adjust_inference(family, family_id="primary", rows=c("a","b"))
before <- serialize(first,NULL)
invalid(adjust_inference(first, family_id="primary", rows="c"))
stopifnot(identical(serialize(first,NULL),before))
separate <- adjust_inference(first, family_id="secondary", rows="c")
stopifnot(identical(separate$data$family_id,c("primary","primary","secondary")),
  isTRUE(all.equal(separate$data$p.adjusted,c(.02,.02,.03))))
# Filtering does not erase the complete-family identity retained in metadata.
filtered <- first; filtered$data <- filtered$data[3L,,drop=FALSE]
invalid(adjust_inference(filtered, family_id="primary", rows="c"))
invalid(new_inference_result(data.frame(row_id = c("a", "a")), list(method_id = "test")))
invalid(new_inference_result(data.frame(p.value = 2), list(method_id = "test")))
invalid(new_inference_result(data.frame(n = -1), list(method_id = "test")))
invalid(new_inference_result(data.frame(estimate = NaN), list(method_id = "test")))
invalid(new_inference_result(data.frame(conf.low = 3, conf.high = 2), list(method_id = "test")))
malformed <- x
malformed$metadata$kind <- NULL
invalid(validate_inference(malformed))
invalid(method_spec("not_registered"))
invalid(method_spec("descriptive_summary", options = list(unknown = TRUE)))
invalid(method_spec("descriptive_summary", conf.level = 1))
z <- new_inference_result(data.frame(estimate = NA_real_, p.value = NA_real_,
  status = "undefined", reason = "no observations"), list(method_id = "fixture"))
stopifnot(is.na(result_data(z)$p.reported), result_data(z)$status == "undefined")
empty <- new_inference_result(data.frame(), list(method_id = "empty"))
stopifnot(nrow(result_data(empty)) == 0L, identical(result_data(empty)$row_id, character()))
tmp <- tempfile(); saveRDS(x, tmp); stopifnot(identical(readRDS(tmp), x)); unlink(tmp)
cat("contract checks completed\n")

# Requested levels are strict, while finalized conservative discrete coverage may be 1.
for(level in c(1+.Machine$double.eps,1.1,Inf,-Inf,0,-.1,NaN)){
 invalid(new_inference_result(data.frame(conf.level=level),list(method_id='invalid-level')))
}
for(level in c(1,1.1))invalid(method_spec('rank_sum_mann_whitney',conf.level=level))
conservative<-new_inference_result(data.frame(conf.low=-Inf,conf.high=Inf,conf.level=1),
 list(method_id='conservative-discrete-fixture'))
stopifnot(result_data(conservative)$conf.level==1)
# Match the exact native calls, retaining requested and attained levels separately.
reference_wilcox<-function(...) {
 diagnostics<-character()
 fit<-withCallingHandlers(stats::wilcox.test(...,exact=TRUE,correct=TRUE,conf.int=TRUE,conf.level=.95,digits.rank=Inf),
  warning=function(w){diagnostics<<-c(diagnostics,conditionMessage(w));invokeRestart('muffleWarning')})
 list(fit=fit,diagnostics=diagnostics)
}
check_conservative<-function(value,reference,component){
 rows<-result_data(value);row<-rows[rows$component==component,,drop=FALSE]
 native<-value$payload$native;fit<-reference$fit
 stopifnot(nrow(row)==1L,identical(c(row$conf.low,row$conf.high),as.numeric(fit$conf.int)),
  identical(row$conf.level,as.numeric(attr(fit$conf.int,'conf.level'))),
  row$estimate==unname(fit$estimate),native$p_value==fit$p.value,native$statistic==unname(fit$statistic),
  identical(native$confidence_interval,as.numeric(fit$conf.int)),
  identical(native$attained_confidence_level,attr(fit$conf.int,'conf.level')),
  identical(native$diagnostics,reference$diagnostics),
  native$requested_confidence_level==.95,result_metadata(value)$settings$conf.level==.95)
 invisible(value)
}
rank_x<-c(1,4,6);rank_y<-c(2,3,5,7)
for(alternative in c('two.sided','less','greater')){
 reference<-reference_wilcox(rank_x,rank_y,alternative=alternative)
 result<-run_inference(method_spec('rank_sum_mann_whitney',alternative=alternative,conf.level=.95),
  x=rank_x,y=rank_y,location_shift=TRUE)
 check_conservative(result,reference,'location_shift')
 if(alternative=='two.sided')stopifnot(result$payload$native$attained_confidence_level==1,
  identical(result$payload$native$confidence_interval,c(-Inf,Inf)))
 # These three observations also yield conservative 100% signed-rank intervals.
 reference<-reference_wilcox(rank_x,mu=0,alternative=alternative)
 one<-run_inference(method_spec('signed_rank_wilcoxon_one_sample',alternative=alternative),x=rank_x,mu=0)
 paired<-run_inference(method_spec('signed_rank_wilcoxon',alternative=alternative),
  data=data.frame(id=letters[1:3],x=rank_x+10,y=rep(10,3)))
 for(value in list(one,paired)){
  check_conservative(value,reference,'contrast')
  stopifnot(value$payload$native$attained_confidence_level==1,
   identical(value$payload$native$confidence_interval,c(-Inf,Inf)))
 }
}
cat('PASS conservative attained confidence level 1, exact rank intervals/diagnostics and strict requested-level validation\n')
