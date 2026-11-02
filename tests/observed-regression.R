library(infercalcR)
helper_candidates<-c('support/observed-portability.R','tests/support/observed-portability.R',system.file('tests','support','observed-portability.R',package='infercalcR'))
source(helper_candidates[file.exists(helper_candidates)][1L])
fixtures <- readRDS(system.file('methods','synthetic-baseline.rds',package='infercalcR'))
full <- identical(Sys.getenv('INFER_SUITE_FULL'), 'true')
executed <- character(); skipped <- character(); results <- list()
for (id in names(fixtures)) {
  if (grepl('[.]predict_new$|[.]parallel_analysis$', id)) next
  cat("CHECK native",id,"\n")
  fixture <- fixtures[[id]]
  linked <- grepl('^(boschloo_|classifier_|diagnostic_)', id)
  if (linked && !full) { skipped <- c(skipped,id); next }
  catalogue <- method_catalogue()
  pkgs <- strsplit(catalogue$backend[match(id,catalogue$method_id)],',',fixed=TRUE)[[1L]]
  absent <- pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
  if (length(absent)) {
    if (full) stop('Required feature backend absent: ',paste(absent,collapse=', '))
    skipped <- c(skipped,id);next
  }
  args <- fixture$arguments
  alt <- if (is.null(args$alternative)) 'two.sided' else args$alternative
  level <- if (is.null(args$conf.level)) .95 else args$conf.level
  args[c('alternative','conf.level')] <- NULL
  spec <- method_spec(id, alternative=alt, conf.level=level)
  attached <- search(); directory <- getwd(); devices <- grDevices::dev.list()
  before <- serialize(args,NULL); rng <- if(exists('.Random.seed',.GlobalEnv)) get('.Random.seed',.GlobalEnv) else NULL
  output <- do.call(run_inference,c(list(spec=spec),args))
  stopifnot(inherits(output,'inference_result'),is.data.frame(result_data(output)),nrow(result_data(output))>0L,
    identical(serialize(args,NULL),before),identical(result_metadata(output)$method_id,id),
    identical(search(),attached),identical(getwd(),directory),identical(grDevices::dev.list(),devices))
  if (!linked) .port_native(output$payload$native,fixture$expected,id,args)
  if (grepl('prediction_|cluster_|jonckheere_|chisq_goodness_of_fit_mc',id)) {
    current <- if(exists('.Random.seed',.GlobalEnv)) get('.Random.seed',.GlobalEnv) else NULL
    stopifnot(identical(rng,current))
  }
  results[[id]] <- output;executed <- c(executed,id)
}
parallel <- fixtures[['efa_common_factor_ml.parallel_analysis']]
if (!is.null(parallel)) {
  value <- do.call(parallel_analysis,parallel$arguments)
  stopifnot(isTRUE(all.equal(value$payload$native,parallel$expected,tolerance=1e-12)))
  executed <- c(executed,'efa_common_factor_ml.parallel_analysis')
}
for(id in c('prediction_model_linear','prediction_model_logistic')) {
  helper<-fixtures[[paste0(id,'.predict_new')]]
  if(is.null(helper)||is.null(results[[id]])) stop('Missing prediction helper prerequisite')
  value<-do.call(predict_inference,c(list(result=results[[id]]),helper$arguments))
  stopifnot(isTRUE(all.equal(value$payload$native,helper$expected,tolerance=1e-12)))
  executed<-c(executed,paste0(id,'.predict_new'))
}
stopifnot(length(executed)>=50L)
if(full) stopifnot(length(executed)==71L,!length(skipped))
cat('PASS observed baseline regression:',length(executed),'capabilities; SKIP:',length(skipped),paste(skipped,collapse=', '),'\n')

evidence<-Sys.getenv('INFER_SUITE_EVIDENCE')
if(nzchar(evidence))utils::write.csv(data.frame(method_id=executed,executions=1L,result='PASS'),evidence,row.names=FALSE)
