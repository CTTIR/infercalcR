# Explicit providers used by the guidance package. No private-name dispatcher.
#' Numerical design kernel for a Boschloo procedure
#' @export
boschloo_design_kernel <- function(n1, n2, alpha = .05, variant,
                                    control = boschloo_control()) {
  .bl_check_design(n1,n2); alpha <- .bl_check_alpha(alpha)
  control <- .bl_check_control(control)
  .bl_region(n1,n2,alpha,.bl_variant(if(missing(variant))NULL else variant),control)
}
#' Probability of a finite two-binomial rejection region
#' @export
binomial_region_probability <- function(indices, n1, n2, p1, p2) {
  .bl_check_design(n1,n2);p1<-.bl_check_prob(p1,"p1");p2<-.bl_check_prob(p2,"p2")
  if(!is.numeric(indices)||anyNA(indices)||any(!is.finite(indices))||any(indices!=floor(indices))||
     any(indices<1|indices>(n1+1)*(n2+1))||anyDuplicated(indices)) .ig_abort_invalid("indices must be distinct table indices in the design")
  .bl_power_idx(indices,as.integer(n1),as.integer(n2),p1,p2)
}
binomial_table_space <- function(n1,n2) { .bl_check_design(n1,n2);.bl_tables(as.integer(n1),as.integer(n2)) }
boschloo_region_record <- function(kernel,control=boschloo_control()) {
  if(!is.list(kernel)||!all(c("n1","n2","alpha","info","idx","member","size","sides")%in%names(kernel))) .ig_abort_invalid("kernel must come from boschloo_design_kernel()")
  .bl_region_object(kernel,.bl_check_control(control))
}
boschloo_kernel_status <- function(kernel) .bl_region_status(kernel)
boschloo_control_record <- function(control) .bl_control_record(.bl_check_control(control))
boschloo_control_from_record <- function(record) .bl_control_from_record(record)
boschloo_validate_control <- function(control) .bl_check_control(control)
boschloo_validate_design <- function(n1,n2) .bl_check_design(n1,n2)
boschloo_variant_details <- function(variant=NULL) {
  if(is.null(variant)) return(.bl_variant_info)
  .bl_variant(variant)
}
boschloo_variant_record <- function(info) {
  if(is.character(info))info<-.bl_variant(info)
  if(!is.list(info)||is.null(info$variant)) .ig_abort_invalid("info must identify a Boschloo variant")
  .bl_variant_record(.bl_variant(info$variant))
}
boschloo_engine_info <- function() list(provenance=.bl_provenance(),evidence=.bl_evidence,
  max_tables=.bl_max_tables,exact_ordering_max_total=.bl_exact_max_n)
boschloo_control_call <- function(control) .bl_control_call(.bl_check_control(control))
fisher_ordering_probabilities <- function(n1,n2,variant,control=boschloo_control(),convention="same") {
  .bl_check_design(n1,n2)
  if(!convention%in%c("same","minlike")||length(convention)!=1L) .ig_abort_invalid("convention must be same or minlike")
  .bl_fisher_p(n1,n2,.bl_variant(variant),.bl_check_control(control),convention)
}
boschloo_plot_data_record <- function(plot_id,title,data,fixed,scope,reference=list(),notes=character(),status=ig_status("ok")) .bl_plotdata(plot_id,title,data,fixed,scope,reference,notes,status)

#' Classifier metadata and validation providers
#' @export
classification_dictionary <- function() .ig_cls_dictionary()
classification_contract <- function() list(vocabulary=.ig_classification_vocab,
  metric_statuses=.ig_cls_metric_statuses,interval_statuses=.ig_cls_interval_statuses,
  table_columns=.ig_cls_table_columns,bootstrap_curve_ids=.ig_cls_bootstrap_curve_ids,
  constraint_names=.ig_cls_constraint_names,plot_families=.ig_cls_families)
classification_reason <- function(code,lang="en") .ig_cls_reason_text(code,lang)
classification_metric_ids <- function() .ig_cls_operating_ids()
classification_short_label <- function(id,lang="en") .ig_cls_short_label(id,lang)
classification_check_thresholds <- function(threshold_probability) .ig_cls_check_thresholds(threshold_probability)
classification_check_bins <- function(calibration_bins) .ig_cls_check_bins(calibration_bins)
classification_check_partial <- function(partial_auc) .ig_cls_check_partial(partial_auc)

#' Canonical historical data primitives
#' @export
legacy_canonical_json <- function(x) .ig_canonical_json(x)
legacy_hash_value <- function(x) .ig_hash_value(x)
legacy_sha256 <- function(text) .ig_sha256(text)
legacy_parse_json <- function(text) .ig_json_parse(text)
legacy_format_double <- function(x) .ig_format_double(x)
legacy_parse_decimal <- function(text) .ig_parse_decimal(text)
legacy_double_values <- function(x) .ig_doubles(x)
legacy_nonfinite_paths <- function(x,path="") .ig_finite_paths(x,path)
legacy_r_literal <- function(x) .ig_r_literal(x)
legacy_unclass_frozen <- function(x) .ig_unclass_frozen(x)
legacy_check_spec <- function(spec,kind) .ig_check_aux_spec(spec,kind)
legacy_json_limits <- function() list(max_bytes=.ig_json_max_bytes)

#' Probability kernels with explicit tail and logarithmic semantics
#' @export
normal_probability <- function(q,mean=0,sd=1,lower_tail=TRUE,log_p=FALSE) .ig_p_norm(q,mean,sd,lower_tail,log_p)
normal_quantile <- function(p,mean=0,sd=1,lower_tail=TRUE,log_p=FALSE) .ig_q_norm(p,mean,sd,lower_tail,log_p)
t_probability <- function(q,df,ncp=0,lower_tail=TRUE,log_p=FALSE) .ig_p_t(q,df,ncp,lower_tail,log_p)
t_quantile <- function(p,df,ncp=0,lower_tail=TRUE,log_p=FALSE) .ig_q_t(p,df,ncp,lower_tail,log_p)
chisq_probability <- function(q,df,ncp=0,lower_tail=TRUE,log_p=FALSE) .ig_p_chisq(q,df,ncp,lower_tail,log_p)
chisq_quantile <- function(p,df,ncp=0,lower_tail=TRUE,log_p=FALSE) .ig_q_chisq(p,df,ncp,lower_tail,log_p)
f_probability <- function(q,df1,df2,ncp=0,lower_tail=TRUE,log_p=FALSE) .ig_p_f(q,df1,df2,ncp,lower_tail,log_p)
f_quantile <- function(p,df1,df2,ncp=0,lower_tail=TRUE,log_p=FALSE) .ig_q_f(p,df1,df2,ncp,lower_tail,log_p)
binomial_probability <- function(q,size,prob,lower_tail=TRUE,log_p=FALSE) .ig_p_binom(q,size,prob,lower_tail,log_p)
log_probability_add <- function(a,b) .ig_logspace_add(a,b)
log_probability_sum <- function(x) .ig_logspace_sum(x)
invert_log_cdf <- function(logcdf,p,lower_tail,log_p,support,start) .ig_invert_cdf(logcdf,p,lower_tail,log_p,support,start)
f_lower_log_bound <- function(q,df1,df2,ncp) .ig_f_lower_log_bound(q,df1,df2,ncp)
noncentral_t_density <- function(x,df,ncp) .ig_calc_nct_pdf(x,df,ncp)
