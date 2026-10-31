from pathlib import Path
import re
root=Path('R')
for pat in ('boschloo*.R','classif*.R','calc*.R'):
 for p in root.glob(pat):
  s=p.read_text().replace('inferguidR::','infercalcR::')
  p.write_text(s)
p=root/'boschloo-control.R';s=p.read_text().replace('engine = \"inferguidR Boschloo engine\"','engine = \"infercalcR Boschloo engine\"');p.write_text(s)
p=root/'legacy-status.R';s=p.read_text();s=s.replace('class = c(paste0("inferguidR_", code), "inferguidR_error", "error", "condition")','class = c(paste0("inferguidR_", code), paste0("infercalcR_", code),\n              "infercalcR_error", "inferguidR_error", "error", "condition")');p.write_text(s)
p=root/'legacy-json.R';s=p.read_text()
for name,args in [('.ig_parse_decimal','txt'),('.ig_json_atomic_elements','x, path'),('.ig_json_parse','txt')]:
 s=s.replace(f'{name} <- function({args}) {{',f'{name} <- function({args}) {{\n  .ic_legacy_need("jsonlite")')
s=s.replace('.ig_sha256 <- function(txt) {','.ig_sha256 <- function(txt) {\n  .ic_legacy_need("digest")');p.write_text(s)
# Cache keys are implementation details, not legacy canonical text or identities.
for name in ('boschloo-control.R','boschloo-region.R'):
 p=root/name;s=p.read_text().replace('.ig_format_double(','.bl_cache_number(');p.write_text(s)
p=root/'classif-metrics.R';s=p.read_text().replace('package = "inferguidR"','package = "infercalcR"')
start=s.index('.ig_cls_dictionary <- function() {');end=s.index('\n.ig_cls_metric_entry <-',start)
s=s[:start]+'.ig_cls_dictionary <- function() .ig_cls_dictionary_data\n'+s[end:];p.write_text(s)
p=root/'boschloo-test.R';s=p.read_text();start=s.index('boschloo_test <- function(');end=s.index('\n# p-value of one side',start)
body=s[start:end].replace('boschloo_test <- function(','.bl_test_result <- function(',1).replace('control = boschloo_control()) {','control = boschloo_control(), identity = TRUE) {',1)
body=body.replace('spec <- .ig_new_observed_test_spec(info$variant_id, inputs = inputs,','spec <- if (identity) .ig_new_observed_test_spec(info$variant_id, inputs = inputs,',1).replace('.bl_control_record(control)))','.bl_control_record(control))) else NULL',1)
body=body.replace('r_call = .bl_test_call(x1, n1, x2, n2, info$variant, alpha, groups, event, control)','r_call = if (identity) .bl_test_call(x1, n1, x2, n2, info$variant, alpha, groups, event, control) else NULL')
wrapper='''boschloo_test <- function(x1, n1, x2, n2, variant, alpha = NULL,
                          groups = c("group 1", "group 2"), event = "event",
                          control = boschloo_control()) {
  .bl_test_result(x1, n1, x2, n2, if (missing(variant)) NULL else variant,
                  alpha, groups, event, control, identity = TRUE)
}

'''
s=s[:start]+wrapper+body+s[end:];p.write_text(s)
p=root/'classif-result.R';s=p.read_text().replace('format.classification_result <- function(x, ...) {','format.classification_result <- function(x, ...) {\n  if (inherits(x, "inference_result")) return(utils::capture.output(print.inference_result(x)))').replace('print.classification_result <- function(x, ...) {','print.classification_result <- function(x, ...) {\n  if (inherits(x, "inference_result")) return(print.inference_result(x))');p.write_text(s)

p=root/'classif-dictionary-data.R';s=p.read_text();p.write_text(''.join(c if ord(c)<128 else '\\u%04x'%ord(c) for c in s))
