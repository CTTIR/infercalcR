# Development-only source extraction. Production code never sources another package.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
src <- normalizePath(args[1L], mustWork = TRUE)
all <- list(); origin <- character()
for (f in list.files(file.path(src,'R'),full.names=TRUE,pattern='[.]R$')) {
 p <- parse(f,keep.source=TRUE); lines <- readLines(f,warn=FALSE)
 for (i in seq_along(p)) {
  e<-p[[i]]
  if(is.call(e)&&as.character(e[[1]])%in%c('<-','=')&&(is.symbol(e[[2]])||is.character(e[[2]]))) {
   n<-as.character(e[[2]]); sr<-attr(p,'srcref')[[i]]
   all[[n]]<-lines[seq.int(sr[1],sr[3])];origin[n]<-basename(f)
  }
 }
}
write_defs<-function(nms,file){stopifnot(all(nms%in%names(all)));writeLines(c('# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.', '# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.','',unlist(lapply(nms,function(n)c(all[[n]],'')))),file)}
whole<-c('boschloo-control.R','boschloo-ordering.R','boschloo-nuisance.R','boschloo-test.R','classif-curves.R','classif-enumerate.R','classif-evaluate.R','classif-metrics.R','classif-plotdata.R','classif-probability.R','classif-result.R','classif-scenario.R','classif-select.R','classif-spec.R','classif-theory.R','classif-uncertainty.R','calc-dist.R','calc-eval.R','calc-functions.R','calc-parse.R','calc-trig.R')
for(f in whole)stopifnot(file.copy(file.path(src,'R',f),file.path('R',f),overwrite=TRUE))
region<-setdiff(names(origin)[origin=='boschloo-region.R'],c('boschloo_power','.bl_power_object','format.boschloo_power','print.boschloo_power'))
plotdata<-c('.bl_plotdata','print.boschloo_plotdata','.bl_pvalues_all','boschloo_plotdata_lattice','boschloo_plotdata_nuisance')
num<-setdiff(names(origin)[origin=='num-dist.R'],c('.ig_critical_symmetric','.ig_critical_values','.ig_two_sided_prob','.ig_power_t_test'))
status<-names(origin)[origin=='core-status.R']
aux<-c('.ig_classification_vocab','.ig_new_classification_spec','.ig_new_observed_test_spec','.ig_check_aux_spec','.ig_classification_spec_problems','.ig_finite_paths','.ig_doubles','.ig_r_literal','.ig_unclass_frozen','.ig_abort_frozen','$<-.ig_frozen','[[<-.ig_frozen','[<-.ig_frozen')
json<-c('.ig_nonfinite_codes','.ig_json_max_bytes','.ig_decimal_pattern','.ig_parse_decimal','.ig_format_double','.ig_nonfinite_marker','.ig_json_atomic_elements','.ig_json_object','.ig_json_value','.ig_canonical_json','.ig_sha256','.ig_hash_value','.ig_json_parse','.ig_json_restore_numbers','.ig_find_duplicate_keys','.ig_is_nonfinite_marker','.ig_decode_nonfinite')
write_defs(region,'R/boschloo-region.R');write_defs(plotdata,'R/boschloo-plotdata.R');write_defs(num,'R/num-dist.R');write_defs(status,'R/legacy-status.R');write_defs(aux,'R/legacy-objects.R');write_defs(json,'R/legacy-json.R');write_defs('.ig_cls_pd_probability','R/classif-probability-data.R')
map<-rbind(data.frame(symbol=names(origin)[origin%in%whole],source_file=unname(origin[origin%in%whole]),target_file=unname(origin[origin%in%whole])),do.call(rbind,lapply(list('boschloo-region.R'=region,'boschloo-plotdata.R'=plotdata,'num-dist.R'=num,'legacy-status.R'=status,'legacy-objects.R'=aux,'legacy-json.R'=json,'classif-probability-data.R'='.ig_cls_pd_probability'),function(ns)data.frame(symbol=ns,source_file=unname(origin[ns]),target_file=''))))
for (f in list.files('R',full.names=TRUE)) {p<-parse(f);for(e in p)if(is.call(e)&&as.character(e[[1]])%in%c('<-','=')&&(is.symbol(e[[2]])||is.character(e[[2]])))map$target_file[map$symbol==as.character(e[[2]])]<-basename(f)}
write.csv(map,'tools/extraction/source-map.csv',row.names=FALSE)
stopifnot(file.copy(file.path(src,'inst/metadata/metric-dictionary.json'),'inst/metadata/metric-dictionary.json',overwrite=TRUE))
doc<-jsonlite::parse_json(paste(readLines(file.path(src,'inst/metadata/metric-dictionary.json'),warn=FALSE),collapse='\n'),simplifyVector=FALSE)
names(doc$metrics)<-vapply(doc$metrics,`[[`,'','id')
cat('# Static metric definitions; generated from inst/metadata/metric-dictionary.json.\n.ig_cls_dictionary_data <- ',file='R/classif-dictionary-data.R');con<-file('R/classif-dictionary-data.R',open='a');dput(doc,file=con);close(con)
