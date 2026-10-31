# Development-time generator. Run with the unchanged baseline source and an output directory.
args<-commandArgs(TRUE);stopifnot(length(args)==2L)
source_root<-normalizePath(args[1L],mustWork=TRUE);output<-args[2L]
dir.create(output,recursive=TRUE,showWarnings=FALSE)
files<-list.files(file.path(source_root,'inst/advisor/templates'),'[.]R$',full.names=TRUE)
for(file in files){
 id<-sub('^tpl[.]','',sub('[.]v1[.]R$','',basename(file)))
 lines<-readLines(file,warn=FALSE)
 if(grepl('^(classifier_|diagnostic_)',id)){
  # These templates already delegate calculations and construct public classifier specs.
  writeLines(sub('[[:blank:]]+$','',gsub('inferguidR::','infercalcR::',lines,fixed=TRUE)),file.path(output,basename(file)))
  next
 }
 parsed<-parse(file,keep.source=TRUE);refs<-attr(parsed,'srcref');changes<-list()
 for(i in seq_along(parsed)){
  expr<-parsed[[i]]
  if(!is.call(expr)||!identical(expr[[1]],as.name('<-'))||!is.call(expr[[3]])||!identical(expr[[3]][[1]],as.name('function')))next
  name<-as.character(expr[[2]]);f<-expr[[3]];formal_names<-names(f[[2]])
  if(name=='analyse'){
   alt<-if('alternative'%in%formal_names&&!grepl('^prediction_',id))'alternative' else '"two.sided"'
   conf<-if('conf.level'%in%formal_names)'conf.level' else '.95'
   inputs<-setdiff(formal_names,c('alternative','conf.level'))
   body<-sprintf('{ spec <- infercalcR::method_spec("%s", alternative = %s, conf.level = %s); infercalcR::run_inference(spec, %s)$payload$native }',id,alt,conf,paste(paste0(inputs,' = ',inputs),collapse=', '))
   f[[3]]<-parse(text=body)[[1]]
   if(grepl('^prediction_',id))f[[3]]<-as.call(c(list(as.name('{')),list(quote(if(!is.null(alternative))stop('This procedure has no hypothesis alternative'))),as.list(f[[3]])[-1L]))
   expr[[3]]<-f;replacement<-deparse(expr,width.cutoff=100L)
  }else if(name=='signed_analysis'){
   f[[3]]<-quote({spec<-infercalcR::method_spec('signed_rank_wilcoxon_one_sample',alternative,conf.level);infercalcR::run_inference(spec,x=x,mu=mu,exact=exact)$payload$native})
   expr[[3]]<-f;replacement<-deparse(expr,width.cutoff=100L)
  }else if(name=='parallel_analysis'){
   body<-paste0('{ infercalcR::parallel_analysis(',paste(paste0(formal_names,' = ',formal_names),collapse=', '),')$payload$native }')
   f[[3]]<-parse(text=body)[[1]];expr[[3]]<-f;replacement<-deparse(expr,width.cutoff=100L)
  }else if(name=='predict_new'){
   body<-sprintf('{ common <- infercalcR::new_inference_result(data.frame(), list(method_id = "%s"), payload = list(native = result)); value <- infercalcR::predict_inference(common, newdata, interval = interval, conf.level = conf.level)$payload$native; if (identical(interval, "none")) value$estimate else value }',id)
   # Resolve the original vector default before passing to a public scalar interface.
   f[[3]]<-parse(text=body)[[1]];f[[3]]<-as.call(c(list(as.name('{')),list(quote(interval<-match.arg(interval))),as.list(f[[3]])[-1L]));expr[[3]]<-f;replacement<-deparse(expr,width.cutoff=100L)
  }else if(name%in%c('check_settings','plot_example')) next else replacement<-character()
  changes[[length(changes)+1L]]<-list(start=refs[[i]][1],end=refs[[i]][3],replacement=replacement)
 }
 for(change in rev(changes)){
  before<-if(change$start>1L)lines[seq_len(change$start-1L)] else character()
  after<-if(change$end<length(lines))lines[seq.int(change$end+1L,length(lines))] else character()
  lines<-c(before,change$replacement,after)
 }
 writeLines(sub('[[:blank:]]+$','',lines),file.path(output,basename(file)))
}
cat('Wrote',length(files),'delegating templates to',output,'\n')
