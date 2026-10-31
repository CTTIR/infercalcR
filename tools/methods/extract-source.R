# Development-only extraction from the recorded inferguidR source checkout.
# Installed code contains ordinary functions; it never reads advisor scripts.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
source_root <- normalizePath(args[1L], mustWork = TRUE)
files <- list.files(file.path(source_root, 'inst/advisor/templates'), '[.]R$', full.names=TRUE)
stopifnot(length(files)==68L)
info <- list()
data_arguments <- c('data','x','y','counts','table','events','n','observed','tuning','evaluation','tp','fn','fp','tn')
for (file in files) {
 id <- sub('^tpl[.]|[.]v1[.]R$', '', basename(file))
 id <- sub('[.]v1[.]R$', '', id)
 code <- parse(file, keep.source=FALSE)
 definitions <- Filter(function(x) is.call(x) && identical(x[[1L]],as.name('<-')) &&
   is.call(x[[3L]]) && identical(x[[3L]][[1L]],as.name('function')) &&
   as.character(x[[2L]])!='plot_example', as.list(code))
 names <- vapply(definitions,function(x)as.character(x[[2L]]),'')
 prefix <- paste0('.obs_',id,'_')
 replacements <- stats::setNames(paste0(prefix,names), names)
 rename <- function(x) {
  if(is.symbol(x)) {
   key<-as.character(x); if(key %in% names(replacements)) return(as.name(replacements[[key]]))
  } else if(is.call(x)) {
   if(identical(x[[1L]],as.name('::')) && identical(x[[2L]],as.name('inferguidR'))) return(x[[3L]])
   for(i in seq_along(x)) if(!identical(x[[i]],quote(expr=))) x[i]<-list(rename(x[[i]]))
   if(identical(x[[1L]],as.name('boschloo_test'))) {
    x[[1L]]<-as.name('boschloo_inference')
    x<-call('$',call('$',x,as.name('payload')),as.name('native'))
   }
  } else if(is.pairlist(x)) {
   z<-as.list(x)
   for(i in seq_along(z)) if(!identical(z[[i]],quote(expr=))) z[i]<-list(rename(z[[i]]))
   x<-as.pairlist(z)
  }
  x
 }
 rewritten <- lapply(definitions,rename)
 lines <- c('# Numerical definitions extracted from the existing implementation.',
   paste0('# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, ',basename(file)),
   '# Example execution and plotting are deliberately outside this module.', '',
   unlist(lapply(rewritten,function(x)c(deparse(x,width.cutoff=100L),''))))
 if(id=='gee_logistic_exchangeable') lines<-gsub('    glm <- stats::glm', '    cluster <- d$cluster\n    glm <- stats::glm', lines, fixed=TRUE)
 writeLines(lines,file.path('R',paste0('observed-native-',id,'.R')))
 original <- definitions[[match('analyse',names)]][[3L]]
 formals <- names(original[[2L]])
 source_lines<-readLines(file,warn=FALSE)
 code_lines<-source_lines[!grepl('^\\s*#',source_lines)]
 packages<-unique(unlist(regmatches(code_lines,gregexpr('[A-Za-z][A-Za-z0-9.]*::',code_lines))))
 packages<-sort(setdiff(sub('::$','',packages),c('inferguidR','graphics')))
 info[[id]] <- list(method_id=id,backend=paste(packages,collapse=','),
   arguments=formals, options=setdiff(formals,c(data_arguments,'alternative','conf.level')),
   has_alternative='alternative'%in%formals,
   hypothesis=!grepl('^(descriptive|prediction|pca|cluster|classifier|diagnostic|efa)',id),
   helpers=names)
}
registry <- c('# Explicit registry: ordinary function references, no namespace discovery.',
 '.ic_method_info <-',capture.output(dput(info)), '',
 '.ic_method_function <- function(id) {', '  switch(id,',
 paste0('    ',names(info),' = .obs_',names(info),'_analyse',c(rep(',',length(info)-1L),'')),
 '  )','}')
writeLines(registry,'R/observed-registry.R')
write.csv(data.frame(method_id=names(info),backend=vapply(info,`[[`,'','backend'),
 signature=vapply(info,function(x)paste(x$arguments,collapse=', '),'')),
 'inst/methods/implemented.csv',row.names=FALSE)
cat('Extracted',length(info),'methods without example execution.\n')
