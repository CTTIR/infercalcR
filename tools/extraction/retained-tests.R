# Retained numerical regression checks against an installed calc candidate.
# Source test files remain unchanged in guid until integrated coverage passes.
args<-commandArgs(TRUE);stopifnot(length(args)==2L)
src<-normalizePath(args[1],mustWork=TRUE);out<-args[2];dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(requireNamespace('testthat',quietly=TRUE))
files<-c('test-calc-fixtures.R','test-calc-functions.R','test-calc-parser.R','test-calc-trig-domain.R','test-calc-trig.R','test-classif-compare.R','test-classif-independent.R','test-classif-metrics.R','test-classif-theory.R')
for(f in files){txt<-readLines(file.path(src,'tests/testthat',f),warn=FALSE);txt<-gsub('inferguidR:::','infercalcR:::',txt,fixed=TRUE);txt<-gsub('inferguidR::','infercalcR::',txt,fixed=TRUE);writeLines(txt,file.path(out,f))}
dir.create(file.path(out,'fixtures'),showWarnings=FALSE)
stopifnot(file.copy(file.path(src,'tests/testthat/fixtures/calculator-trig-domain-independent.csv'),file.path(out,'fixtures/calculator-trig-domain-independent.csv'),overwrite=TRUE))
r<-testthat::test_dir(out,package='infercalcR',load_package='installed',reporter='summary',stop_on_failure=FALSE)
d<-as.data.frame(r);write.csv(d,file.path(out,'results.csv'),row.names=FALSE)
stopifnot(nrow(d)>0,sum(d$failed)==0,sum(d$error)==0)
cat('PASS retained numerical regression:',nrow(d),'test blocks;',sum(d$nb),'expectations;',sum(d$skipped),'skips.\n')
