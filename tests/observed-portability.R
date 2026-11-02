# Equivalence policies must accept whole-basis changes and reject numerical regressions.
library(infercalcR)
paths<-c('support/observed-portability.R','tests/support/observed-portability.R',system.file('tests','support','observed-portability.R',package='infercalcR'))
source(paths[file.exists(paths)][1L])
f<-readRDS(system.file('methods','synthetic-baseline.rds',package='infercalcR'))
reject<-function(expr)stopifnot(inherits(tryCatch(force(expr),error=identity),'error'))
id<-'efa_common_factor_ml';n<-f[[id]]$expected;p<-c(2L,1L);s<-c(-1,1)
n$loadings<-sweep(n$loadings[,p],2L,s,'*');n$factor_correlation<-n$factor_correlation[p,p]*outer(s,s)
.port_native(n,f[[id]]$expected,id,f[[id]]$arguments)
n$loadings[1,1]<-n$loadings[1,1]+.001;reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
n<-f[[id]]$expected;n$loadings[,1]<--n$loadings[,1]
reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
id<-'cluster_hierarchical_ward';n<-f[[id]]$expected;n$fit$tree$merge<-n$fit$tree$merge[,2:1]
.port_native(n,f[[id]]$expected,id,f[[id]]$arguments)
n$fit$tree$height[1]<-n$fit$tree$height[1]+.001;reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
id<-'regression_multinomial';n<-f[[id]]$expected;.port_native(n,n,id,f[[id]]$arguments)
n$coefficient_table$p_value[1]<-n$coefficient_table$p_value[1]+1e-4;reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
id<-'glmm_logistic_random_intercept';n<-f[[id]]$expected
.port_glmm_math(n,n,f[[id]]$arguments,id)
n$p_value<-n$p_value+1e-4;reject(.port_glmm_math(n,f[[id]]$expected,f[[id]]$arguments,id))
id<-'efa_common_factor_ml';n<-f[[id]]$expected;rownames(n$loadings)[1]<-'wrong variable'
reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
id<-'cluster_hierarchical_ward';n<-f[[id]]$expected;n$fit$tree$order<-rev(n$fit$tree$order)
reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
id<-'regression_multinomial';n<-f[[id]]$expected;attr(n$coefficient_table,'row.names')<-rev(attr(n$coefficient_table,'row.names'))
reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
id<-'t_mean_one_sample';n<-f[[id]]$expected;n$p_value<-n$p_value+1e-9
reject(.port_native(n,f[[id]]$expected,id,f[[id]]$arguments))
cat('PASS coupled basis/tree equivalence and deliberate numerical-regression rejection\n')
