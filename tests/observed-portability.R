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

# The normal-tail derivative can amplify accepted z roundoff in a small p-value.
# It cannot excuse an altered p or primitive drift above the existing tolerance.
id<-'regression_ordinal_proportional_odds';ordinal<-f[[id]]$expected
.port_native(ordinal,ordinal,id,f[[id]]$arguments)
coherent_ordinal<-function(beta=ordinal$coefficient_table$estimate,se=ordinal$coefficient_table$std_error){
 n<-ordinal;t<-n$coefficient_table;critical<-stats::qnorm((1-n$confidence_level)/2,lower.tail=FALSE)
 t$estimate<-beta;t$std_error<-se;t$z<-beta/se;t$p_value<-2*stats::pnorm(abs(t$z),lower.tail=FALSE)
 t$lower<-beta-critical*se;t$upper<-beta+critical*se;t$odds_ratio<-exp(beta)
 t$ratio_lower<-exp(t$lower);t$ratio_upper<-exp(t$upper)
 attributes(t)<-attributes(ordinal$coefficient_table);n$coefficient_table<-t;n
}
n<-coherent_ordinal(beta=ordinal$coefficient_table$estimate+5.06e-13*ordinal$coefficient_table$std_error)
.port_native(n,ordinal,id,f[[id]]$arguments)
for(delta in c(1e-10,1e-15)){
 n<-ordinal;n$coefficient_table$p_value<-n$coefficient_table$p_value+delta
 attributes(n$coefficient_table)<-attributes(ordinal$coefficient_table)
 reject(.port_native(n,ordinal,id,f[[id]]$arguments))
}
n<-coherent_ordinal(beta=ordinal$coefficient_table$estimate*(1+2e-12))
reject(.port_native(n,ordinal,id,f[[id]]$arguments))
n<-coherent_ordinal(se=ordinal$coefficient_table$std_error*(1+2e-12))
reject(.port_native(n,ordinal,id,f[[id]]$arguments))
n<-ordinal;attr(n$coefficient_table,'unexpected')<-TRUE
reject(.port_native(n,ordinal,id,f[[id]]$arguments))
cat('PASS ordinal propagated Wald p roundoff and strict primitive rejection\n')
