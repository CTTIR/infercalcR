# Test-only numerical policies. Frozen fixtures remain unchanged.
# Exact adapter mapping is tested separately against the current finalized payload.
.port_equal <- function(a,b,label,tolerance=1e-12) {
 difference<-all.equal(a,b,tolerance=tolerance,check.environment=FALSE)
 if(!isTRUE(difference))stop(label,': ',paste(difference,collapse='; '),call.=FALSE)
}
.port_delta <- function(a,b,label,bound=1e-12) {
 if(!identical(dim(a),dim(b))||length(a)!=length(b)||!all(is.finite(a))||!all(is.finite(b)))stop(label,': incompatible dimensions or nonfinite values',call.=FALSE)
 error<-if(length(a))max(abs(a-b)) else 0
 if(error>bound)stop(label,': maximum absolute delta ',format(error,digits=17),' exceeds ',bound,call.=FALSE)
}
.port_align <- function(a,b) {
 # Only whole-column sign/permutation equivalence; no elementwise absolute values.
 k<-ncol(a)
 permutations<-function(v)if(length(v)==1L)matrix(v,1L) else do.call(rbind,lapply(v,function(i)cbind(i,permutations(setdiff(v,i)))))
 choices<-permutations(seq_len(k))
 quality<-apply(choices,1L,function(p)sum(abs(colSums(a[,p,drop=FALSE]*b))))
 p<-choices[which.max(quality),];signs<-sign(colSums(a[,p,drop=FALSE]*b));signs[signs==0]<-1
 list(order=p,sign=signs)
}
.port_clades <- function(tree) {
 nodes<-vector('list',nrow(tree$merge));keys<-character(length(nodes))
 for(i in seq_along(nodes)){
  nodes[[i]]<-sort(unlist(lapply(tree$merge[i,],function(k)if(k<0L)-k else nodes[[k]])))
  keys[i]<-paste(nodes[[i]],collapse=',')
 }
 values<-setNames(tree$height,keys);values[order(names(values))]
}
.port_efa <- function(n,baseline,id) {
 covariance<-n$loadings%*%n$factor_correlation%*%t(n$loadings)+diag(n$uniquenesses)
 .port_delta(covariance,n$reproduced_correlation,paste(id,'covariance identity'))
 .port_delta(n$observed_correlation-covariance,n$residual_correlation,paste(id,'residual identity'))
 stopifnot(min(eigen(covariance,symmetric=TRUE,only.values=TRUE)$values)>0,all(n$uniquenesses>0),all(n$uniquenesses<1))
 objective<-as.numeric(determinant(covariance,logarithm=TRUE)$modulus-determinant(n$observed_correlation,logarithm=TRUE)$modulus)+sum(diag(solve(covariance,n$observed_correlation)))-ncol(covariance)
 .port_delta(objective,n$objective,paste(id,'likelihood identity'))
 .port_delta(objective,baseline$objective,paste(id,'frozen optimum'),1e-12)
 .port_equal(rownames(n$loadings),rownames(baseline$loadings),paste(id,'variable names'))
 alignment<-.port_align(n$loadings,baseline$loadings);p<-alignment$order;s<-alignment$sign
 n$loadings<-sweep(n$loadings[,p,drop=FALSE],2L,s,'*');dimnames(n$loadings)<-dimnames(baseline$loadings)
 n$factor_correlation<-n$factor_correlation[p,p,drop=FALSE]*outer(s,s);dimnames(n$factor_correlation)<-dimnames(baseline$factor_correlation)
 n
}
.port_multinomial <- function(n,args,id) {
 # Independent likelihood, score and information checks; no nnet refit.
 d<-args$data;X<-cbind(1,as.matrix(d[args$predictors]));tab<-n$coefficient_table;cats<-n$category_order
 B<-matrix(tab$estimate,nrow=ncol(X));eta<-cbind(0,X%*%B)
 E<-exp(eta-apply(eta,1L,max));P<-E/rowSums(E)
 .port_delta(P,n$fitted,paste(id,'softmax identity'))
 Y<-vapply(cats,function(k)as.numeric(d$y==k),numeric(nrow(d)))
 score<-crossprod(X,Y[,-1L,drop=FALSE]-P[,-1L,drop=FALSE])
 if(max(abs(score))>1e-5)stop(id,': likelihood score residual exceeds optimizer bound')
 H<-matrix(0,ncol(X)*(length(cats)-1L),ncol(X)*(length(cats)-1L))
 for(j in seq_len(length(cats)-1L))for(k in seq_len(length(cats)-1L)){
  jj<-(j-1L)*ncol(X)+seq_len(ncol(X));kk<-(k-1L)*ncol(X)+seq_len(ncol(X))
  H[jj,kk]<-crossprod(X,X*(P[,j+1L]*((j==k)-P[,k+1L])))
 }
 se<-sqrt(diag(solve(H)));critical<-stats::qnorm((1+n$confidence_level)/2)
 .port_delta(se,tab$std_error,paste(id,'information SE'))
 .port_delta(2*stats::pnorm(-abs(tab$estimate/se)),tab$p_value,paste(id,'Wald p'))
 .port_delta(tab$estimate-critical*se,tab$lower,paste(id,'lower limit'))
 .port_delta(tab$estimate+critical*se,tab$upper,paste(id,'upper limit'))
 .port_delta(exp(tab$estimate),tab$relative_odds,paste(id,'ratio identity'))
 ll<-sum(Y*log(P));ll0<-sum(Y*rep(log(colMeans(Y)),each=nrow(Y)))
 .port_delta(2*(ll-ll0),n$omnibus$statistic,paste(id,'likelihood-ratio identity'),1e-10)
}
.port_glmm_integrals<-function(n,args){
 d<-args$data;d<-d[!is.na(d$y),];d$group_numeric<-as.integer(d$group)-1L
 groups<-split(d,d$cluster);beta<-unname(n$coefficients);tau<-sqrt(n$random_intercept_variance)
 ll<-0;score<-rep(0,3L);info<-matrix(0,3L,3L)
 for(g in groups){
  X<-c(1,g$group_numeric[1L]);events<-sum(g$y);size<-nrow(g)
  fun<-function(u,j=0,k=0){
   eta<-sum(X*beta)+u;p<-stats::plogis(eta)
   weight<-exp(events*stats::plogis(eta,log.p=TRUE)+(size-events)*stats::plogis(eta,lower.tail=FALSE,log.p=TRUE)+stats::dnorm(u,sd=tau,log=TRUE))
   if(j==0)return(weight)
   scores<-cbind((events-size*p)*X[1L],(events-size*p)*X[2L],u^2/tau^2-1)
   if(k==0)return(weight*scores[,j])
   h<-if(j<=2L&&k<=2L)-size*p*(1-p)*X[j]*X[k] else if(j==3L&&k==3L)-2*u^2/tau^2 else 0
   weight*(scores[,j]*scores[,k]+h)
  }
  integral<-function(j=0,k=0)stats::integrate(fun,-Inf,Inf,j=j,k=k,rel.tol=1e-11,abs.tol=1e-13,subdivisions=300L)$value
  prob<-integral();s<-vapply(1:3,function(j)integral(j)/prob,numeric(1))
  H<-outer(1:3,1:3,Vectorize(function(j,k)integral(j,k)/prob))
  ll<-ll+log(prob);score<-score+s;info<-info+tcrossprod(s)-H
 }
 list(loglik=ll,score=score,information=info,covariance=solve(info)[1:2,1:2])
}

.port_glmm_math <- function(n,baseline,args,id) {
 # Gaussian integration and analytic derivatives use no mixed-model backend.
 evidence<-.port_glmm_integrals(n,args);frozen<-.port_glmm_integrals(baseline,args)
 .port_delta(evidence$loglik,frozen$loglik,paste(id,'marginal log likelihood'),1e-10)
 if(max(abs(evidence$score))>1e-5)stop(id,': marginal likelihood score exceeds optimizer bound')
 stopifnot(min(eigen(evidence$information,symmetric=TRUE,only.values=TRUE)$values)>0)
 .port_delta(n$covariance,evidence$covariance,paste(id,'integrated likelihood information'),1e-5)
 .port_delta(n$coefficients,baseline$coefficients,paste(id,'frozen fixed effects'),1e-6)
 .port_delta(n$random_intercept_variance,baseline$random_intercept_variance,paste(id,'frozen random variance'),1e-6)
 .port_delta(n$covariance,baseline$covariance,paste(id,'frozen finite-difference covariance'),2e-5)
 beta<-unname(n$coefficients[2]);se<-sqrt(n$covariance[2,2]);z<-beta/se
 .port_delta(n$log_odds_ratio,beta,paste(id,'log-odds identity'))
 .port_delta(n$std_error,unname(se),paste(id,'SE identity'))
 .port_delta(n$statistic,unname(z),paste(id,'Wald statistic identity'))
 .port_delta(n$p_value,unname(2*stats::pnorm(-abs(z))),paste(id,'Wald p identity'))
 .port_delta(n$odds_ratio,exp(beta),paste(id,'odds-ratio identity'))
 .port_delta(n$confidence_interval,exp(beta+c(-1,1)*stats::qnorm((1+n$confidence_level)/2)*se),paste(id,'Wald interval identity'))
}
.port_glmm <- function(n,baseline,args,id) {
 # Refit the documented reference on this runtime; never accept fixture drift
 # merely because another platform or lme4 version was used.
 d<-args$data;d<-d[!is.na(d$y),,drop=FALSE];d$cluster<-factor(d$cluster)
 d$group_numeric<-as.integer(d$group)-1L;d<-d[order(d$cluster,d$id),,drop=FALSE]
 fit<-lme4::glmer(y~group_numeric+(1|cluster),data=d,family=stats::binomial(),nAGQ=15L,
  control=lme4::glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=1e5)),na.action=stats::na.fail)
 .port_equal(unname(n$coefficients),unname(lme4::fixef(fit)),paste(id,'runtime-native fixed effects'))
 .port_equal(n$covariance,as.matrix(stats::vcov(fit)),paste(id,'runtime-native covariance'))
 .port_equal(n$random_intercept_variance,as.numeric(lme4::VarCorr(fit)$cluster[1,1]),paste(id,'runtime-native random variance'))
 .port_glmm_math(n,baseline,args,id)
 # Only these fields are permitted to vary after strict runtime parity,
 # independent likelihood/information and frozen primitive checks above.
 fields<-c('log_odds_ratio','std_error','statistic','p_value','odds_ratio','confidence_interval',
  'random_intercept_variance','coefficients','covariance')
 for(field in fields){.port_equal(attributes(n[[field]]),attributes(baseline[[field]]),paste(id,field,'attributes'));baseline[[field]]<-n[[field]]}
 baseline
}

.port_rank <- function(n,args,id) {
 x<-if(id=='signed_rank_wilcoxon')args$data$x-args$data$y else args$x
 mu<-if(id=='signed_rank_wilcoxon')0 else args$mu
 warnings<-character()
 reference<-withCallingHandlers(stats::wilcox.test(x[x-mu!=0],mu=mu,
  alternative=n$alternative,exact=n$exact,correct=TRUE,conf.int=TRUE,
  conf.level=n$requested_confidence_level,digits.rank=Inf),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
 .port_equal(n$confidence_interval,as.numeric(reference$conf.int),paste(id,'runtime-native limits'))
 .port_equal(n$attained_confidence_level,attr(reference$conf.int,'conf.level'),paste(id,'runtime-native reported level'))
 .port_equal(n$pseudomedian,unname(reference$estimate),paste(id,'runtime-native pseudomedian'))
 .port_equal(n$p_value,reference$p.value,paste(id,'runtime-native p-value'))
 .port_equal(n$diagnostics,warnings,paste(id,'runtime-native diagnostics'))
}
.port_native <- function(n,baseline,id,args) {
 if(id=='cluster_hierarchical_ward'){
  .port_equal(.port_clades(n$fit$tree),.port_clades(baseline$fit$tree),paste(id,'clades and heights'))
  .port_delta(as.matrix(stats::cophenetic(n$fit$tree)),as.matrix(stats::cophenetic(baseline$fit$tree)),paste(id,'cophenetic distances'))
  .port_equal(outer(n$fit$groups,n$fit$groups,'=='),outer(baseline$fit$groups,baseline$fit$groups,'=='),paste(id,'requested partition'))
  # Partial cuts inside equal-height blocks can differ by backend tie ordering.
  for(k in seq_len(nrow(n$fit$scaled))){
   a<-stats::cutree(n$fit$tree,k=k);b<-stats::cutree(baseline$fit$tree,k=k)
   if(!identical(outer(a,a,'=='),outer(b,b,'=='))){
    i<-nrow(n$fit$scaled)-k;stopifnot(k!=n$k,i>=1L,i<length(n$fit$tree$height))
    .port_delta(n$fit$tree$height[i],n$fit$tree$height[i+1L],paste(id,'tied merge'),1e-12)
    .port_delta(baseline$fit$tree$height[i],baseline$fit$tree$height[i+1L],paste(id,'frozen tied merge'),1e-12)
   }
  }
  n$fit$tree$merge<-baseline$fit$tree$merge
 }
 if(id=='efa_common_factor_ml')n<-.port_efa(n,baseline,id)
 if(id=='regression_multinomial').port_multinomial(n,args,id)
 if(id=='glmm_logistic_random_intercept')baseline<-.port_glmm(n,baseline,args,id)
 if(id%in%c('signed_rank_wilcoxon','signed_rank_wilcoxon_one_sample')){
  .port_rank(n,args,id)
  # R changed the recorded exact confidence-level convention; compare it to the
  # matched runtime oracle above, not to an attribute serialized by another R.
  baseline$attained_confidence_level<-n$attained_confidence_level
 }
 compare<-function(a,b,path){
  if(is.list(a)&&is.list(b)&&identical(names(a),names(b))&&length(a)==length(b)){
   .port_equal(attributes(a),attributes(b),paste(path,'attributes'))
   for(i in seq_along(a))compare(a[[i]],b[[i]],paste0(path,'$',if(is.null(names(a)))i else names(a)[i]))
  }else{
   optimizer_field<-(id=='efa_common_factor_ml'&&grepl('\\$(loadings|factor_correlation|uniquenesses|reproduced_correlation|residual_correlation)',path)) ||
    (id=='regression_multinomial'&&grepl('\\$(coefficient_table|fitted)',path))
   if(optimizer_field&&is.numeric(a)&&is.numeric(b)){
    .port_equal(attributes(a),attributes(b),paste(path,'attributes'))
    .port_delta(a,b,path,1e-8)
   }else .port_equal(a,b,path)
  }
 }
 # EFA objective and LR statistic have tiny relative differences near zero;
 # compare their absolute numerical error while retaining the strict 1e-12 bound.
 if(id=='efa_common_factor_ml'){
  .port_delta(n$objective,baseline$objective,paste(id,'objective'))
  .port_delta(n$goodness_of_fit$statistic,baseline$goodness_of_fit$statistic,paste(id,'GoF statistic'))
  baseline$objective<-n$objective;baseline$goodness_of_fit$statistic<-n$goodness_of_fit$statistic
 }
 compare(n,baseline,id)
 invisible(TRUE)
}
