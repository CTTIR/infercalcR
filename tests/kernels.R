library(infercalcR)
# Independent small-support Fisher ordering and direct binomial summation.
independent_p <- function(x1,n1,x2,n2,side) {
  tab <- expand.grid(x1=0:n1,x2=0:n2)
  ordering <- vapply(seq_len(nrow(tab)),function(i) stats::fisher.test(
    matrix(c(tab$x1[i],n1-tab$x1[i],tab$x2[i],n2-tab$x2[i]),2,2),alternative=side)$p.value,0)
  obs <- stats::fisher.test(matrix(c(x1,n1-x1,x2,n2-x2),2,2),alternative=side)$p.value
  keep <- ordering <= obs*(1+1e-7)
  probability <- function(p)sum(stats::dbinom(tab$x1[keep],n1,p)*stats::dbinom(tab$x2[keep],n2,p))
  grid <- seq(0,1,length.out=101)
  max(vapply(grid,probability,0),vapply(seq_len(100),function(i)stats::optimize(probability,grid[c(i,i+1)],maximum=TRUE,tol=1e-12)$objective,0))
}
for(v in c('less','greater','minlike','central')) {
  r <- boschloo_inference(1,4,3,5,v,alpha=.05)
  d <- result_data(r)
  ref <- if(v=='central')min(1,2*min(independent_p(1,4,3,5,'less'),independent_p(1,4,3,5,'greater'))) else independent_p(1,4,3,5,if(v=='minlike')'two.sided' else v)
  stopifnot(abs(d$p.value-ref)<1e-10,d$p.upper>=d$p.value,d$p.upper-d$p.value<2.1e-9,
    is.na(d$conf.low),is.na(d$conf.high),d$estimate==1/4-3/5)
  rr<-boschloo_inference(3,5,1,4,switch(v,less='greater',greater='less',v),alpha=.05)
  stopifnot(abs(result_data(rr)$p.value-d$p.value)<1e-12)
  kernel<-boschloo_design_kernel(4,5,.05,v)
  stopifnot(isTRUE(all.equal(binomial_region_probability(kernel$idx,4,5,.5,.5),
    sum(stats::dbinom(kernel$x1[kernel$idx],4,.5)*stats::dbinom(kernel$x2[kernel$idx],5,.5)),tolerance=1e-15)))
  # Membership is tested against observed decisions at every small table.
  for(i in seq_along(kernel$member)) {
    z<-boschloo_inference(kernel$x1[i],4,kernel$x2[i],5,v,alpha=.05)
    stopifnot(identical(isTRUE(result_data(z)$decision=='reject'),kernel$member[i]))
  }
}
# A chosen alpha inside numerical bounds stays unresolved.
u<-boschloo_inference(0,1,1,1,'less',alpha=.25)
stopifnot(result_data(u)$status=='unresolved',result_data(u)$decision=='unresolved')
for(p in c(1e-100,.001,.5,.999)) {
  q<-t_quantile(p,df=12)
  stopifnot(abs(log(t_probability(q,12))-log(p))<1e-11)
}
stopifnot(isTRUE(all.equal(normal_probability(c(-2,0,2)),stats::pnorm(c(-2,0,2)),tolerance=1e-15)),
  isTRUE(all.equal(f_probability(2,3,20),stats::pf(2,3,20),tolerance=1e-14)),
  isTRUE(all.equal(chisq_probability(3,5),stats::pchisq(3,5),tolerance=1e-14)))
cond<-tryCatch(boschloo_inference(5,4,3,5,'less'),infercalcR_invalid_input=function(e)e)
stopifnot(inherits(cond,'infercalcR_invalid_input'),inherits(cond,'inferguidR_invalid_input'))
cat('PASS kernels: independent small-support ordering; four variants; 120 membership checks; bounds, direction and probability kernels.\n')
