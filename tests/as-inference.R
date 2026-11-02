library(infercalcR)
err<-function(expr,class='infercalcR_invalid_input'){x<-tryCatch(force(expr),error=identity);stopifnot(inherits(x,class))}
x<-c(1,2,3,5,8);y<-c(2,4,6,7,8,10)
t<-stats::t.test(x,y)
before<-serialize(t,NULL)
a<-as_inference(t,n=c(length(x),length(y)),population='synthetic independent groups',groups=c('x','y'))
d<-result_data(a)
stopifnot(identical(before,serialize(t,NULL)),d$estimate==mean(x)-mean(y),d$n==11,
 isTRUE(all.equal(c(d$conf.low,d$conf.high),as.numeric(t$conf.int))))
err(as_inference(t))
err(as_inference(t,population='synthetic groups'))
unknown<-as_inference(t,population='synthetic groups; sizes not recorded',groups=c('x','y'))
stopifnot(is.na(result_data(unknown)$n))
b<-stats::binom.test(2,10,p=.5,alternative='greater')
bb<-as_inference(b,population='synthetic binary trials',event='event',n=10)
stopifnot(result_data(bb)$estimate==.2,result_data(bb)$conf.high==1,result_data(bb)$null.value==.5)
err(as_inference(b,population='trials'))
chi<-stats::chisq.test(matrix(c(10,20,30,40),2),correct=FALSE)
cc<-as_inference(chi,population='independent synthetic counts',n=100)
stopifnot(is.na(result_data(cc)$estimate),is.na(result_data(cc)$conf.low),result_data(cc)$component=='omnibus')
fake<-t;fake$method<-'Unregistered result';err(as_inference(fake,population='unknown'),'infercalcR_unsupported')
dat<-data.frame(x=1:10,y=c(1,3,2,5,4,8,6,9,8,12))
lmfit<-stats::lm(y~x,dat);save<-serialize(lmfit,NULL)
lmout<-as_inference(lmfit,population='synthetic complete cases',conf.level=.9)
lmd<-result_data(lmout);ci<-stats::confint(lmfit,level=.9)
stopifnot(identical(save,serialize(lmfit,NULL)),isTRUE(all.equal(lmd$estimate,unname(stats::coef(lmfit)))),
 isTRUE(all.equal(lmd$conf.low,unname(ci[,1]))),isTRUE(all.equal(lmd$conf.high,unname(ci[,2]))))
alias<-stats::lm(y~x+I(2*x),dat);aliasout<-result_data(as_inference(alias,population='synthetic aliased design'))
stopifnot(any(aliasout$status=='undefined'),all(is.na(aliasout$p.value[aliasout$status=='undefined'])))
gd<-data.frame(x=rep(c(-1,0,1),10),y=rep(c(0,1,1,1,0,0,0,0,1,1),3))
g<-stats::glm(y~x,gd,family=stats::binomial());save<-serialize(g,NULL)
gout<-as_inference(g,population='synthetic binary outcomes');gg<-result_data(gout)
stopifnot(identical(save,serialize(g,NULL)),sum(gg$component=='ratio')==1,
 gg$estimate[gg$component=='ratio']==exp(stats::coef(g)[2]),
 all(grepl('Wald',gg$interval.method)),all(gg$null.value[gg$component=='ratio']==1))
err(as_inference(g,population='synthetic',interval='profile'),'infercalcR_unsupported')
cat('PASS htest/lm/glm conversions, unknown sizes, estimands and interval policies\n')
fisher_rxc<-stats::fisher.test(matrix(c(3,1,4,1,5,2),nrow=2,byrow=TRUE))
fv<-result_data(as_inference(fisher_rxc,population='synthetic RxC table',n=16))
stopifnot(fv$component=='omnibus',is.na(fv$estimate),is.na(fv$conf.low),fv$p.value==fisher_rxc$p.value)

fp<-stats::fisher.test(matrix(c(4,1,1,4),2))
err(as_inference(fp,population='table',event='success'))
fr<-result_data(as_inference(fp,population='table',event='success',groups=c('a','b')))
stopifnot(fr$contrast=='a / b',fr$estimate==unname(fp$estimate))
prop_many<-stats::prop.test(c(10,20,30),c(30,40,50))
pr<-result_data(as_inference(prop_many,population='three groups',event='success'))
stopifnot(pr$component=='omnibus',is.na(pr$estimate),is.na(pr$conf.low),pr$p.value==prop_many$p.value)
err(as_inference(b,population='trials',event='success',n=NaN))
err(as_inference(b,population='trials',event='success',n_excluded='unknown'))

# Signed-rank htest labels do not identify whether observations or paired differences were supplied.
wx<-c(2,5,9,14,20,27,35,44);wy<-c(1,3,6,10,15,21,28,36)
for(exact in c(TRUE,FALSE))for(alternative in c('two.sided','greater')){
 one_sample<-stats::wilcox.test(wx-wy,conf.int=TRUE,conf.level=.9,exact=exact,alternative=alternative)
 paired<-stats::wilcox.test(wx,wy,paired=TRUE,conf.int=TRUE,conf.level=.9,exact=exact,alternative=alternative)
 stopifnot(identical(one_sample$method,paired$method))
 for(origin in c('one_sample','paired')){
  native<-if(origin=='paired')paired else one_sample;original<-serialize(native,NULL)
  generic<-as_inference(native,population='synthetic supplied analysis',n=length(wx))
  stopifnot(result_metadata(generic)$method_id=='imported_signed_rank',
   result_metadata(generic)$variant_id=='imported_signed_rank',result_metadata(generic)$design=='unknown')
  id<-if(origin=='paired')'signed_rank_wilcoxon' else 'signed_rank_wilcoxon_one_sample'
  declared<-as_inference(native,method_id=id,population='synthetic declared design',n=length(wx),
   groups=if(origin=='paired')c('x','y') else NULL)
  stopifnot(result_metadata(declared)$method_id==id,result_metadata(declared)$variant_id==id,
   result_metadata(declared)$design==origin,result_metadata(declared)$design_source=='caller_declared')
  if(origin=='paired')stopifnot(grepl('paired differences',result_data(declared)$estimand),result_data(declared)$contrast=='x minus y')
  for(value in list(generic,declared)){
   row<-result_data(value)
   stopifnot(row$estimate==unname(native$estimate),row$statistic==unname(native$statistic),
    row$p.value==native$p.value,row$null.value==unname(native$null.value),row$n==length(wx),
    identical(c(row$conf.low,row$conf.high),as.numeric(native$conf.int)),
    row$conf.level==attr(native$conf.int,'conf.level'),row$alternative==native$alternative,
    grepl("native-reported confidence level",row$interval.method),
    identical(result_metadata(value)$warnings,result_metadata(value)$interval_level_convention),
    is.na(result_metadata(value)$interval_level_source$r_version))
  }
  stopifnot(identical(original,serialize(native,NULL)))
  err(as_inference(native,method_id='rank_sum_mann_whitney',population='incompatible design'))
 }
}
cat('PASS signed-rank design ambiguity, explicit paired/one-sample identity and preserved exact/approximate intervals\n')
