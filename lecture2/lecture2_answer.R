lean <- c(6.13,7.05,7.48,7.48,7.53,7.58,7.9,8.08,8.09,8.11,8.40, 10.15,10.88)
obese <- c(8.79,9.19,9.21,9.68,9.69,9.97,11.51,11.85,12.79) 

ci_1mean = function(dat, ci_lev=0.95)
{
n = length(dat)
tval = qt(1-((1-ci_lev)/2),n)
mean1=mean(dat)
sd1=sd(dat)
return(list(nmsd=c(n, mean1, sd1),
civals=c(mean1-tval*sd1/sqrt(n),
mean1+tval*sd1/sqrt(n)),ci_lev=ci_lev))
}
ci_1mean(dat=x)

x = c(10,9,8,7,6,5)
test.1mean = function(dat,m0)
{
n = length(dat)
df = n-1
t = (mean(dat)-m0)/(sd(dat)/sqrt(n))
pvalue = (1-pt(abs(t),df))*2
return (list(dat5=head(dat,n=5), df=df, t=t,
pvalue=pvalue))
}
test.1mean(dat=x,m0=0)
t.test(x, mu=0)

test.2mean = function(d1,d2){
n1 = length(d1)
n2 = length(d2)
df = n1+n2-2
dif = mean(d1)-mean(d2)
sd_pool = sqrt((var(d1)*(n1-1)+var(d2)*(n2-1))/df)
t = (mean(d1)-mean(d2))/(sd_pool*sqrt(1/n1+1/n2))
pvalue = (1-pt(abs(t),df))*2
return (list(df=df, dif=dif, sd_pooled=sd_pool, t=t,
pvalue=pvalue))}
test.2mean(d1=x1,d2=x2)

ci_2mean = function(d1,d2,ci_lev){
n1 = length(d1)
n2 = length(d2)
df = n1+n2-2
sd_pool = sqrt((var(d1)*(n1-1)+var(d2)*(n2-1))/df)
se = sd_pool*sqrt(1/n1+1/n2)
dif = mean(d1)-mean(d2)
tval = qt(1-((1-ci_lev)/2),df)
return(list(df=df, dif=dif, sd_pooled=sd_pool,
civals=c(dif-tval*se, dif+tval*se),ci_lev=ci_lev))
}
ci_2mean(d1=x1,d2=x2,0.95)