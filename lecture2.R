lean <- c(6.13,7.05,7.48,7.48,7.53,7.58,7.9,8.08,8.09,8.11,8.40, 10.15,10.88)
obese <- c(8.79,9.19,9.21,9.68,9.69,9.97,11.51,11.85,12.79) 

ci_1mean = function(dat, ci_lev)
{
  n = length(dat)
  tval = qt(1-((1-ci_lev)/2), n-1)
  mean1 = mean(dat)
  sd1 = sd(dat)
  return(list(nmsd = c(n, mean1, sd1),
              civals = c(mean1 - tval*sd1/sqrt(n),
              mean1+tval*sd1/sqrt(n)),
              ci_lev=ci_lev))
  
}

ci_1mean(dat=lean, 0.95)


test.lean = function(dat, m0)
{
  n = length(dat)
  df = n-1
  t = abs((mean(dat)-m0)/(sd(dat)/sqrt(n)))
  pvalue = (1-pt(t, df))*2
  return(list(df=df,t=t,pvalue=pvalue))
}

test.lean(dat = lean, m0=10)

test.lean_obese = function(dat1,dat2,ci_lev)
{
  pooled = sqrt(((length(dat1)-1)*var(dat1) + (length(dat2) - 1 )*var(dat2))/(length(dat1) + length(dat2) - 2))
  t_den = pooled*sqrt(1/length(dat1)+1/length(dat2))
  t_num = mean(dat1)-mean(dat2)
  t = t_num/t_den
  df = length(dat1) + length(dat2) -2
  under_score = qt(ci_lev, df)
  over_score = qt(ci_lev,df,lower.tail = FALSE)
  return(list(df=df, t=t, under_score = under_score, over_score = over_score))
}

test.lean_obese(dat1 = lean, dat2 = obese, ci_lev = 0.025)
t.test(lean, obese, var.equal = T)
