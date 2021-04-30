df = c(10,20,30,40,50,100,200) 

ratio = numeric(length(df)) 

for (i in 1:length(df)){ 
  
  ratio[i] = qt(0.975, df[i])/1.96 
  
} 



plot(df,ratio,col="blue",pch=16,cex=1.5, xlab="degrees of freedom", 
     
     ylab="Ratio of width of 95%CI",type="b",xlim=c(0,200),ylim=c(1,1.15)) 

abline(h=1,lty="dotted") 