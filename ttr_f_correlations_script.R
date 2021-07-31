coll=read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\script files\\ttr_f_length.csv",sep=";",stringsAsFactors = TRUE)
colnames(coll)<-c("type","f","ttrword","ttrlemmapos","ttrlemma","amount_words","amount_docs","avg_doc_len")

plot(coll$f,coll$avg_doc_len)

plot(coll$f,coll$ttrword)

cor(coll[-1], method = c("pearson"))

install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(coll[-1]))
res2

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")


chart.Correlation(coll[-1], histogram=FALSE, pch=19)
