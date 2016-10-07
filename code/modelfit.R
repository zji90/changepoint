library(mgcv)

for (f in list.files("data/rawdata/")) {
      load(paste0("data/rawdata/",f))
      data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-1-31"),]
      x <- data$Date
      y <- data$Close
      n <- length(x)
      pdf(paste0("plot/",sub(".rda",".pdf",f)))
      plot(x,y)
      lines(x,fitted(mgcv::gam(y~s(as.numeric(x),k=3))))
      dev.off()
}

for(f in list.files("data/rawdata/")) {
      load(paste0("data/rawdata/",f))
      data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-3-31"),]
      x <- data$Date
      y <- data$Close
      n <- length(x)
      pdf(paste0("plot/",sub(".rda",".pdf",f)))
      plot(x,y)
      lines(x,fitted(smooth.spline(y~x,nknots = 4)))
      dev.off()
}

for(f in list.files("data/rawdata/")) {
      load(paste0("data/rawdata/",f))
      data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-1-31"),]
      x <- data$Date
      y <- data$Close
      n <- length(x)
      pval <- 0
      i <- 0
      while(pval < 0.05 && i < 20) {
            i <- i + 1
            model1 <- mgcv::gam(y~poly(as.numeric(x),k=i))
            model2 <- mgcv::gam(y~poly(as.numeric(x),k=i+1))
            pval <- pchisq(model1$deviance - model2$deviance, model1$df.residual - model2$df.residual,lower.tail = F)
      }
      pdf(paste0("plot/",sub(".rda",".pdf",f)))
      plot(x,y,main = i)
      lines(x,fitted(mgcv::gam(y~poly(as.numeric(x),k=i))))
      dev.off()
}





load("data/rawdata/AAPL.rda")
data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-12-31"),]

x <- data$Date
y <- data$Close
n <- length(x)

plot(x,y)



kfit <- sapply(1:10,function(i) {
      fitted(mgcv::gam(y~poly(as.numeric(x),k=i)))
})
resid <- sweep(kfit,1,y,"-")
plot(x,y)
lines(x,kfit[,6])

plot(resid[,6])


      
      




plot(x,y)
lines(x,kfit[,1])
sapply(2:ncol(kfit),function(i) {
      sum((kfit[,i]-kfit[,i-1])^2)
})


model <- mgcv::gam(y~poly(as.numeric(x),k=3))
resid <- y-fitted(model)
rf <- sapply(1:1000,function(d) {
      ry <- sample(resid) + fitted(model)
      fitted(mgcv::gam(ry~poly(as.numeric(x),k=3)))
})
for (i in 1:1000) {
      lines(x,rf[,i],col="grey")
}



tmp <- rowMeans(sapply(4:10,function(i) {
      fitted(smooth.spline(y~x,nknots = i))
}))

tmp <- sapply(4:20,function(i) {
      fitted(mgcv::gam(y~poly(as.numeric(x),k=i)))
})

dim(tmp)
plot(colSums(sweep(tmp,1,rowMeans(tmp),"-")^2))


plot(x,y)
lines(x,tmp)
lines(x,fitted(mgcv::gam(y~poly(as.numeric(x),k=5))))

lines(x,fitted(mgcv::gam(y~poly(as.numeric(x),k=7))))

model1 <- smooth.spline(y~x,nknots=5)
model2 <- smooth.spline(y~x,nknots=6)
rss1 <- sum((fitted(model1) - y)^2)
rss2 <- sum((fitted(model2) - y)^2)
pf((rss1-rss2)/(rss2/(n-4)),1,n-5,lower.tail = F)


plot(x,y)
lines(data$Date,fitted(smooth.spline(data$Close~data$Date,nknots=6)))




