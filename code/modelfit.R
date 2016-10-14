library(mgcv)
library(ggplot2)
library(gplots)
load("data/quandldata.rda")

getdata <- function(stock) {
      data <- quandldata[[stock]]
      data <- data[data[,1] >= as.Date("2015-01-01") & data[,1] <=  as.Date("2015-1-31"),]
      list(x=data$Date, y=data$Close)
}

changepointfunc <- function(x,y,simutime=100) {
      n <- length(x)
      predx <- seq(as.numeric(min(x)),as.numeric(max(x)),length.out=1000)
      model <- mgcv::gam(y~s(as.numeric(x),k=3))
      predy <- unlist(predict(model,data.frame(x=predx)))
      rlevec <- rle(as.numeric(diff(predy) > 0))
      if (length(rlevec$lengths) > 1) {
            changepoint <- cumsum(rlevec$lengths)
            changepoint <- changepoint[-length(changepoint)]
            set.seed(12345)
            permutechangepoint <- lapply(1:simutime,function(i) {
                  permutey <- sample(resid(model)) + fitted(model)
                  permutepredy <- unlist(predict(mgcv::gam(permutey~s(as.numeric(x),k=3)),data.frame(x=predx)))
                  tmprlevec <- rle(as.numeric(diff(permutepredy) > 0))
                  if (identical(tmprlevec$values,rlevec$values)) {
                        tmpchangepoint <- cumsum(tmprlevec$lengths)
                        tmpchangepoint[-length(tmpchangepoint)]      
                  } else {
                        NULL
                  }
            })
            permutechangepoint <- permutechangepoint[sapply(permutechangepoint,length)==length(changepoint)]
            if (length(permutechangepoint)/simutime < 0.75) {
                  list(pattern="indefinite",data=data.frame(x=x,y=y),fitted=data.frame(x=predx,y=predy))
            } else {
                  pattern <- rlevec$values
                  pattern[pattern==1] <- "increasing"
                  pattern[pattern==0] <- "decreasing"
                  changepointframe <- t(sapply(1:length(changepoint),function(i) {
                        tmppoint <- sapply(permutechangepoint,function(j) j[i])
                        c(changepoint[i],quantile(tmppoint,0.025),quantile(tmppoint,0.975))
                  },USE.NAMES = F))
                  dimnames(changepointframe) <- NULL
                  changepointframe <- data.frame(changepoint=predx[changepointframe[,1]],lowerbound=predx[changepointframe[,2]],upperbound=predx[changepointframe[,3]])
                  list(pattern=paste0(pattern,collapse = "_"),changepoint=changepointframe,data=data.frame(x=x,y=y),fitted=data.frame(x=predx,y=predy))
            }
      } else {
            if (rlevec$values==1) {
                  list(pattern="monotone increasing",data=data.frame(x=x,y=y),fitted=data.frame(x=predx,y=predy))
            } else {
                  list(pattern="monotone decreasing",data=data.frame(x=x,y=y),fitted=data.frame(x=predx,y=predy))
            }
      }
}

plotfunc <- function(fit, stock) {
      fit$fitted$x <- as.Date(fit$fitted$x)
      p <- ggplot(aes(x=x,y=y),data=fit$data) + geom_point(size=2) + geom_line(aes(x=x,y=y),size=1.5,data=fit$fitted) + ggtitle(bquote(atop(.(paste("Stock:",stock)), .(paste0("Pattern: ",fit$pattern)))))
      if ("changepoint" %in% names(fit)) {
            cp <- fit$changepoint
            segdata <- NULL
            middata <- NULL
            for (i in 1:nrow(cp)) {
                  tmpy <- fit$fitted$y[which.min(abs(cp[i,1]-as.numeric(fit$fitted$x)))]
                  segdata <- rbind(segdata,c(cp[i,1],tmpy))
                  middata <- rbind(middata,c(cp[i,2],cp[i,3],tmpy,tmpy))
            }
            segdata <- data.frame(x=as.Date(segdata[,1]),y=segdata[,2])
            middata <- data.frame(x1=as.Date(middata[,1]),x2=as.Date(middata[,2]),y1=middata[,3],y2=middata[,4])
            p <- p + geom_point(data=segdata,size=4,color="red") + geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2),data=middata,size=1.5,color="green")
      }
      p + theme(axis.line.x = element_line(colour = "black"),
                  axis.line.y = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.text.x = element_text(size=17,color='black'),
                  axis.text.y = element_text(size=17,color='black'),
                  axis.title.x = element_text(size=20,vjust=-1),
                  axis.title.y = element_text(size=20,vjust=1),
                  strip.text.y = element_text(size=17,color='black'),
                  plot.margin = unit(c(1,1,1,1), "cm")
            ) + xlab("Date") + ylab("Price")
}

compareplot <- function(fitlist,stock) {
      dataorder <- order(sapply(fitlist,function(i) i$changepoint[,1]))
      fitlist <- fitlist[dataorder]
      pooldata <- sapply(fitlist,function(i) i$fitted$y)
      pooldata <- apply(pooldata,2,scale)
      par(cex=1.5)
      image(as.Date(fitlist[[1]]$fitted$x),1:ncol(pooldata),pooldata,yaxt="n",col = bluered(100),xlab="Date",ylab="Stock")
      axis(2, at=1:ncol(pooldata), labels=stock)
      xpos <- sapply(fitlist,function(i) i$changepoint[,1])
      points(y=1:ncol(pooldata),x=xpos,pch=19)
      for (i in 1:ncol(pooldata)) {
            lines(c(fitlist[[i]]$changepoint[,2],fitlist[[i]]$changepoint[,3]),c(i,i))
      }
}

AXPdata <- getdata("AXP")
AXPfit <- changepointfunc(AXPdata$x,AXPdata$y)
plotfunc(AXPfit,"AXP")
PGdata <- getdata("PG")
PGfit <- changepointfunc(PGdata$x,PGdata$y)
plotfunc(PGfit,"PG")
PFEdata <- getdata("PFE")
PFEfit <- changepointfunc(PFEdata$x,PFEdata$y)
plotfunc(PFEfit,"PFE")
GEdata <- getdata("GE")
GEfit <- changepointfunc(GEdata$x,GEdata$y)
plotfunc(GEfit,"GE")

compareplot(list(PGfit,PFEfit,GEfit),c("PG","PFE","GE"))

# for (stock in names(quandldata)) {
#       data <- quandldata[[stock]]
#       data <- data[data[,1] >= as.Date("2015-01-01") & data[,1] <=  as.Date("2015-1-31"),]
#       x <- data$Date
#       y <- data$Close
#       fitres <- changepointfunc(x,y)
#       pdf(paste0("plot/",stock,".pdf"))
#       print(plotfunc(fitres,stock))
#       dev.off()
# }


