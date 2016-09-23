multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      plots <- c(list(...), plotlist)
      numPlots = length(plots)
      if (is.null(layout)) {
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),ncol = cols, nrow = ceiling(numPlots/cols))
      }
      if (numPlots==1) {
            print(plots[[1]])
      } else {
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            for (i in 1:numPlots) {
                  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
            }
      }
}

library(ggplot2)

load("data/rawdata/AAPL.rda")
data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-12-31"),]
AAPL <- ggplot(aes(x=Date,y=Close),data=data) + geom_line() + ggtitle("AAPL") + geom_smooth()
load("data/rawdata/CAT.rda")
data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-12-31"),]
CAT <- ggplot(aes(x=Date,y=Close),data=data) + geom_line() + ggtitle("CAT") + geom_smooth()
load("data/rawdata/IBM.rda")
data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-12-31"),]
IBM <- ggplot(aes(x=Date,y=Close),data=data) + geom_line() + ggtitle("IBM") + geom_smooth()
load("data/rawdata/PG.rda")
data <- rawdata[rawdata[,1] >= as.Date("2015-01-01") & rawdata[,1] <=  as.Date("2015-12-31"),]
PG <- ggplot(aes(x=Date,y=Close),data=data) + geom_line() + ggtitle("PG") + geom_smooth()
pdf("report/Stockprice.pdf",width=8,height=6)
multiplot(AAPL,CAT,IBM,PG,cols=2)
dev.off()
