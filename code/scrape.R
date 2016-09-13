library(Quandl)
for (stock in c("JNJ","DIS","MSFT","BA","NKE","PG","MCD","CVX","PFE","V","TRV","VZ","MRK","WMT","GE","IBM","UTX","DD","HD","AAPL","CAT","CSCO","KO","XOM","UNH","AXP","GS","MMM","JPM","INTC")) {
      rawdata <- Quandl(paste0("EOD/",stock), api_key="FzzgSFxTFLtnYj4trM66")      
      save(rawdata,file=paste0("data/rawdata/",stock,".rda"))
}


