library(Quandl)
quandldata <- list()
for (stock in c("JNJ","DIS","MSFT","BA","NKE","PG","MCD","CVX","PFE","V","TRV","VZ","MRK","WMT","GE","IBM","UTX","DD","HD","AAPL","CAT","CSCO","KO","XOM","UNH","AXP","GS","MMM","JPM","INTC")) {
      quandldata[[stock]] <- Quandl(paste0("EOD/",stock), api_key="FzzgSFxTFLtnYj4trM66")      
}
save(quandldata,file=paste0("data/quandldata.rda"))

