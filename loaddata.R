library(readxl)
library(PerformanceAnalytics)
require(lubridate)


#load returns from xls file
returns <- read_excel("ShinyDash_input.xlsx")

returns$Date<-as.Date(returns$Date, format("%m/%d/%Y"))

returns <- returns[order(returns$Date), ]
returns <- as.xts(returns[, 2:7], order.by = returns$Date)
saveRDS(returns, "returns.rds")

#getSymbols('SPX'),from =  index(returns[1,]), to =index(tail(returns,1)) ) 

to <- index(tail(returns,1))
month(to) <- month(to) +1

from <- index(returns[1,])
month(from) <- month(from) - 1



spx$Date<-as.Date(spx$date, format("%m/%d/%Y"))
spx$date <- NULL
spx <- spx[order(spx$Date), ]
spx <- as.xts(spx[, 4], order.by = spx$Date)



spx_returns<-Return.calculate(to.monthly(spx))$ spx.Close


names(spx_returns) <- "SPX"
spx_returns <- spx_returns[-1,]

saveRDS(spx_returns, "spx_returns.rds")


weights <- c(.1,.1,.1,.2,.2,.3)

# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(returns, weights = weights, verbose = TRUE )

# Create a portfolio that rebalances monthly
pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE )


#chart.CumReturns(pf_bh$returns)