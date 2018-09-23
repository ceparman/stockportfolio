library(readxl)
library(PerformanceAnalytics)

returns <- read_excel("ShinyDash_input.xlsx")

returns$Date<-as.Date(returns$Date, format("%m/%d/%Y"))

returns <- returns[order(returns$Date), ]
returns <- as.xts(returns[, 2:7], order.by = returns$Date)
saveRDS(returns, "returns.rds")





weights <- c(.1,.1,.1,.2,.2,.3)

# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(returns, weights = weights, verbose = TRUE )

# Create a portfolio that rebalances monthly
pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE )


#chart.CumReturns(pf_bh$returns)