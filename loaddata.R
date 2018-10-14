library(readxl)
library(PerformanceAnalytics)
require(lubridate)
require(quantmod)
require(tidyquant)

#load top 10 tech stocks

symbols <- c("AAPL","AMZN","GOOG","MSFT","FB","INTC","CSCO","ORCL","NFLX")

from <- "2013-01-01"
to <-   "2018-01-01"


prices <-  tq_get(symbols,from = from, to =to  )

returns <- prices %>%   
           group_by(symbol) %>%
           tq_transmute(select= adjusted,
                        mutate_fun = periodReturn,
                        period     = "monthly",
                        type       = "arithmetic")

saveRDS(returns,"returns.rds")

spx <-  tq_get("^GSPC",from = from, to =to  )

spx_returns <- spx %>%   
  
  tq_transmute(select= adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "arithmetic")


names(spx_returns)[2] <- "SPX"
saveRDS(spx_returns, "spx_returns.rds")
