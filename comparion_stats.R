
comp_stats <- function(pf,pf_spx)
{

  
  
# returns and spx_returns

spx_annualized_return <- Return.annualized(pf_spx$returns, scale=12)

pf_annualized_return <- Return.annualized(pf$returns, scale=12)

spx_sd <- sd.annualized(pf_spx$returns)

pf_sd <- sd.annualized(pf$returns)

spx_sharpe <- SharpeRatio.annualized(pf_spx$returns)

pf_sharpe <- SharpeRatio.annualized(pf$returns)

spx_sortino <-  SortinoRatio(pf_spx$returns)

pf_sortino <-  SortinoRatio(pf$returns)

spx_mean_up <- mean(pf_spx$returns[pf_spx$returns >0 ] )

pf_mean_up <- mean(pf$returns[pf$returns >0 ] )

spx_mean_down <- mean(pf_spx$returns[pf_spx$returns <0 ] )

pf_mean_down <- mean(pf$returns[pf$returns < 0 ] )


spx_max_up <- max(pf_spx$returns)

pf_max_up <- max(pf$returns)

spx_min_down <- min(pf_spx$returns)

pf_min_down <- min(pf$returns)

pf_profitable_percentage <- length(pf$returns[pf$returns >0 ])/length(pf$returns)

spx_profitable_percentage <- length(pf_spx$returns[pf_spx$returns >0 ])/
                             length(pf_spx$returns)

spx_dd <- DownsideDeviation(pf_spx$returns)

pf_dd <-  DownsideDeviation(pf$returns)

correlation <- cor(cumprod(1 + pf$returns),cumprod(1 + pf_spx$returns))

r2 <- correlation * correlation


### make table


performance_comparision <-data.frame(
   "Custom Portfolio" = c(pf_annualized_return, pf_sd,pf_sharpe,pf_sortino,
                          pf_mean_up,pf_mean_down,pf_min_down,pf_max_up,
                          pf_profitable_percentage,pf_dd,NA,NA),


"S&P 500" = c( spx_annualized_return, spx_sd,spx_sharpe,spx_sortino,
                       spx_mean_up,spx_mean_down,spx_min_down,spx_max_up,
                       spx_profitable_percentage,spx_dd,correlation,r2),

row.names = c("Annual Return","Standard Deviation"," Sharpe Ratio","Sortino Ratio",
              "Avg. Return Up Months","Avg. Return Down Months", "Lowest Monthly Return",
              "Hightest Monthly Return","Profitable Percentage","Downside Deviation",
              "Correlation","R2"),

stringsAsFactors = FALSE)
 
return(performance_comparision)                         
  
}
  
  
  
  
  
  
  




