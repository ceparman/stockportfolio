
comp_stats <- function(pf,pf_spx)
{

pts<- as.xts(pf$`Custom Portfolio`,order.by = pf$date)
sts <-  as.xts(pf_spx$SPX,order.by = pf_spx$date)  
  
# returns and spx_returns

spx_annualized_return <- Return.annualized(sts, scale=12)

pf_annualized_return <- Return.annualized(pts, scale=12)

spx_sd <- sd.annualized(sts)

pf_sd <- sd.annualized(pts)

spx_sharpe <- SharpeRatio.annualized(sts)

pf_sharpe <- SharpeRatio.annualized(pts)

spx_sortino <-  SortinoRatio(sts)

pf_sortino <-  SortinoRatio(pts)

spx_mean_up <- mean(sts[sts >0 ] )

pf_mean_up <- mean(pts[pts >0 ] )

spx_mean_down <- mean(sts[sts <0 ] )

pf_mean_down <- mean(pts[pts < 0 ] )


spx_max_up <- max(sts)

pf_max_up <- max(pts)

spx_min_down <- min(sts)

pf_min_down <- min(pts)

pf_profitable_percentage <- length(pts[pts >0 ])/length(pts)

spx_profitable_percentage <- length(sts[sts >0 ])/
                             length(sts)

spx_dd <- DownsideDeviation(sts)

pf_dd <-  DownsideDeviation(pts)

correlation <- cor(cumprod(1 + pts),cumprod(1 + sts))

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
  
  
  
  
  
  
  




