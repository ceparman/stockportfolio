

tq_CalendarReturns <- function(pf,digits=4)
{

#sort
pf<- pf[order(pf$date),]
  
#number of years

nyears <- length(unique(lubridate::year(pf$date)))

pf$row <- lubridate::year(pf$date)-min(lubridate::year(pf$date))+1

pf$col <- lubridate::month(pf$date)

cal <- matrix(rep(NA,nyears*12),nrow = 5,ncol = 12)

digits <- 4

for (i in 1:nrow(pf)){cal[pf$row[i], pf$col[i]] <- round(pf$`Custom Portfolio`[i],digits) }

rownames<- unique(lubridate::year(pf$date))[order(unique(lubridate::year(pf$date)))]
colnames <-  lubridate::month(label = TRUE,seq(as.Date("1999/1/1"), as.Date("1999/12/12"), "months"))
rownames(cal) <- rownames
colnames(cal) <- colnames

cal
}