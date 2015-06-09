rm (list = ls())
graphics.off()

source ("./hp_loess.R")

library (quantmod)
set.seed (8353)
ticker <- "XEG.TO"
description <- "S&P/TSX Capped Energy Index Fund that gives exposure to Canada's energy sector."
d <- getSymbols (ticker, src = "yahoo", from = "1998-01-01", to = "2015-06-04",
				auto.assign = FALSE)

d <- to.monthly (d)
d <- Ad (d)
the_months <- index (d)
price <- coredata (d)
colnames (price) <- NULL

plot (the_months, price, pch=20, col='blue'); grid()
df <- data.frame (t=c(1:length(the_months)), p=price)

default.span <- loess (p~t, data=df, control=loess.control (surface="direct"))
lines (the_months, default.span$fitted, col='black')

ptm <- proc.time()
good.span <- hp_loess (p~t, dat=df, span_low = 0.75/9, span_high=0.75*9)
ptm <- proc.time() - ptm
lines (the_months, good.span$fitted, col='red')






















