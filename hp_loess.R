# This code attempts to idenitfy a suitable 'span' parameter for loess
# with respect to the given data
require (formula.tools)
hp_loess <- function (frm, dat=data.frame(), span_low = 0.75/9, span_high=0.75*9, nl = 10) {  
  
  lambs = sapply (0:(nl-1), function (i) {span_low * (span_high/span_low)^(i/(nl-1))})
  
  m = dim(dat)[1]
  cv.size = as.integer (m / 5)
  B = 10
  
  errs = matrix (rep (0.0, nl*B), nrow=nl)
  for (i in 1:nl) {
    for (b in 1:B) {
      tmp = sample.int (n=m, size=m, replace=F)
      cv.ind = tmp[1:cv.size]
      tr.ind = tmp[(cv.size+1):m]
      tr.dat = dat [tr.ind, ]
      cv.dat = dat [cv.ind, ]
      
      md = loess (frm, data=tr.dat, span=lambs[i],
                  control=loess.control (surface="direct"))
      
      prd = predict (md, newdata=cv.dat)
      
      if (sum (is.na (prd)) > 0) {
        stop()
      }
      
      errs [i, b] = sum ((prd - cv.dat[, as.character (lhs (frm))])^2)
    }
  }
  
  m.errs = apply (errs, 1, mean)
  s.errs = apply (errs, 1, sd) / sqrt(B)
  
  ind = which.min (m.errs)
  best_lambda = lambs [ind]
  
  
  res = loess (frm, data=dat, span=best_lambda,
               control=loess.control (surface="direct"))
  
  res$lambs = lambs
  res$best_span = best_lambda
  res$cv.errs = m.errs
  res$cv.stderrs = s.errs
  res
}
