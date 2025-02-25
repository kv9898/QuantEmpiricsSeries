hausman <- function(consistent, efficient){
  cf_diff <- coef(consistent) - coef(efficient)
  vc_diff <- vcov(consistent) - vcov(efficient)
  stat <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
  df= degrees_freedom(consistent, type='k')-1
  p <-  pchisq(stat, df = df, lower.tail = FALSE)
  htest <- list(statsitic= c('chisq'=stat),
                p.value = p,
                parameter = c('df'=df),
                method = "Hausman Test",
                data.name = capture.output(print(formula(efficient))),
                alternative = 'one model is inconsistent') 
  class(htest) <- 'htest'
  return(htest)
}
