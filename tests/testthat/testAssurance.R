test_that("assurance with normal prior",{
  skip_on_cran()
  m <- 1
  s <- 1
  a <- 2
  b <- 2
  N <- 10000
  n <- 30
  # Estimate directly
  delta <- rnorm(N, m, s)
  sigmaT <- 1/ sqrt(rgamma(N, a, b))
  sigmaC <- 1/ sqrt(rgamma(N, a, b))
  xT <- matrix(rnorm(N*n, delta, sigmaT),
               nrow = n,
               ncol = N,
               byrow = TRUE)
  xC <- matrix(rnorm(N*n, 0, sigmaC),
               nrow = n,
               ncol = N,
               byrow = TRUE)
  pvals <- rep(0, N)
  tstats <- rep(0, N)
  for(i in 1:N){
    ttestresults <- t.test(xT[, i], xC[, i])
    pvals[i] <- ttestresults$p.value
    tstats[i] <- ttestresults$statistic
  }
  myAssurance <- mean(pvals< 0.05 & tstats > 0)

  myfitDelta <- SHELF::fitdist(vals = qnorm(c(0.25, 0.5, 0.75),
                                            mean = m,
                                            sd = s),
                               probs = c(0.25, 0.5, 0.75))
  sLower <- 1/sqrt(qgamma(0.95, a, b))
  sUpper <- 1/sqrt(qgamma(0.05, a, b))
  proportionLower <- 0.5 - pnorm(0, mean = m, sd = sUpper)
  proportionUpper <- 0.5 - pnorm(0, mean = m, sd = sLower)

  myfitTau <- SHELF::fitprecision(interval = c(0, m),
                                  propvals = c(proportionLower,
                                               proportionUpper),
                                  med = m,
                                  pplot = FALSE)
  packageAssurance <- assuranceNormal(fitDelta = myfitDelta,
                                      fitPrecisionTmt = myfitTau,
                                      fitPrecisionCtrl = myfitTau,
                                      nControl = n,
                                      nTreatment = n)

  expect_equal(myAssurance, packageAssurance,
               tolerance = 3 * sqrt(2 * 0.25 / N),
               scale = 1)
})
