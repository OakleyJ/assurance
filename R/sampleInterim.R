#' Simulate posterior probabilities that the treatment is effective, given a new study
#'
#' Simulates a number of trials, and for each simulated trial dataset, estimates
#' the posterior probability that the treatment is effective, given the simulated
#' data. This can be used to assess how informative a new trial would be, by
#' investigating how close the simulated posterior probabilities are to 0 or 1.
#'
#' @param fitDelta an object of class \code{elicitation}
#' (obtained with \code{SHELF::fitdist()}), which
#' describes the elicited prior distribution for the treatment effect
#' \eqn{\delta}, conditional on \eqn{\delta \neq 0}.
#' @param fitPrecisionTmt an object of class \code{elicitation}, which
#' describes the elicited prior distribution for the treatment group precision
#' \eqn{\sigma_t^{-2}}.
#' @param precisionCtrl character string: one of \code{"iid"} for same distribution
#' as treatment group precision; \code{"separate"} if a different distribution is to be
#' used; \code{"fixed"} if the control group variance is to be fixed at a constant.
#' @param fitPrecisionCtrl an object of class \code{elicitation}, which
#' describes the elicited prior distribution for the control group precision
#' \eqn{\sigma_c^{-2}}. Only used if \code{precisionCtrl} is set to \code{separate}.
#' @param sigmaSqCtrl a fixed value to be used for the control group variance.
#' Only used if \code{precisionCtrl} is set to \code{"fixed"}.
#' @param pDeltaZero the prior probability that the treatment effect
#' \eqn{\delta = 0}.
#' @param nControl the sample size for the control arm.
#' @param nTreatment the sample size for the treatment arm.
#' @param deltaDist the chosen distribution for the treatment effect \eqn{\delta},
#' conditional on \eqn{\delta \neq 0}. One of \code{"normal"},
#' \code{"t"}, \code{"gamma"}, \code{"lognormal"}, \code{"logt"},
#' \code{"beta"}.
#' @param precisionTmtDist the chosen distribution for the treatment group precision.
#' Can be either \code{"gamma"} or \code{"lognormal"}
#' @param precisionCtrlDist the chosen distribution for the control group precision.
#' Can be either \code{"gamma"} or \code{"lognormal"}.
#' Ignored if \code{precisionCtrl} is set to \code{"fixed"}.
#' @param deltaThreshold the minimum required value for the treatment effect: for each
#' trial the posterior probability \eqn{P(\delta >}\code{deltaThreshold}|data) will
#' be estimated.
#' @param diagnostics logical. Set to \code{TRUE} to produce convergence diagnostics
#' for one example chain.
#' @param nTrials number of trials to be simulated.
#' @param nIter number of MCMC iterations per simulated trial
#' @param nAdapt number of iterations per simulated trial used prior to choose the
#' proposal distributions. See \code{?rjags::adapt} for details
#' @param nBurn number of iterations discarded from each Markov chain as burn-in.
#' @param no_cores number of cores used in \code{parallel::parLapply}
#' @return a vector of simulated posterior probabilities
#' \eqn{P(\delta >}\code{deltaThreshold}|data)
#'
#' @export


sampleInterim <- function(fitDelta,
                          fitPrecisionTmt,
                          precisionCtrl = "iid",
                          fitPrecisionCtrl = NULL,
                          sigmaSqCtrl = NULL,
                          pDeltaZero = 0,
                          nControl = 25,
                          nTreatment = nControl,
                          deltaDist = "normal",
                          precisionTmtDist = "gamma",
                          precisionCtrlDist = precisionTmtDist,
                          deltaThreshold = 0,
                          diagnostics = FALSE,
                          nTrials = 100,
                          nIter = 1000,
                          nAdapt = 1000,
                          nBurn = 0,
                          no_cores = parallel::detectCores() - 1){

  deltaSample <- SHELF::sampleFit(fitDelta, n = nTrials)[, deltaDist]
  sigmaTmtSample <- 1 /
    sqrt(SHELF::sampleFit(fitPrecisionTmt,
                   n = nTrials)[, precisionTmtDist])

  if(precisionCtrl == "fixed"){
    sigmaCtrlSample <- rep(sqrt(sigmaSqCtrl), nTrials)
  }

  if(precisionCtrl == "iid"){
    fitPrecisionCtrl <- fitPrecisionTmt
    precisionCtrlDist <- precisionTmtDist

    sigmaCtrlSample <- 1 /
      sqrt(SHELF::sampleFit(fitPrecisionCtrl,
                     n = nTrials)[, precisionCtrlDist])
  }

  if(precisionCtrl  == "separate"){
    sigmaCtrlSample <- 1 /
      sqrt(SHELF::sampleFit(fitPrecisionCtrl,
                     n = nTrials)[, precisionCtrlDist])
  }

  if(pDeltaZero > 0){
    nNotEffective <- ceiling(pDeltaZero * nTrials)
    deltaSample[1:nNotEffective] <- 0
    sigmaTmtSample[1:nNotEffective] <- sigmaCtrlSample[1:nNotEffective]
  }

  XCtrlSample <-
    matrix(rnorm(nControl * nTrials, mean = 0,
                 sd = sigmaCtrlSample),
           nrow = nControl,
           ncol = nTrials,
           byrow = TRUE
    )
  XTmtSample <-
    matrix(rnorm(nTreatment * nTrials, mean = deltaSample,
                 sd = sigmaTmtSample),
           nrow = nTreatment,
           ncol = nTrials,
           byrow = TRUE
    )

  sampleDiff <- colMeans(XTmtSample) - colMeans(XCtrlSample)

  ### Define model

  mainModelString = "
model{
  for(i in 1:nTmt) {
    xTreatment[i] ~ dnorm(muTreatment, precisionTreatment)
  }
  for(i in 1:nCtrl){
    xControl[i] ~ dnorm(0,  precisionControl)
  }
  effective ~ dbern(pE)

  muTreatment <- effective * delta

  precisionTreatment <- effective * precisionEffectiveTreatment + (1 - effective) * precisionControl

"

  threshold <- paste0("deltaSuccess <- step(delta - ",
                      deltaThreshold, ") * effective" )

  modelString <- paste(mainModelString,
                       makeJAGSdeltaPrior(fitDelta, 'normal'),
                       makeJAGSprecisionTreatmentPrior(fitPrecisionTmt,
                                                       precisionTmtDist),
                       makeJAGSprecisionControlPrior(fitPrecisionCtrl,
                                                     precisionCtrlDist,
                                                     sigmaSqCtrl),
                       threshold,
                       "}",
                       sep = "\n")



  generatePosterior <- function(i, plots = FALSE){


    initial.values<-list( list(delta = sampleDiff[i],
                               precisionEffectiveTreatment = 1/sigmaTmtSample[i]^2,
                               precisionControl = 1/sigmaCtrlSample[i]^2,
                               effective = 1))

    data.list<-  trialData <- list(nTmt = nTreatment,
                                   nCtrl = nControl,
                                   xTreatment = XTmtSample[, i],
                                   xControl = XCtrlSample[, i],
                                   pE = 1 - pDeltaZero)

    m <- rjags::jags.model(file=textConnection(modelString),
                           data = data.list,
                           inits = initial.values,
                           n.chains = 1,
                           n.adapt = nAdapt,
                           quiet = TRUE)

    samples <- rjags::coda.samples(model = m,
                                   variable.names = c("delta",
                                                      "deltaSuccess"),
                                   n.iter = nIter,
                                   progress.bar = "none")

    if(plots){
      par(mfrow=c(2, 1))
      plot(as.numeric(samples[[1]][, "delta"]), main = "",
           xlab = "iteration",
           ylab = expression(delta),
           type = "l")
      acf(samples[[1]][, "delta"], main = "autocorrelations")
      print(coda::effectiveSize(samples))
    }

    mean(samples[[1]][(nBurn + 1) : nIter, "deltaSuccess"])

  }


  if(diagnostics){
    generatePosterior(1, plots = TRUE)
  }


  no_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(no_cores)

  pE <- parallel::parLapply(cl, 1:nTrials, generatePosterior)
  parallel::stopCluster(cl)
  unlist(pE)


}

