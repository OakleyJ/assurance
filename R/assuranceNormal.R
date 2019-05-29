#' Estimate the assurance for a two-arm trial with normally distributed data
#'
#' Given elicited distributions for the treatment effect
#' and observation variance, this function will simulate observations
#' for a number of trials, and return the proportion of trials that result
#' in a statistically significant difference between the treatment and
#' control groups
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
#' @param nRep the number of simulated trials used in the Monte Carlo estimate
#' of the assurance.
#' @param size the significance level used in the hypothesis test of no treatment
#' effect.
#'
#' @import stats
#' @import graphics
#' @export


assuranceNormal <- function(fitDelta,
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
                            nRep = 10000,
                            size = 0.05){
  deltaSample <- SHELF::sampleFit(fitDelta, n = nRep)
  precisionTmtSample <- SHELF::sampleFit(fitPrecisionTmt, n = nRep)
  if(precisionCtrl == "iid"){
    precisionCtrlSample <- SHELF::sampleFit(fitPrecisionTmt,
                                           n = nRep)
  }
  if(precisionCtrl == "iid"){
    precisionCtrlSample <- SHELF::sampleFit(fitPrecisionTmt,
                                           n = nRep)
  }
  if(precisionCtrl == "separate"){
    precisionCtrlSample <- SHELF::sampleFit(fitPrecisionCtrl,
                                           n = nRep)
  }

  if(precisionCtrl == "fixed"){
    precisionCtrlSample <- rep(1 / sigmaSqCtrl, n = nRep)
  }



  if(pDeltaZero > 0){
    nNotEffective <- ceiling((1 - pDeltaZero) * nRep)
    deltaSample[1:nNotEffective, ] <- 0
    precisionTmtSample[1:nNotEffective, ] <- precisionCtrlSample[1:nNotEffective, ]
  }

  sampleAssurance(delta = deltaSample[, deltaDist],
                  sigmaControl = 1 /
                    sqrt(precisionTmtSample[, precisionTmtDist]),
                  sigmaTreatment = 1 /
                    sqrt(precisionCtrlSample[, precisionCtrlDist]),
                  nControl = nControl,
                  nTreatment = nTreatment,
                  nRep = nRep)

}

