

makeJAGSdeltaPrior <- function(fit, distribution){
  # "normal", "t", "gamma", "lognormal", "logt","beta"

  if(distribution == "normal" | distribution == "t"){
    return(
      paste("delta", "~ dnorm(", fit$Normal[1, "mean"],
            ",",
            1 / fit$Normal[1, "sd"]^2,
            ")")
    )
  }

  if(distribution == "gamma"){
    return(
      paste("delta", "~ dgamma(", fit$Gamma[1, "shape"],
            ",",
            fit$Gamma[1, "rate"],
            ")")
    )
  }

  if(distribution == "lognormal" | distribution == "logt"){
    return(
      paste("delta", "~ dlnorm(", fit$Log.normal[1, "mean.log.X"],
            ",",
            1/fit$Log.normal[1, "sd.log.X"]^2,
            ")")
    )
  }

  if(distribution == "beta" ){
    return(
      paste("delta", "~ dbeta(", fit$Beta[1, "shape1"],
            ",",
            fit$Beta[1, "shape2"],
            ")")
    )
  }

}


makeJAGSprecisionTreatmentPrior <- function(fit, distribution){

  if(distribution == "gamma"){
    return(
      paste("precisionEffectiveTreatment",
            "~ dgamma(", fit$Gamma[1, "shape"],
            ",",
            fit$Gamma[1, "rate"],
            ")")
    )
  }

  if(distribution == "lognormal"){
    return(
      paste("precisionEffectiveTreatment",
            "~ dlnorm(", fit$Log.normal[1, "mean.log.X"],
            ",",
            fit$Log.normal[1, "sd.log.X"],
            ")")
    )
  }

}

makeJAGSprecisionControlPrior <- function(fit = NULL, distribution = NULL, value = NULL){
  if(is.null(value)){
  if(distribution == "gamma"){
    return(
      paste("precisionControl",
            "~ dgamma(", fit$Gamma[1, "shape"],
            ",",
            fit$Gamma[1, "rate"],
            ")")
    )
  }

  if(distribution == "lognormal"){
    return(
      paste("precisionControl",
            "~ dlnorm(", fit$Log.normal[1, "mean.log.X"],
            ",",
            fit$Log.normal[1, "sd.log.X"],
            ")")
    )
  }
  }
  if(!is.null(value)){
    return(
      paste("precisionControl <-", 1 / value)
    )
  }


}
