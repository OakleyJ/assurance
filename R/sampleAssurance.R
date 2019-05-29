


sampleAssurance <- function(delta, sigmaControl, sigmaTreatment,
                            nControl = 25,
                            nTreatment = nControl,
                            nRep = 10000,
                            size = 0.05){

  # Sample control group and treatment group data
  Xcontrol <- matrix(rnorm(nRep * nControl, 0, sigmaControl),
                     nrow = nControl, ncol = nRep, byrow = T)
  Xtreatment <- matrix(rnorm(nRep * nTreatment, delta, sigmaControl),
                       nrow = nTreatment, ncol = nRep, byrow = T)


  # Compute degrees and freedom and test statistic
  mControl <- colMeans(Xcontrol)
  mTreatment <- colMeans(Xtreatment)

  sighatsqControl <- 1 / (nControl - 1) *
    (nControl * colMeans(Xcontrol ^ 2) - nControl * mControl ^ 2)
  sighatsqTreatment <- 1 / (nTreatment - 1) *
    (nTreatment * colMeans(Xtreatment ^ 2) - nTreatment * mTreatment ^ 2)

  vControl <- sighatsqControl / nControl
  vTreatment <- sighatsqTreatment / nTreatment
  vTotal <- vControl + vTreatment

  nu <- (vTotal) ^ 2 /
    (vControl ^ 2 / (nControl - 1) + vTreatment ^ 2 / (nTreatment - 1))

  test.stat <- (mTreatment - mControl) /
    sqrt(vTotal)


  # Return probability of success,
  # assuming treatment effect needs to be positive
  mean(test.stat > qt(1 - size / 2, nu))


}

