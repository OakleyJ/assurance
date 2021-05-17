#' Launch app for elicitation and assurance for normally distributed data
#
#' @export


assuranceNormalApp <- function(){

ui <- fluidPage(

    titlePanel("Assurance: normally distributed data"),
    tabsetPanel(
        tabPanel("Treatment effect",
                 wellPanel(
                     h4("Instructions"),
                     tags$ol(
                     tags$li("Specify lower and upper parameter limits.
                             These will be used to set the axes ranges in the
                             plots. Note that the gamma, log normal and log t
                             distributions are shifted to have support
                             (lower limit, Infinity), and the beta distribution is
                             scaled and shifted to have support (lower limit,
                             upper limit). For the 'mirror' distributions,
                             (upper limit - X) has a gamma, log normal or log t distribution."),
                     tags$li("Elicit at least two probabilities for the treatment
                             effect Pr(delta < x | delta not 0) = p. Enter the values
                             x in the 'Treatment effect values' box, and the
                             corresponding probabilities p in the 'Cumulative
                             probabilities box'. The smallest probability must
                             be less than 0.4, and the largest probability must
                             be greater than 0.6."),
                     tags$li("Elicit a probability that the treatment effect
                             is equal to 0. (This probability can be set to 0). "),
                     tags$li("Choose which distribution to fit to the elicited
                             judgements about the treatment effect."),
                     tags$li("If a non-zero probability is specified in step 3,
                     the Distribution is displayed approximately with a
                             histogram."))),



                 elicitUnivariateInput("treatmentEffect",
                                       c("Limits",
                                         "Treatment effect values",
                                         "Cumulative probabilities"))
        ),
        tabPanel("Treatment group variance",
                 wellPanel(
                 h4("Instructions"),
                 tags$ol(
                 tags$li("Specify a hypothetical value for the treatment group
                         median"),
                 tags$li("Specify an interval of treatment group responses.
                         One of (-Inf, a), (a, median), (median, b),
                         (b, Inf), for constants a and b."),
                 tags$li("Elicit lower and upper values for the proportion
                         of patients with responses in the specified interval,
                         corresponding to the choices of
                         'Proportion quantiles'."),
                 tags$li("Select either a gamma or log normal distribution,
                         to be fitted to the treatment group precision, using
                         the two elicited values for the proportion."))),


                 elicitPrecisionInput("treatmentVarianceDistribution",
                                      label = c("Treatment group median",
                                                "Treatment group interval",
                                                "Lower/upper proportions",
                                                "Proportion quantiles"))


        ),
        tabPanel("Control group variance",

                 wellPanel(
                     radioButtons(inputId = "controlVar",
                                  h5("Distribution options"),
                                  choices = c("Fixed value" = "fixed",
                                              "Same distribution as treatment group variance" =
                                                  "tmt",
                                              "Separate distribution" =
                                                  "elicit"),
                                  inline = FALSE,
                                  selected = "tmt")
                 ),

                 conditionalPanel(
                     condition = "input.controlVar == 'fixed' ",
                     numericInput("controlSigSq",
                                  label =
                                      h5("Control group variance"),
                                  value = 1)
                 ),


                 conditionalPanel(
                     condition = "input.controlVar == 'elicit'",

                     wellPanel(
                         h4("Instructions"),
                         tags$ol(
                         tags$li("Specify a hypothetical value for the control group
                         median"),
                         tags$li("Specify an interval of control group responses.
                         One of (-Inf, a), (a, median), (median, b),
                         (b, Inf), for constants a and b."),
                         tags$li("Elicit lower and upper values for the proportion
                         of patients with responses in the specified interval,
                         corresponding to the choices of
                         'Proportion quantiles'."),
                         tags$li("Select either a gamma or log normal distribution,
                         to be fitted to the control group precision, using
                         the two elicited values for the proportion."))),


                     elicitPrecisionInput("controlVarianceDistribution",
                                          label = c("Control group median",
                                                    "Control group interval",
                                                    "Lower/upper proportions",
                                                    "Proportion quantiles"))
                 )

        ),
        tabPanel("Assurance",
                 plotOutput("assurancePlot"),
                 wellPanel(
                     h4("Custom sample sizes"),
                     fluidRow(
                         column(4,
                                numericInput("nTmt", h5("Treatment group sample size"),
                                             value = 50)
                         ),
                         column(4,
                                numericInput("nCtrl", h5("Control group sample size"),
                                             value = 50)
                         ),
                         column(4,
                                h5("Assurance"),
                                h5(textOutput("customAssurance"))
                         )
                     )
                 )
        ),
        # tabPanel("Sensitivity analysis",
        #          fluidRow(
        #              column(4,
        #                     numericInput("nTmtSA",
        #                                  h5("Treatment group sample size"),
        #                                  value = 50)
        #              ),
        #              column(4,
        #                     numericInput("nCtrlSA",
        #                                  h5("Control group sample size"),
        #                                  value = 50)
        #              )
        #          ),
        #
        #          plotOutput("robustPlot")
        # ),
        tabPanel("Interim analysis",
                 wellPanel(
                     h4("Instructions"),
                     tags$ol(
                         tags$li("Specify numbers of patients in the treatment
                                 and control arms"),
                         tags$li("Specify a desired threshold, such that
                                 you would wish to know
                                 P(treatment effect > threshold | trial data)"),
                         tags$li("Choose how many trials to simulate (try the
                                 default number first, to see how long the
                                 simulation takes to run)."),
                         tags$li("Click on 'Run simulation'. ")),
                         tags$p("This will simulate data from trials with the specified
                                sample sizes. For each simulated trial, the posterior
                                probability that the treatment effect is above the
                                specified threshold is estimated. A histogram will
                                show the distribution of these posterior probabilties,
                                over the specified number of simulated trials. For the
                                data to be 'informative', posterior probabilities close to 0
                                or 1 would be desirable.")


                 ),

                 fluidRow(
                     column(3,
                            numericInput("nTmtInterim",
                                         h5("Treatment group sample size"),
                                         value = 10)
                     ),
                     column(3,
                            numericInput("nCtrlInterim",
                                         h5("Control group sample size"),
                                         value = 10)
                     ),
                     column(3,
                            numericInput("deltaThreshold",
                                         h5("Threshold"),
                                         value = 0)
                     ),
                     column(3,
                            numericInput("nJAGS",
                                         h5("Number of simulated trials"),
                                         value = 100)
                     )

                 ),
                 fluidRow(

                     column(3,
                            actionButton("goButton", "Run simulation")
                     )

                 ),

                 plotOutput("predictivePlot")

        ),
        tabPanel("About this app",
                 includeHTML(system.file("shinyAppFiles", "help.html",
                                         package="assurance")))

    )
)


server <- function(input, output) {
    tmtEffect <- callModule(elicitUnivariate, "treatmentEffect")
    precisionTmt <- callModule(elicitPrecision, "treatmentVarianceDistribution")
    precisionCtrl <- callModule(elicitPrecision, "controlVarianceDistribution")


    output$assurancePlot <- renderPlot({
        n <- c(25, 50, 100, 250, 500, 1000)
        assur <- rep(0, 6)
        withProgress(message = 'Estimating assurance', value = 0,
                     for(i in 1:6){
                         assur[i] <- sampleAssurance(
                             delta = tmtEffect$deltaSample(),
                             sigmaControl = sigmaCtrlSample(),
                             sigmaTreatment = precisionTmt$sigmaSample(),
                             nControl = n[i])
                         incProgress(1/6)


                     }
        )
        priorProb <- round(mean(tmtEffect$deltaSample() > 0), 2)
        data.frame(n, assur) %>%
            ggplot(aes(x = n, y  = assur ))+
            geom_point(size = 2)+
            scale_x_continuous(breaks = n, name = "sample size per arm",
                               minor_breaks = NULL)+
            labs(y = "assurance") +
            geom_hline(yintercept = priorProb, linetype = "dashed",
                       col = "blue") +
            annotate("text", x = 750, y = priorProb + 0.05,
                     label = "Prior probability treatment is effective",
                     col = "blue") +
            ylim(0, 1)
        # plot(n, assur, pch = 16, ylim = c(0, 1),
        #      xlab = "Sample size (per arm)",
        #      ylab = "Assurance",
        #      xaxp = c(25, 1000, 1),
        #      yaxp = c(0,1, 1), las = 1)
        # axis(1, c(50, 100, 250, 500))
        # axis(2, round(c(assur[1], priorProb ), 2), las = 1)
        # abline(h = priorProb, lty = 2)
    })

    output$customAssurance <- renderText({
        round(sampleAssurance(
            delta = tmtEffect$deltaSample(),
            sigmaControl = sigmaCtrlSample(),
            sigmaTreatment = precisionTmt$sigmaSample(),
            nControl = input$nCtrl,
            nTreatment = input$nTmt), 2)

    })

    sigmaCtrlSample <- reactive({

        if(input$controlVar == 'fixed'){
            s <- rep(sqrt(input$controlSigSq), 10000)
        }
        if(input$controlVar == 'tmt'){
            s <- precisionTmt$sigmaSample()
        }

        if(input$controlVar == 'elicit'){
            s <- precisionCtrl$sigmaSample()
        }
        s
    })

    # output$robustPlot <- renderPlot({
    #     propRange <- c(seq(from = 0.001, to = 0.45, length = 10),
    #                    precisionTmt$props())
    #
    #     if(precisionTmt$interval()[1] == -Inf){
    #         sigma <- (precisionTmt$interval()[2] - precisionTmt$med())/
    #             qnorm(propRange)
    #         intervalText <- paste("Proportion in [-infinity, ",
    #                               precisionTmt$interval()[2],"]", sep = "")
    #     }
    #
    #     if(precisionTmt$interval()[2] == Inf){
    #         sigma <- (precisionTmt$interval()[1] - precisionTmt$med())/
    #             qnorm(1 - propRange)
    #         intervalText <- paste("Proportion in [",
    #                               precisionTmt$interval()[1],
    #                               ", infinity]", sep = "")
    #     }
    #
    #     if(all(is.finite(precisionTmt$interval()))){
    #         if(precisionTmt$interval()[1] == precisionTmt$med()){
    #             sigma <- (precisionTmt$interval()[2] - precisionTmt$med())/
    #                 qnorm(0.5 + propRange)}else{
    #                     sigma <- (precisionTmt$interval()[1] -
    #                                   precisionTmt$med())/
    #                         qnorm(0.5 - propRange)
    #
    #                 }
    #         intervalText <- paste("Proportion in [",
    #                               precisionTmt$interval()[1],
    #                               ", ",
    #                               precisionTmt$interval()[2],
    #                               "]", sep = "")
    #     }
    #
    #
    #
    #     ass <- rep(0, length(propRange))
    #     for(i in seq_along(ass)){
    #         ass[i] <- sampleAssuranceSigma(tmtEffect$deltaSample(),
    #                                        sigmaControl = sigma[i],
    #                                        sigmaTreatment = sigma[i],
    #                                        nControl = input$nCtrlSA,
    #                                        nTreatment = input$nTmtSA)
    #     }
    #
    #     priorProb <- mean(tmtEffect$deltaSample() > 0)
    #
    #     data.frame(propRange = propRange, ass = ass) %>%
    #         ggplot(aes(x = propRange, y = ass))+
    #         geom_line(lwd = 2) +
    #         labs(x = intervalText, y = "assurance") +
    #         geom_hline(yintercept = priorProb,
    #                    linetype = "dashed", lwd = 2,
    #                    colour = "blue") +
    #         annotate("text", x = 0.1, y = priorProb + 0.05,
    #                  label = "Prior probability treatment is effective",
    #                  colour = "blue") +
    #         ylim(0, 1) +
    #         annotate("point", x= propRange[11:12],y= ass[11:12],
    #                  colour = "red",
    #                  size = 5)
    #     # plot(propRange, ass, type = "l", ylim = c(0, 1),
    #     #      xlab = intervalText,
    #     #      ylab = "assurance",
    #     #      lwd = 2)
    #     # abline(h = , lty = 2, lwd = 2)
    #     # points(propRange[11:12], ass[11:12], pch = 16, cex = 2, col = "red")
    #
    # })

    output$predictivePlot <- renderPlot({

        if(input$goButton>0){
            showNotification("Running simulation.
                             This may take a while...", action = NULL, duration = NULL, closeButton = FALSE,
                             id = "updating", type = c("message"))
            if(isolate(input$controlVar) == "fixed"){
                fPC <- 1 / isolate(input$controlSigSq)
            }else{
                fPC <- NULL
            }

            if(isolate(input$controlVar) == "tmt"){
                pC <- isolate(precisionTmt$precisionfit())
                pCD <- isolate(precisionTmt$dist())
            }

            if(isolate(input$controlVar) == "elicit"){
                pC <- isolate(precisionCtrl$precisionfit())
                pCD <- isolate(precisionCtrl$dist())
            }



            pE <- sampleInterim(nTreatment = isolate(input$nTmtInterim),
                                nControl = isolate(input$nCtrlInterim),
                                fitDelta = isolate(tmtEffect$myfit()),
                                fitPrecisionTmt =
                                    isolate(precisionTmt$precisionfit()),
                                fitPrecisionCtrl = pC,
                                deltaDist =
                                    isolate(tmtEffect$deltaDistribution()),
                                precisionTmtDist =
                                    isolate(precisionTmt$dist()),
                                precisionCtrlDist = pCD,
                                pDeltaZero = isolate(tmtEffect$pNull()),
                                deltaThreshold =
                                    isolate(input$deltaThreshold),
                                precisionCtrl = "iid",
                                sigmaSqCtrl = fPC,
                                diagnostics = FALSE,
                                nTrials = isolate(input$nJAGS),
                                nIter = 1000,
                                nAdapt = 1000,
                                nBurn = 0)
            removeNotification("updating")



            histData <- hist(pE, breaks = (0:20)/20, plot = FALSE)
            ..density.. <- NULL # hack to avoid R CMD check NOTE

            data.frame(pE = pE) %>%
                ggplot(aes(x = pE, y = ..density..))+
                geom_histogram(breaks = (0:20)/20,
                               colour = "blue",
                               fill = "white") +
                scale_x_continuous(breaks = (0:5)/5,
                                   minor_breaks = (0:20)/20) +
                annotate("text", x = histData$mids,
                         y = 1.3 * max(histData$density),
                         label = histData$counts /
                             sum(histData$counts))+
                labs(x = expression(P(delta>threshold~"|"~trial~data)),
                     title = "Predictive distribution of posterior probability of desired treatment effect, given data from the interim analysis ")

        }

    })




}

# Run the application
shinyApp(ui = ui, server = server)

}
