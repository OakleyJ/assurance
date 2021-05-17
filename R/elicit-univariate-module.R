#' @import ggplot2
#' @import shiny
#' @import magrittr
#' @import SHELF


# Module UI function
elicitUnivariateInput <- function(id, label = c("Parameter limits",
                                                "Parameter values",
                                                "Cumulative probabilities")) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
             textInput(ns("limits"), label = h5(label[1]),
                       value = "-1, 2")
      ),
      column(4,
             textInput(ns("values"), label = h5(label[2]),
                       value = "0.25, 0.4, 0.55")
      ),
      column(4,
             textInput(ns("probs"), label = h5(label[3]),
                       value = "0.25, 0.5, 0.75")
      )
    ),
    fluidRow(
      column(4,
             numericInput(ns("pNull"), label = h5("P(treatment effect = 0)"),
                          value = 0.5, min = 0, max = 1)
      ),
      column(4,
             selectInput(ns("dist"), label = h5("Distribution"),
                         choices =  list(Histogram = "hist",
                                         Normal = "normal",
                                         'Student-t' = "t",
                                         Gamma = "gamma",
                                         'Log normal' = "lognormal",
                                         'Log Student-t' = "logt",
                                         Beta = "beta",
                                         'Mirror gamma' = "mirrorgamma",
                                         'Mirror log normal' = "mirrorlognormal",
                                         'Mirror log Student-t' = "mirrorlogt",
                                         'Best fitting' = "best"),
                         selected = "normal")
      ),
      column(4,
             conditionalPanel(
               condition = "input.dist == 't' || input.dist == 'logt' || input.dist == 'mirrorlogt'",
               numericInput(ns("tdf"), label = h5("Student-t degrees of freedom"),
                            value = 3),
               ns = NS(id)
             )
      )


    ),

    fluidRow(
      column(6,
             plotOutput(ns("distPlot"))),
      column(6,
             plotOutput(ns("cdfPlot")))
    )


  )
}




elicitUnivariate <- function(input, output, session){

  limits <- reactive({
    eval(parse(text = paste("c(", input$limits, ")")))
  })


  p <- reactive({
    eval(parse(text = paste("c(", input$probs, ")")))
  })

  v <- reactive({
    eval(parse(text = paste("c(", input$values, ")")))
  })

  myfit <- reactive({
    fitdist(vals = v(), probs = p(), lower = limits()[1],
            upper = limits()[2],
            tdf = input$tdf)
  })

  delta <- reactive({
    x <- sampleFit(myfit(), n = 10000)

    # Modify if P(treatment effect = 0) > 0
    if(input$pNull >= 1/10000 & input$pNull <= 1){
      setToZero <- sample(1:10000, round(input$pNull * 10000))
      x[setToZero, ] <- 0
    }
    x
  })

  deltaSample <- reactive({
    if(input$dist == 'best'){
      index <- as.character(myfit()$best.fitting[1, 1])
    }else{
      index <- input$dist
    }
    delta()[, index]
  })


  output$distPlot <- renderPlot({

    ..density.. <- dlt <-
      NULL # hack to avoid R CMD check NOTE


    if(input$pNull == 0){
      suppressWarnings(plotfit(myfit(), d = input$dist,
                               int = F, ql = 0.05, qu = 0.95,
                               xl = limits()[1], xu = limits()[2],
                               xlab = expression(delta),
                               ylab = expression(f(delta)))) }else{
                                 #fs = input$fs))
                                 data.frame(dlt = deltaSample()) %>%
                                   ggplot(aes(x = dlt, y=..density..))+
                                   geom_histogram(bins = 50,
                                                  center = 0,
                                                  colour = "black",
                                                  fill = "white") +
                                   labs(x = expression(delta))

                               }

  })

  output$cdfPlot <- renderPlot({

    p1 <- SHELF::makeCDFPlot(lower = limits()[1],
                             v = v(),
                             p = p(),
                             upper = limits()[2],
                             fit = myfit(),
                             dist = input$dist,
                             showFittedCDF = TRUE,
                             showQuantiles = FALSE,
                             ql = 0.05,
                             qu = 0.95,
                             ylab = expression(P(delta <=x ~"|"~delta != 0)))
    print(p1)

  })

  list(deltaSample = deltaSample, myfit = myfit,
       deltaDistribution = reactive({input$dist}),
       pNull = reactive({input$pNull}))
}
