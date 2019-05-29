

# Module UI function
elicitPrecisionInput <- function(id, label = c("Median",
                                               "Interval",
                                               "Lower, upper proportions",
                                               "Proportion quantiles")) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fluidRow(
      column(3,
             numericInput(ns("med"), label = h5(label[1]),
                       value = 0.4)
      ),
      column(3,
             textInput(ns("interval"), label = h5(label[2]),
                       value = "-Inf, 0.2")
      ),
      column(3,
             textInput(ns("props"), label = h5(label[3]),
                       value = "0.2, 0.4")
      ),
      column(3,
             textInput(ns("quantiles"), label = h5(label[4]),
                       value = "0.05, 0.95")
      )
    ),
    fluidRow(
      column(6,
    plotOutput(ns("precisionPlot"))),
    column(6,
    plotOutput(ns("priorSD")))
    ),
    selectInput(ns("dist"), label = h5("Selected distribution"),
                choices =  list(Gamma = "gamma",
                                'Log normal' = "lognormal"),
                selected = "Gamma"
    )
  )
}



elicitPrecision <- function(input, output, session){

  interval <- reactive({
    eval(parse(text = paste("c(", input$interval, ")")))
  })


  props <- reactive({
    eval(parse(text = paste("c(", input$props, ")")))
  })

  quantiles <- reactive({
    eval(parse(text = paste("c(", input$quantiles, ")")))
  })


  precisionfit <- reactive({
    fitprecision(interval(), props(), med = input$med,
                 pplot = FALSE, propprobs = quantiles()
            )
  })

  allSigmaSamples <- reactive({
    1 / sqrt(sampleFit(precisionfit(),
                       n = 10000)[, c("gamma", "lognormal")])
  })

  sigmaSample <- reactive({
    allSigmaSamples()[, input$dist]
  })

  displayQuantiles <- reactive({
    quantile(sigmaSample(), quantiles())
  })

  output$precisionPlot <- renderPlot({
    fitprecision(interval(), props(), med = input$med,
                 propprobs = quantiles())
  })

  output$priorSD <- renderPlot({

    ..density.. <- distribution <-
      NULL # hack to avoid R CMD check NOTE


    data.frame(allSigmaSamples()) %>%
      tidyr::gather(key = "distribution",
             value = "sigma") %>%
      ggplot(aes(x = sigma, y = ..density..)) +
      geom_density(aes(fill = distribution), alpha = 0.5)+
      geom_vline(xintercept = displayQuantiles(), color = "red",
                 linetype = "dashed") +
      labs(x = expression(sigma),
           title = "Elicited distributions for the standard deviation",
           fill = "Distribution \nfor precision")
  })

  return(list(precisionfit = precisionfit,
              interval = interval,
              med = reactive({input$med}),
              props = props,
              sigmaSample = sigmaSample,
              dist = reactive({input$dist})))

}
