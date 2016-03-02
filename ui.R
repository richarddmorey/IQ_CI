require(shiny)


reactiveSvg <- function (outputId) 
{
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

textInput3<-function (inputId, label, value = "",...) 
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,...))
}


shinyUI(
  navbarPage(
    title="Demonstration of Gaussian interval estimates",
    tabPanel(title="Figures",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          HTML('<h4>Prior</h4>'),
          checkboxInput(inputId = "prior1_noninf", label = "Noninformative?"),
          conditionalPanel(
            condition = "!input.prior1_noninf",
            sliderInput("pr1.mu_0", "Mean 'location':", 
                        min = 50, max = 150, value = 100, step= 1),
            sliderInput("pr1.n_0", "Mean 'sample size':", 
                        min = 0, max = 50, value = 5),
            sliderInput("pr1.sig_0", "SD 'location':", 
                        min = 1, max = 25, value = 15),
            sliderInput("pr1.nu_0", "SD 'df':", 
                        min = 1, max = 50, value = 5)
          ),
          HTML('<h4>Data</h4>'),
          sliderInput("data.ybar", "Sample mean:", 
                      min = 50, max = 150, value = 100, step= 1),
          sliderInput("data.s", "Sample SD:", 
                      min = 1, max = 25, value = 15),
          sliderInput("data.N", "Sample size:", 
                      min = 3, max = 100, value = 10),
          HTML('<h4>Setup</h4>'),
          sliderInput("conf.coef", "Credibility coef.:", 
                      min = 1, max = 99, value = 95, step= 1),
          sliderInput("mu_range", "μ plot range:",
                      min = 50, max = 150, value = c(50,150))
        ),
        mainPanel(
          htmlOutput(outputId="svg.grid")
        )
      )
    ),
    tabPanel(title="Details",
      withMathJax(),
      HTML('<h4>Details for the interval estimates</h4>'),
      helpText('The top-row figures show the marginal priors on the two parameters: the population mean (μ) and 
        the population standard deviation (σ). The parameters of these priors can be adjusted by the sliders on the left hand side.
        With the current slider settings, the priors are:'),
      uiOutput(outputId="prior.out"),
      helpText('The Mean \'location\' is the a priori most likely value for μ (the prior mode).'),
      helpText('The Mean \'sample size\' indexes the influence of the prior on μ in sample size units.'),
      helpText('The SD \'location\' can roughly be interpreted as an a priori average estimate of σ.'),
      helpText('The SD \'df\' can roughly be interpreted as the influence of the prior on σ on the results in sample size units.'),
      helpText('The figure in the middle row shows the bivariate likelihood.'),
      helpText('The figure in the bottom row shows the marginal posterior for (μ); the shaded region shows the credible interval. The area under the posterior curve in the shaded region is the credibility coefficient.'),
      helpText('With the current slider settings, the marginal posterior for μ is:'),
      uiOutput(outputId="post.out"),
      uiOutput(outputId="ci.out")


    )
  )
)
