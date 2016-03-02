# IQ_CI
A shiny app to demonstrate Gaussian confidence and credible intervals.

To run, ensure your `shiny` installation and other packages are updated (this is important) 

    install.packages(c('shiny','shinyjs','Cairo','MCMCpack'))

you'll also need to install the `bioconductor` package `svgAnnotation`:

    source("http://bioconductor.org/biocLite.R")
    biocLite("SVGAnnotation")

and then run in R:
    
    shiny::runGitHub( "IQ_CI", "richarddmorey")

to run the `shiny` app. Explanation can be found on the "Instructions" tab.
