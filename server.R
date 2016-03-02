require(lattice)
require(stats)
require(shiny)
require("RColorBrewer")
require("SVGAnnotation")
require("XML")
require(grImport)
require(shinyjs)
require("gridSVG")
require(MCMCpack)

options(shiny.usecairo=FALSE)

source('utility.R')



shinyServer(function(input,output,session) {

  output$prior.out <- renderUI({

    mu_0 = input$pr1.mu_0
    n_0 = input$pr1.n_0
    sig_0 = input$pr1.sig_0
    sig2_0 = sig_0^2
    nu_0 = input$pr1.nu_0
    
    N = input$data.N
    s2 = (input$data.s)^2
    ybar = input$data.ybar
    
    if(input$prior1_noninf){
      yy = dnormposterior(0, ybar, s2, N)
    }else{
      yy = dnormposterior(0, ybar, s2, N, mu_0, n_0, nu_0, sig2_0)
    }
    
    str1 = paste('
    $$\\begin{eqnarray}
    \\mu &\\sim& \\mbox{Normal}(',yy$mu_0,',\\sigma^2/',yy$n_0,')\\\\
    \\delta = \\frac{\\mu - ',yy$mu_0,'}{\\sigma} &\\sim& \\mbox{Normal}(0,1/',yy$n_0,')\\\\
    \\sigma^2 &\\sim& \\mbox{Inverse Gamma}(',yy$nu_0/2,',',yy$nu_0*yy$sig2_0/2,')
    \\end{eqnarray}
    $$')

    withMathJax(helpText(str1))
  })
  
  output$post.out <- renderUI({
    
    mu_0 = input$pr1.mu_0
    n_0 = input$pr1.n_0
    sig_0 = input$pr1.sig_0
    sig2_0 = sig_0^2
    nu_0 = input$pr1.nu_0
    
    N = input$data.N
    s2 = (input$data.s)^2
    ybar = input$data.ybar
    
    if(input$prior1_noninf){
      yy = dnormposterior(0, ybar, s2, N)
    }else{
      yy = dnormposterior(0, ybar, s2, N, mu_0, n_0, nu_0, sig2_0)
    }
    
    str1 = paste('
                $$\\begin{eqnarray}
                \\mu\\mid \\bar{y},s^2 &\\sim& \\mbox{Shifted, scaled}~t_{',yy$nu_1,'}(',round(yy$mu_1,2),',',round(yy$posterior_se,2),')
                \\end{eqnarray}
                $$')
    
    withMathJax(helpText(str1))
  })
  
  
  output$ci.out <- reactive({
    
    mu_0 = input$pr1.mu_0
    n_0 = input$pr1.n_0
    sig_0 = input$pr1.sig_0
    sig2_0 = sig_0^2
    nu_0 = input$pr1.nu_0
    
    N = input$data.N
    s2 = (input$data.s)^2
    ybar = input$data.ybar
    
    if(input$prior1_noninf){
      yy = dnormposterior(0, ybar, s2, N)
    }else{
      yy = dnormposterior(0, ybar, s2, N, mu_0, n_0, nu_0, sig2_0)
    }
    
    conf.coef = input$conf.coef
    alpha = 1-conf.coef/100
    
    cred = round(qt(c(alpha/2,1-alpha/2), yy$nu_1)*yy$posterior_se + yy$mu_1,2)
    conf = round(qt(c(alpha/2,1-alpha/2), N-1)*sqrt(s2/N) + ybar,2)
    
    return(paste(
      "The ",input$conf.coef,"% credible interval for μ is [",cred[1],",",cred[2],"].<br/>",
      "The ",input$conf.coef,"% confidence interval for μ is [",conf[1],",",conf[2],"].",
      
      collapse="")
      )
  })
  
  output$svg.grid <- renderText({
    #from lattice package documentation

    mu_0 = input$pr1.mu_0
    n_0 = input$pr1.n_0
    sig_0 = input$pr1.sig_0
    sig2_0 = sig_0^2
    nu_0 = input$pr1.nu_0
    
    N = input$data.N
    s2 = (input$data.s)^2
    ybar = input$data.ybar
    
    mu_range = input$mu_range
    
    conf.coef = input$conf.coef
    alpha = 1-conf.coef/100
    
    if(input$prior1_noninf){
      pars = dnormposterior(0, ybar, s2, N)
    }else{
      pars = dnormposterior(0, ybar, s2, N, mu_0, n_0, nu_0, sig2_0)
    }
    xx.mu = seq(mu_range[1],mu_range[2],len=50)
    xx.more.mu = qt(seq(.01,.99,.01), pars$nu_1)*pars$posterior_se + pars$mu_1
    xx.mu = sort(c(xx.mu,xx.more.mu))
    
    doc = svgPlot( {
      
      layout(matrix(c(1,3,4,2,3,4),3,2))
      par(mar=c(4,4,1,.1),mgp=c(2.5,1,0))

      if(input$prior1_noninf){
        plot(0,0,ylim=c(-1,1),xlim=c(-1,1),ty='n',axes=FALSE,ylab="",xlab="")
        text(0,0,"Noninformative")
        plot(0,0,ylim=c(-1,1),xlim=c(-1,1),ty='n',axes=FALSE,ylab="",xlab="")
        text(0,0,"Noninformative")
        
        yy = dnormposterior(xx.mu, ybar, s2, N)
      }else{
        ## Marginal prior on mu
        
        err = try({
        plot(xx.mu, dt( (xx.mu - mu_0)/sqrt(sig2_0/n_0), nu_0 )/sqrt(sig2_0/n_0), ty='l', ylab="Prior density",
             xlab=expression(paste("Population Mean, ", mu)),axes=FALSE)
        axis(1) 
        abline(h=0,col="gray")
        })
        if(inherits(err, "try-error")){
          plot(0,0,ylim=c(-1,1),xlim=c(-1,1),ty='n',axes=FALSE,ylab="",xlab="")
          text(0,0,"Improper")
        }

        ## Marginal prior on sig2
        xx = seq(0,25,len=200)
        
        err = try({
          plot(xx, dinvgamma(xx^2, nu_0/2, sig2_0*nu_0/2)*2*xx, ty='l', ylab="Prior density",
               xlab=expression(paste("Population SD, ", sigma)),axes=FALSE)
          axis(1)
          abline(h=0,col="gray")
          })
        if(inherits(err, "try-error")){
          plot(0,0,ylim=c(-1,1),xlim=c(-1,1),ty='n',axes=FALSE,ylab="",xlab="")
          text(0,0,"Improper")
        }
        yy = dnormposterior(xx.mu, ybar, s2, N, mu_0, n_0, nu_0, sig2_0)
        
      }
    

    
    # Likelihood
    xx.mu2 = seq(mu_range[1],mu_range[2],len=100)
    ci.s2 = (N-1)*s2/qchisq(c(.975,.025),N-1)
    xx.sigma = seq(0,40,len=100)
    
    z = outer(xx.mu2,xx.sigma, Vectorize(function(mu,sigma){
      dnorm(ybar,mu,sigma/sqrt(N))*dchisq((N-1)*s2/sigma^2,N-1)/sigma^2
    },c("mu","sigma")))
    
    image(xx.mu2,xx.sigma,z,ylab=expression(sigma),xlab=expression(mu),col=terrain.colors(30))
    contour(xx.mu2,xx.sigma,z,add=TRUE,drawlabels=FALSE,col=rgb(1,1,1,.3))
    
    # Marginal posterior
    plot(xx.mu, yy$dens, ylab="Posterior density",
         xlab=expression(paste("Population mean ", mu)),
         ty='l',axes=FALSE)
    abline(h=0,col="gray")
    axis(1)
    cred = qt(c(alpha/2,1-alpha/2), yy$nu_1)*yy$posterior_se + yy$mu_1
    rect(cred[1],par()$usr[3],cred[2],par()$usr[4],border=NA,col=rgb(0,0,0,.1))
    }, height = 10, width = 6, pointsize = 10)  

    
    tempsvg <- tempfile(fileext=".svg")
    on.exit(unlink(tempsvg))
    saveXML(doc, tempsvg)
    svgoutput <- readLines(con = tempsvg, n=-1)
    svgoutput
  }) 



})