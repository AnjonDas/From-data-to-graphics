#File saving and loading directory####


setwd("F:/R Programming/From data to Graphics")


#Exporting table from R####

 ?faithful
 data(faithful)
 faithful
 

 str(faithful)
 summary(faithful$eruptions)
 mad(faithful$eruptions)   #Median Absolute Deviation (MAD) 
 sd(faithful$eruptions)
 IQR(faithful$eruptions) 

 #Droves of numbers####

 str( sort(faithful$eruptions) )  #There is a simple way of seeing someting in that bunch of numbers: just sort them. That is better, but we still have hundreds of numbers, we still do not see anything.
 #stem-and-leaf plot####
  stem(faithful$eruptions) 
 
 #Stripchart (scatterplot)####
 data(faithful)      #One can graphically represent a univariate series by putting the data on an axis
 stripchart(faithful$eruptions, main="The \"stripchart\" function")


 
  #You can also plot the sorted data####
 op <- par(mar=c(3,4,2,2)+.1)
 plot( sort( faithful$eruptions ),
       xlab = "" 
 )
 par(op)
 
 

  op <- par(mar=c(3,4,2,2)+.1)
 plot(sort(faithful$eruptions), xlab="")
 rug(faithful$eruptions, side=2)
 par(op)

 
 
 op <- par(mar=c(3,4,2,2)+.1)
 x <- round( rnorm(100), digits=1 )
 plot(sort(x))
 rug(jitter(x), side=2)
 par(op) 
 
 
 
 #Cumulated frequencies####
 #You can also plot the cumulated frequencies (this plot is symetric to the previous one)
 cumulated.frequencies <- function (x, main="") {
   x.name <- deparse(substitute(x))
   n <- length(x)
   plot( 1:n ~ sort(x), 
         xlab = x.name, 
         ylab = 'Cumulated frequencies',
         main = main
   )
 }    
 cumulated.frequencies(faithful$eruptions,
                       main = "Eruption lengths")

 
 
 data(islands) 
 dotchart(islands, main="Island area") 

 
 dotchart(sort(log(islands)), main="Island area (logarithmic scale)") 
 
 
 
 #cumulative distribution curve####
 op <- par(mfcol=c(2,4), mar=c(2,2,1,1)+.1)
 do.it <- function (x) {
   hist(x, probability=T, col='light blue',
        xlab="", ylab="", main="", axes=F)
   axis(1)
   lines(density(x), col='red', lwd=3)
   x <- sort(x)
   q <- ppoints(length(x))
   plot(q~x, type='l',
        xlab="", ylab="", main="")
   abline(h=c(.25,.5,.75), lty=3, lwd=3, col='blue')
 }
 n <- 200
 do.it(rnorm(n))
 do.it(rlnorm(n))
 do.it(-rlnorm(n))
 do.it(rnorm(n, c(-5,5)))
 par(op)

 
 
 N <- 2000
 x <- rnorm(N)
 op <- par(mar=c(0,0,0,0), oma=c(0,0,0,0)+.1)
 layout(matrix(c(1,1,1,2), nc=1))
 y <- ppoints( length(x) )
 plot(sort(x), y, type="l", lwd=3,
      xlab="", ylab="", main="")
 abline(h=c(0,.25,.5,.75,1), lty=3)
 abline(v = quantile(x), col = "blue", lwd = 3, lty=2)
 points(quantile(x), c(0,.25,.5,.75,1), lwd=10, col="blue")
 boxplot(x, horizontal = TRUE, col = "pink", lwd=5)  
 abline(v = quantile(x), col = "blue", lwd = 3, lty=2)
 par(new=T)
 boxplot(x, horizontal = TRUE, col = "pink", lwd=5)  
 par(op) 

 
 
 #adjust plot margins
 par(mar = c(1, 1, 1, 1))
  plot(1:30)
 
 #Box-and-whiskers plot####
 boxplot(faithful$eruptions, range=0)
 

 
 #The name of this plot is more understandable if it is drawn horizontally.
 boxplot(faithful$eruptions, range=0, horizontal=TRUE)

 
 
 op <- par(mfrow=c(1,2), mar=c(3,2,4,2)+.1)
 do.it <- function (x, xlab="", ylab="", main="") {
   d <- density(x)
   plot(d, type='l', xlab=xlab, ylab=ylab, main=main)
   q <- quantile(x)
   do.it <- function (i, col) {
     x <- d$x[i]
     y <- d$y[i]
     polygon( c(x,rev(x)), c(rep(0,length(x)),rev(y)), border=NA, col=col )
   }
   do.it(d$x <= q[2], 'red')
   do.it(q[2] <= d$x & d$x <= q[3], 'green')
   do.it(q[3] <= d$x & d$x <= q[4], 'blue')
   do.it(d$x >= q[4], 'yellow')
   lines(d, lwd=3)
 }
 do.it( rnorm(2000), main="Gaussian" )
 do.it( rexp(200), main="Exponential" )
 par(op)
 mtext("Quartiles", side=3, line=3, font=2, cex=1.2) 

 
 
 boxplot(faithful$eruptions, horizontal = TRUE,
         main = "No outliers") 
 
 
 
 # There are outliers, they might bring trouble, 
 # but it is normal, it is not pathological
 boxplot(rnorm(500), horizontal = TRUE,
         main = "Normal outliers")

 
 
# If there are only a few outliers, really isolated, they might be errors -- yes, in the real life, the data is "dirty"... 
 x <- c(rnorm(30),20)
 x <- sample(x, length(x))
 boxplot( x, horizontal = TRUE,
          main = "An outlier" )

 
 library(boot)
 data(aml)
 boxplot( aml$time, horizontal = TRUE,
          main = "An outlier" ) 

 
 
 #They can also be the sign that the distribution is not gaussian at all. 
 data(attenu)
 boxplot(attenu$dist, horizontal = TRUE,
         main = "Non gaussian (asymetric) data") 

 
 #Then, we usually transform the data, by applying a simple and well-chosen function, so that it becomes gaussian (more about this later). 
 
 data(attenu)
 boxplot(log(attenu$dist), horizontal = TRUE,
         main = "Transformed variable") 

 
 
x <- aml$time
  summary(x)

 y <- x[x<160]
  summary(y)
  
  y <- c(rnorm(10+100+1000+10000+100000))
  x <- c(rep(1,10), rep(2,100), rep(3,1000), rep(4,10000), rep(5,100000))
  x <- factor(x)
  plot(y~x, 
       horizontal = TRUE, 
       col = "pink",
       las = 1,
       xlab = "", ylab = "",
       main = "The larger the sample, the more outliers")

  
  #confidence interval on the median####
 
  boxplot(faithful$eruptions, 
          notch = TRUE,
          horizontal = TRUE,
          main = "Confidence interval on the median...")

  
  library(boot)
  data(breslow)
  boxplot(breslow$n, 
          notch = TRUE,
          horizontal = TRUE, 
          col = "pink",
          main = "...that goes beyond the quartiles") 

  
  
  boxplot(faithful$eruptions, # Scatter Plot Adding
          horizontal = TRUE,
          col = "pink")
  rug(faithful$eruption, 
      ticksize = .2) 
 
  
  
  #Histogram and density####
  hist(faithful$eruptions)
  hist(faithful$eruptions, breaks=20, col="light blue") 

  
  
  op <- par(mfrow=c(2,1), mar=c(2,2,2,1)+.1)
  hist(faithful$eruptions, breaks=seq(1,6,.5), 
       col='light blue',
       xlab="", ylab="", main="")
  hist(faithful$eruptions, breaks=.25+seq(1,6,.5), 
       col='light blue',
       xlab="", ylab="", main="")
  par(op)
  mtext("Is the first peak symetric or not?", 
        side=3, line=2.5, font=2.5, size=1.5) 

  
  hist(faithful$eruptions, 
       probability=TRUE, breaks=20, col="light blue",
       xlab="", ylab="",
       main="Histogram and density estimation")
  points(density(faithful$eruptions, bw=.1), type='l', 
         col='red', lwd=3) 

  
  #Density estimations still have the first problem of histograms: a different kernel may yield a completely different curve -- but the second problem disappears. 
 
  hist(faithful$eruptions, 
       probability=TRUE, breaks=20, col="light blue",
       xlab="", ylab="",
       main="Histogram and density estimation")
  points(density(faithful$eruptions, bw=1),  type='l', 
         lwd=3, col='black')
  points(density(faithful$eruptions, bw=.5), type='l', 
         lwd=3, col='blue')
  points(density(faithful$eruptions, bw=.3), type='l', 
         lwd=3, col='green')
  points(density(faithful$eruptions, bw=.1), type='l', 
         lwd=3, col='red')

  
  
  #One can add many other elements to a histogram. For instance, a scatterplot, or a gaussian density (to compare with the estimated density). 
  hist(faithful$eruptions, 
       probability=TRUE, breaks=20, col="light blue",
       main="")
  rug(faithful$eruptions)
  points(density(faithful$eruptions, bw=.1), type='l', lwd=3, col='red')
  f <- function(x) {
    dnorm(x, 
          mean=mean(faithful$eruptions), 
          sd=sd(faithful$eruptions),
    ) 
  }
  curve(f, add=T, col="red", lwd=3, lty=2)

  
  
  
  #Symetry plot (seldom used)####
  symetry.plot <- function (x0, 
                            main="Symetry plot",
                            breaks="Sturges", ...) {
    x <- x0[ !is.na(x0) ]
    x <- sort(x)
    x <- abs(x - median(x))
    n <- length(x)
    nn <- ceiling(n/2)
    plot( x[n:(n-nn+1)] ~ x[1:nn] ,
          xlab='Distance below median',
          ylab='Distance above median', 
          main=main,
          ...)
    abline(0,1, col="blue", lwd=3)  
    op <- par(fig=c(.02,.5,.5,.98), new=TRUE)
    hist(x0, probability=T, breaks=breaks,
         col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x0), col="red", lwd=2)
    box()
    par(op)
  }
  
  symetry.plot(rnorm(500), 
               main="Symetry plot (gaussian distribution)")

  
  symetry.plot(rexp(500),
               main="Symetry plot (exponential distribution)") 
  symetry.plot(-rexp(500),
               main="Symetry plot (negative skewness)") 
  symetry.plot(rexp(500),
               main="Symetry plot, logarithmic scales)") 
  symetry.plot(faithful$eruptions, breaks=20) 

  
  
  symetry.plot.2 <- function (x, N=1000, 
                              pch=".", cex=1, ...) {
    x <- x[ !is.na(x) ]
    x <- sort(x)
    x <- abs(x - median(x))
    n <- length(x)
    nn <- ceiling(n/2)
    plot( x[n:(n-nn+1)] ~ x[1:nn] ,
          xlab='Distance below median',
          ylab='Distance above median', 
          ...)
    for (i in 1:N) {
      y <- sort( rnorm(n) )
      y <- abs(y - median(y))
      m <- ceiling(n/2)
      points( y[n:(n-m+1)] ~ y[1:m], 
              pch=pch, cex=cex, col='red' )
    }
    points(x[n:(n-nn+1)] ~ x[1:nn] , ...)
    abline(0,1, col="blue", lwd=3)  
  }
  
  n <- 100
  symetry.plot.2( rnorm(n), pch='.', lwd=3, 
                  main=paste("Symetry plot: gaussian,", n, "observations")) 

  n <- 10
  symetry.plot.2( rnorm(n), pch=15, lwd=3, type="b", cex=.5,
                  main=paste("Symetry plot: gaussian,", n, "observations")) 

  
  
  
  robust.symetry.plot(rnorm(100), N=100, pch=16)

  
  
  #Quantile-Quantile plot (important) ####
  data(airquality)
  x <- airquality[,4]
  hist(x, probability=TRUE, breaks=20, col="light blue")
  rug(jitter(x, 5))
  points(density(x), type='l', lwd=3, col='red')
  f <- function(t) {
    dnorm(t, mean=mean(x), sd=sd(x) ) 
  }
  curve(f, add=T, col="red", lwd=3, lty=2)

  
  
  # gaussian quantiles versus the sample quantile####
  x <- airquality[,4]
  qqnorm(x)
  qqline(x,
         col="red", lwd=3)

 
   y <- rnorm(100)
  qqnorm(y, main="Gaussian random variable")
  qqline(y, 
         col="red", lwd=3) 

   
  y <- rnorm(100)^2
  qqnorm(y, main="Non gaussian variable")
  qqline(y,
         col="red", lwd=3)

  
 # we can overlay several gaussian qqplots to our plot, to see how far from gaussian our data are 
  my.qqnorm <- function (x, N=1000, ...) {
    op <- par()
    x <- x[!is.na(x)]
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    print("a")
    qqnorm(x, axes=F, ...)
    for (i in 1:N) {
      par(new=T)
      qqnorm(rnorm(n, mean=m, sd=s), col='red', pch='.', 
             axes=F, xlab='', ylab='', main='')
    }
    par(new=T)
    qqnorm(x, ...)
    qqline(x, col='blue', lwd=3)
    par(op)
  }
  my.qqnorm(rnorm(100), 
            main = "QQplot: Gaussian distribution")

  
  my.qqnorm(runif(100), 
            main = "uniform distribution")  

  
  my.qqnorm(exp(rnorm(100)), 
            main = 'log-normal distribution') 

 
  
  my.qqnorm(c(rnorm(50), 5+rnorm(50)), 
            main = 'bimodal distribution') 

  
  my.qqnorm(c(rnorm(50), 20+rnorm(50)), 
            main = 'two remote peaks')  

  
  x <- rnorm(100)
  x <- x + x^3
  my.qqnorm(x, main = 'fat tails')  

  
  #Two distributions shifted to the left.  
  y <- exp(rnorm(100))
  qqnorm(y, 
         main = '(1) Log-normal distribution')
  qqline(y, 
         col = 'red', lwd = 3)

  
  y <- rnorm(100)^2
  qqnorm(y, ylim = c(-2,2), 
         main = "(2) Square of a gaussian variable")
  qqline(y, 
         col = 'red', lwd = 3)  

  
  y <- -exp(rnorm(100))
  qqnorm(y, ylim = c(-2,2), 
         main = "(3) Opposite of a log-normal variable")
  qqline(y, 
         col = 'red', lwd = 3)  

  
  #A distribution less dispersed that the gaussian distribution (this is called a leptokurtic distribution).  
  y <- runif(100, min=-1, max=1)
  qqnorm(y, ylim = c(-2,2), 
         main = '(4) Uniform distribution')
  qqline(y, 
         col = 'red', lwd = 3) 

  
  #A distribution more dispersed that the gaussian distribution (this is called a platykurtic distribution).  
  y <- rnorm(10000)^3
  qqnorm(y, ylim = c(-2,2), 
         main = "(5) Cube of a gaussian r.v.")
  qqline(y, 
         col = 'red', lwd = 3)

  
  #A distribution with several peaks.  
  y <- c(rnorm(50), 5+rnorm(50))
  qqnorm(y, 
         main = '(6) Two peaks')
  qqline(y, 
         col = 'red', lwd = 3)

  
  y <- c(rnorm(50), 20+rnorm(50))
  qqnorm(y, 
         main = '(7) Two peaks, farther away')
  qqline(y, 
         col = 'red', lwd = 3)  

  
  y <- sample(seq(0,1,.1), 100, replace=T)
  qqnorm(y, 
         main = '(7) Discrete distribution')
  qqline(y, 
         col = 'red', lwd = 3)  

  
 ## You can read those plots as follows.
  
  #a. If the distribution is more concentrated to the left than the gaussian distribution, the left part of the plot is above the line (examples 1, 2 and 4 above).
  
  # b. If the distribution is less concentrated to the left than the gaussian distribution, the left part of the plot is under the line (example 3 above).
  
  #c. If the distribution is more concentrated to the right than the gaussian distribution, the right part of the plot is under the line (examples 3 and 4 above).
  
  #d. If the distribution is less concentrated to the right than the gaussian distribution, the right part of the plot is above the line (examples 1 and 2 above).
  
  #For instance, example 5 can be interpreted as: the distribution is symetric, to the left of 0, near 0, it is more concentrated that a gaussian distribution ; to the left of 0, far from 0, it is less concentrated than a gaussian distribution; on the right of 0, it is the same.  
  
 
  
  x <- seq(from=0, to=2, length=100)
  y <- exp(x)-1
  plot( y ~ x, type = 'l', col = 'red',
        xlim = c(-2,2), ylim = c(-2,2), 
        xlab = "Theoretical (gaussian) quantiles", 
        ylab = "Sample quantiles")        
  lines( x~y, type='l', col='green')
  x <- -x
  y <- -y
  lines( y~x, type='l', col='blue', )
  lines( x~y, type='l', col='cyan')
  abline(0,1)
  legend( -2, 2,
          c( "less concentrated on the right",  
             "more concentrates on the right",
             "less concentrated on the left",
             "more concentrated on the left"
          ),
          lwd=3,
          col=c("red", "green", "blue", "cyan")
  )
  title(main="Reading a qqplot")

  
  
  #e. If the distribution is "off-centered to the left" (think: if the median is less than the mean between the first and third quartiles), then the curve is under the line in the center of the plot (examples 1 and 2 above).
  
  #f. If the distribution is "off-centered to the right" (think: if the median is more than the mean between the first and third quartiles), then the curve is above the line in the center of the plot (example 3 above).
  
  #g. If the distribution is symetric (think: if the median coincides with the average of the first and third quartiles), then the curve cuts the line in the center of the plot (examples 4 and 5 above). 
 
 
   op <- par()
  layout( matrix( c(2,2,1,1), 2, 2, byrow=T ),
          c(1,1), c(1,6),
  )
  # The plot
  n <- 100
  y <- rnorm(n)
  x <- qnorm(ppoints(n))[order(order(y))]
  par(mar=c(5.1,4.1,0,2.1))
  plot( y ~ x, col = "blue", 
        xlab = "Theoretical (gaussian) quantiles", 
        ylab = "Sample quantiles" )
  y1 <- scale( rnorm(n)^2 )
  x <- qnorm(ppoints(n))[order(order(y1))]
  lines(y1~x, type="p", col="red")
  y2 <- scale( -rnorm(n)^2 )
  x <- qnorm(ppoints(n))[order(order(y2))]
  lines(y2~x, type="p", col="green")
  abline(0,1)
  
  # The legend
  par(bty='n', ann=F)
  g <- seq(0,1, length=10)
  e <- g^2
  f <- sqrt(g)
  h <- c( rep(1,length(e)), rep(2,length(f)), rep(3,length(g)) )
  par(mar=c(0,4.1,1,0))
  boxplot( c(e,f,g) ~ h, horizontal=T, 
           border=c("red", "green", "blue"),
           col="white", # Something prettier?
           xaxt='n',
           yaxt='n', 
  )
  title(main="Reading a qqplot")
  par(op)

  
  
  #You can roll up your own qqplot, by going back to the definition.
  y <- rnorm(100)^2
  y <- scale(x)
  y <- sort(x)
  x <- qnorm( seq(0,1,length=length(y)) )
  plot(y~x)
  abline(0,1)

  
  
  two.point.line <- function (x1,y1,x2,y2, ...) {
    a1 <- (y2-y1)/(x2-x1)
    a0 <- y1 - a1 * x1
    abline(a0,a1,...)
  }
  trended.probability.plot <- function (x, q=qnorm) {
    n <- length(x)
    plot( sort(x) ~ q(ppoints(n)), 
          xlab='theoretical quantiles', 
          ylab='sample quantiles')
    two.point.line(q(.25), quantile(x,.25), 
                   q(.75), quantile(x,.75), col='red')
  }
  detrended.probability.plot <- function (x, q=qnorm,
                                          xlab="", ylab="") {
    n <- length(x)
    x <- sort(x)
    x1 <- q(.25)
    y1 <- quantile(x,.25)
    x2 <- q(.75)
    y2 <- quantile(x,.75)
    a1 <- (y2-y1)/(x2-x1)
    a0 <- y1 - a1 * x1
    u <- q(ppoints(n))
    x <- x - (a0 + a1 * u)
    plot(x ~ u, 
         xlab=xlab, ylab=ylab)
    abline(h=0, col='red')
  }
  
  op <- par(mfrow = c(3,2), mar = c(2,2,2,2) + .1)
  x <- runif(20)
  trended.probability.plot(x)
  detrended.probability.plot(x)
  x <- runif(500)
  trended.probability.plot(x)
  detrended.probability.plot(x)
  trended.probability.plot(x, qunif)
  detrended.probability.plot(x,qunif)
  par(op)
  mtext("Detrended quantile-quantile plots", 
        side=3, line=3, font=2, size=1.5)  

  
  
  #Gini concentration####
  #The classical example for the Gini curve is the study of incomes.
  xy <- matrix(c( 0, 0,
                  .2, .9, 
                  .3, .95, 
                  .5, .99,
                  1, 1), byrow = T, nc = 2)
  plot(xy, type = 'b', pch = 15,
       main = "Conventration curve",
       xlab = "patients",
       ylab = "expenses")
  polygon(xy, border=F, col='pink')
  lines(xy, type='b', pch=15)
  abline(0,1,lty=2)

  
  
  x <- c(0,20,2720,3000)/3000
  y <- c(0,100000,100100,100110)/100110
  plot(x,y, type='b', pch=15,
       xlab = "Genes", ylab = "ARNm",
       main = "Conventration curve")
  polygon(x,y, border=F, col='pink')
  lines(x,y, type='b', pch=15)
  abline(0,1,lty=2)  

  
  
  
  n <- 500
  library(ineq)
  Gini(runif(n))
  Gini(runif(n,0,10))
  Gini(runif(n,10,11))
  Gini(rlnorm(n))
  Gini(rlnorm(n,0,2))
  Gini(exp(rcauchy(n,1)))
  Gini(rpois(n,1))
  Gini(rpois(n,10))
  
  

  op <- par(mfrow=c(3,3), mar=c(2,3,3,2)+.1, oma=c(0,0,2,0))
  n <- 500
  set.seed(1)
  plot(Lc(runif(n,0,1)),   
       main="uniform on [0,1]", col='red',
       xlab="", ylab="")
  do.it <- function (x, main="", xlab="", ylab="") { 
    plot(Lc(x), col = "red",
         main=main, xlab=xlab, ylab=ylab)
  }
  do.it(runif(n,0,10),       main="uniform on [0,10]")
  do.it(runif(n,10,11),      main="uniform on [10,11]")
  do.it(rlnorm(n),          main="log-normal")
  do.it(rlnorm(n,0,4),      main="log-normal, wider")
  do.it(abs(rcauchy(n,1)),  main="half-Cauchy")
  do.it(abs(rnorm(n,1)),    main="half-Gaussian")
  do.it(rpois(n,1),         main="Poisson with mean 1")
  do.it(rpois(n,10),        main="Poisson with mean 10")
  par(op)
  mtext("Gini concentration curves", side=3, line=3, 
        font=2, cex=1.5)   


  
  #Column plots  
  data(HairEyeColor)
  x <- apply(HairEyeColor, 2, sum)
  barplot(x)
  title(main="Column plot")

  
  
  barplot(x, col = 1, density = c(3,7,11,20), 
          angle = c(45,-45,45,-45))
  title(main = "Column plot")  

  
  
  #Bar plot
  x <- apply(HairEyeColor, 2, sum)
  barplot(as.matrix(x), legend.text = TRUE)
  title("Bar plot")

  
  barplot(as.matrix(x), 
          horiz = TRUE, 
          col = rainbow(length(x)), 
          legend.text = TRUE)
  title(main = "Bar plot")  

  
  #For a single variable, it might be better to place the legend yourself.  
  op <- par(no.readonly=TRUE)
  par(mar=c(5,4,4,7)+.1)
  barplot(as.matrix(x))
  title("Bar plot, with legend")
  par(xpd=TRUE)  # Do not clip to the drawing area
  lambda <- .025
  legend(par("usr")[2], 
         par("usr")[4],
         names(x),
         fill = grey.colors(length(x)),         
         xjust = 0, yjust = 1
  )
  par(op)
  
  
  op <- par(no.readonly=TRUE)
  par(mar=c(3,1,4,7)+.1)
  barplot(as.matrix(x), 
          horiz = TRUE, 
          col = rainbow(length(x)))
  title(main = "Bar plot, with legend")
  par(xpd=TRUE)  # Do not clip to the drawing area
  lambda <- .05
  legend((1+lambda)*par("usr")[2] - lambda*par("usr")[1], 
         par("usr")[4],
         names(x),
         fill = rainbow(length(x)),         
         xjust = 0, yjust = 1
  )
  par(op)
  
  
  
  
  #Pareto Plot
  data(attenu)
  op <- par(las=2) # Write the labels perpendicularly to the axes
  barplot(table(attenu$event))
  title(main="Column plot")
  par(op)
  
  
  
  op <- par(las=2)
  barplot(rev(sort(table(attenu$event))))
  title(main="Pareto Plot")
  par(op)
  
  
  
  
  # I cannot seem to manage to do it with 
  # the "barplot" function...
  pareto <- function (x, main = "", ylab = "Value") {
    op <- par(mar = c(5, 4, 4, 5) + 0.1,
              las = 2)
    if( ! inherits(x, "table") ) {
      x <- table(x)
    }
    x <- rev(sort(x))
    plot( x, type = 'h', axes = F, lwd = 16,
          xlab = "", ylab = ylab, main = main )
    axis(2)
    points( x, type = 'h', lwd = 12, 
            col = heat.colors(length(x)) )
    y <- cumsum(x)/sum(x)
    par(new = T)
    plot(y, type = "b", lwd = 3, pch = 7, 
         axes = FALSE, 
         xlab='', ylab='', main='')
    points(y, type = 'h')
    axis(4)
    par(las=0)
    mtext("Cumulated frequency", side=4, line=3)
    print(names(x))
    axis(1, at=1:length(x), labels=names(x))
    par(op)
  }
  pareto(attenu$event)    
  title(main="Pareto plot with cumulated frequencies")
  
  
  
  
  #Pie chart
  x <- apply(HairEyeColor, 2, sum)
  pie(x)
  title(main="Pie chart")
  
  
  #it makes sense to plot trees (dendograms) in polar coordinates.
  library(ape)
  example(plot.ancestral)
  example(plot.phylo)

  
  
  #Beyond mosaic plots: Treemaps, Region Trees and TempleMVV plots
  #Treemaps are 2-dimensional barplots used to represent hiearchical classifications.  
  library(portfolio)
  example(map.market)
  
  
  
  # A Region Tree is a set of barplots that progressively drill-down into the data. 
  olap <- function (x, i) {
    # Project (drill-up?) a data cube
    y <- x <- apply(x, i, sum)
    if (length(i) > 1) {
      y <- as.vector(x)
      n <- dimnames(x)
      m <- n[[1]]
      for (i in (1:length(dim(x)))[-1]) {
        m <- outer(m, n[[i]], paste)
      }
      names(y) <- m
    }
    y
  }
  col1 <- c("red", "green", "blue", "brown")
  col2 <- c("red", "light coral",
            "green", "light green",
            "blue", "light blue",
            "brown", "rosy brown")
  col3 <- col2[c(1,2,1,2,3,4,3,4,5,6,5,6,7,8,7,8)]
  op <- par(mfrow=c(3,1), mar=c(8,4,0,2), oma=c(0,0,2,0), las=2)
  barplot(olap(Titanic,1),   space=0, col=col1)
  barplot(olap(Titanic,2:1), space=0, col=col2)
  barplot(olap(Titanic,3:1), space=0, col=col3)
  par(op)
  mtext("Region tree", font = 2, line = 3)


  

  
  #Point cloud
  data(cars)
  plot(cars$dist ~ cars$speed, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)", 
       las = 1)
  title(main = "Point cloud")
  
  
  plot(cars$dist ~ cars$speed, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)", 
       las = 1)
  title(main = "cars data")
  rug(side=1, jitter(cars$speed, 5))
  rug(side=2, jitter(cars$dist, 20))
  
  
  
  op <- par()
  layout( matrix( c(2,1,0,3), 2, 2, byrow=T ),
          c(1,6), c(4,1),
  )
  
  par(mar=c(1,1,5,2))
  plot(cars$dist ~ cars$speed, 
       xlab='', ylab='',
       las = 1)
  rug(side=1, jitter(cars$speed, 5) )
  rug(side=2, jitter(cars$dist, 20) )
  title(main = "cars data")
  
  par(mar=c(1,2,5,1))
  boxplot(cars$dist, axes=F)
  title(ylab='Stopping distance (ft)', line=0)
  
  par(mar=c(5,1,1,2))
  boxplot(cars$speed, horizontal=T, axes=F)
  title(xlab='Speed (mph)', line=1)
  
  par(op)
  
  
  
  plot(dist ~ speed, data = cars,
       main = "\"cars\" data and regression line")
  abline(lm( dist ~ speed, data = cars), 
         col = 'red')
  
  
  
  #The "loess" function approximates the data with a curve, not necessarily a line. We shall explain what is behind this (it is called "local regression") when we tackle regression.
  plot(cars, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)",
       las = 1)
  # lines(loess(dist ~ speed, data=cars), 
  #       col = "red") # Didn't that use to work?
  r <- loess(dist ~ speed, data=cars)
  lines(r$x, r$fitted, col="red")
  title(main = "\"cars\" data and loess curve")
  
  
  plot(cars, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)",
       las = 1)
  lines(lowess(cars$speed, cars$dist, 
               f = 2/3, iter = 3), 
        col = "red")
  title(main = "\"cars\" data and lowess curve")
  
  
  
  library(plotrix)
  clock24.plot(x,
               line.col = "blue", 
               lwd = 10)
  
  
  # See also polar.plot, radial.plot
  library(circular)
  rose.diag(x)
  
  
  # x <- as.circular(rep( 2*pi / 24 * (0:23), x ))
  detach("package:circular")   # redefines "var"...

  
  
  #Lattice
  library(lattice)
  y <- cars$dist
  x <- cars$speed
  
  
  # vitesse <- shingle(x, co.intervals(x, number=6))
  vitesse <- equal.count(x)
  histogram(~ y | vitesse)

  
  bwplot(~ y | vitesse, layout=c(1,6))  

  
  densityplot(~ y | vitesse, aspect='xy')  

  
  densityplot(~ y | vitesse, layout=c(1,6))  
 
  
  #Convex hull####
  #To have an idea of the shape of the cloud of points, some people suggest to have a look at the convex hull of the cloud.
  plot(cars)
  polygon( cars[chull(cars),], col="pink", lwd=3)
  points(cars)

  
  #Ellipse####
  draw.ellipse <- function (
    x, y = NULL, 
    N = 100,
    method = lines, 
    ...
  ) {
    if (is.null(y)) {
      y <- x[,2]
      x <- x[,1]
    }
    centre <- c(mean(x), mean(y))
    m <- matrix(c(var(x),cov(x,y),
                  cov(x,y),var(y)),
                nr=2,nc=2)
    e <- eigen(m)
    r <- sqrt(e$values)
    v <- e$vectors
    theta <- seq(0,2*pi, length=N)
    x <- centre[1] + r[1]*v[1,1]*cos(theta) +
      r[2]*v[1,2]*sin(theta)
    y <- centre[2] + r[1]*v[2,1]*cos(theta) +
      r[2]*v[2,2]*sin(theta)
    method(x,y,...)
  }
  plot(cars)
  draw.ellipse(cars, col="blue", lwd=3)

  
  
 
   #2-dimensional density estimation
  library(hexbin)
  plot(hexbin(x,y))
  
  library(MASS)
  z <- kde2d(x,y, n=50)
  image(z, main = "Density estimation")

  
  ##Counter Plot
 contour(z, 
          col = "red", lwd = 3, drawlabels = FALSE, 
          add = TRUE,
          main = "Density estimation: contour plot")  
 
 
 
 persp(z, main = "Density estimation: perspective plot")

 
 
 persp(z, 
       phi = 45, theta = 30, 
       xlab = "age", ylab = "income", zlab = "density",
       main = "Density estimation: perspective plot")  

 
 
 op <- par(mar=c(0,0,2,0)+.1)
 persp(z, phi = 45, theta = 30, 
       xlab = "age", ylab = "income", zlab = "density", 
       col = "yellow", shade = .5, border = NA,
       main = "Density estimation: perspective plot")
 par(op)

 
 

 r <- loess(y~x)
 o <- order(x)
 plot( jitter(x, amount = .5), y, pch = ".",
       xlab = "age", ylab = "income",
       main = "Loess curve")
 lines(r$x[o], r$fitted[o], col="blue", lwd=3)
 r <- kde2d(x,y)
 contour(r, drawlabels=F, col="red", lwd=3, add=T)
 
 
 
 ##Box-and-whiskers plots (boxplots)
 data(InsectSprays)
 boxplot(count ~ spray, 
         data = InsectSprays,
         xlab = "Type of spray", 
         ylab = "Insect count",
         main = "InsectSprays data", 
         varwidth = TRUE, 
         col = "lightgray")

 
 
 #Violin plot
 #(This package used to be called UsingR)
 library(UsingR)
 n <- 1000
 k <- 10
 x <- factor(sample(1:5, n, replace=T))
 m <- rnorm(k,sd=2)
 s <- sample( c(rep(1,k-1),2) )
 y <- rnorm(n, m[x], s[x])
 simple.violinplot(y~x, col='pink')

 
 
 
 library(lattice)
 bwplot( y ~ x, 
         panel = panel.violin,
         main = "panel.violin" )

 
 #cumulative distribution function
 op <- par(mfrow=c(3,1), mar=c(2,2,3,2))
  n <- length(x)
 plot(sort(x), (1:n)/n, 
      type = "l",
      main = "Cumulative distribution function") 
 
 
 
 bwplot( ~ y | x, 
         panel = panel.bpplot, 
         main = "panel.bpplot",
         layout = c(1,5) )

 
 
 #Highest Density Region (HDR)
 #Yet another modification of the box-and-whiskers plot
 library(hdrcde)
 hdr.boxplot(rnorm(1000), col = "pink",
             main = "Highest Density Region Plot")

 
 hdr.boxplot(faithful$waiting, 
             col = "pink",
             main = "Highest Density Region Plot") 

 
 
 #Parallel scatterplot
 stripchart(InsectSprays$count ~ InsectSprays$spray, 
            method = 'jitter')

 
 
 #Colored scatterplot
 #In higher dimensions, one often adds colors in scatterplots.
 data(iris)
 plot(iris[1:4], 
      pch = 21, 
      bg = c("red", "green", "blue")[
        as.numeric(iris$Species)
      ])
 
 
 a <- as.vector(t(iris[1]))
 b <- rnorm(length(a))
 plot(b ~ a, 
      pch = 21, 
      bg = c("red", "green", "blue")[
        as.numeric(iris$Species)
      ],
      main = "1-dimensional scatter plot",
      xlab = "Number of insects",
      ylab = "")
 
 
 
# Histograms
#We can also plot several histograms, one for each group.
 hists <- function (x, y, ...) {
   y <- factor(y)
   n <- length(levels(y))
   op <- par( mfcol=c(n,1), mar=c(2,4,1,1) )    
   b <- hist(x, ..., plot=F)$breaks
   for (l in levels(y)){
     hist(x[y==l], breaks=b, probability=T, ylim=c(0,.3), 
          main="", ylab=l, col='lightblue', xlab="", ...)
     points(density(x[y==l]), type='l', lwd=3, col='red')
   }
   par(op)
 }
 hists(InsectSprays$count, InsectSprays$spray) 

 
 
 #Lattice (treillis) plots
 #We can do the same thing with lattice plots. 
 library(lattice)
 histogram( ~ count | spray, data=InsectSprays)

 
 
 densityplot( ~ count | spray, data = InsectSprays ) 

 
 
 bwplot( ~ count | spray, data = InsectSprays ) 

 
 
 
 bwplot( ~ count | spray, data = InsectSprays, layout=c(1,6) ) 



  
  
  #Line chart####
  #Here, we plot several variables Y1, Y2, etc. as a function of X. We can overlay the curves with the "matplot" function.
  n <- 10
  d <- data.frame(y1 = abs(rnorm(n)),
                  y2 = abs(rnorm(n)),
                  y3 = abs(rnorm(n)),
                  y4 = abs(rnorm(n)),
                  y5 = abs(rnorm(n))
  )
  matplot(d, 
          type = 'l',
          ylab = "", 
          main = "Matplot")

  
  
  palette(rainbow(12, s = 0.6, v = 0.75))
  stars(mtcars[, 1:7], 
        len = 0.8, 
        key.loc = c(12, 1.5),
        main = "Motor Trend Cars", 
        draw.segments = TRUE)  


  
  
  #stereoscopic plot####
  data(iris)
  print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
              data = iris, cex = .8,
              groups = Species,
              subpanel = panel.superpose,
              main = "Stereo",
              screen = list(z = 20, x = -70, y = 3)),
        split = c(1,1,2,1), more = TRUE)
  print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
              data = iris, cex = .8,
              groups = Species,
              subpanel = panel.superpose,
              main = "Stereo",
              screen = list(z = 20, x = -70, y = 0)),
        split = c(2,1,2,1))

  
  
  
  histogram( ~ Sepal.Length | Species, data = iris, 
             layout = c(1,3) )

  
  
  xyplot( Sepal.Length ~ Sepal.Width | Species, data = iris,
          layout = c(1,3) )  

  
  
  data(iris)
  plot(iris[1:4], pch=21, 
       bg=c("red", "green", "blue")[as.numeric(iris$Species)]) 

  
  
  a <- rnorm(10)
  b <- 1+ rnorm(10)
  c <- 1+ rnorm(10)
  d <- rnorm(10)
  x <- c(a,b,c,d)
  y <- factor(c( rep("A",20), rep("B",20)))
  z <- factor(c( rep("U",10), rep("V",20), rep("U",10) ))
  op <- par(mfrow=c(2,2))
  plot(x~y)
  plot(x~z)
  plot(x[z=="U"] ~ y[z=="U"], border="red", ylim=c(min(x),max(x)))
  plot(x[z=="V"] ~ y[z=="V"], border="blue", add=T)
  plot(x[y=="A"] ~ z[y=="A"], border="red", ylim=c(min(x),max(x)))
  plot(x[y=="B"] ~ z[y=="B"], border="blue", add=T)
  par(op)  
  
  
  
  
  l <- rep("",length(x))
  for (i in 1:length(x)){
    l[i] <- paste(y[i],z[i])
  }
  l <- factor(l)
  boxplot(x~l)
  
  
  
  
  #Decorations####
  #We can play and add 3d effects -- it distracts the reader, but if you do not want to convey information but merely deceive, this is for you...
  
  #A pie chart with a shadow.
  shaded.pie <- function (...) {
    pie(...)
    op <- par(new=T)
    a <- seq(0,2*pi,length=100)
    for (i in (256:64)/256) {
      r <- .8-.1*(1-i)
      polygon( .1+r*cos(a), -.2+r*sin(a), border=NA, col=rgb(i,i,i))
    }
    par(new=T)
    pie(...)
    par(op)
  }
  x <- rpois(10,5)
  x <- x[x>0]
  shaded.pie(x)
  
  
  
  #xyplot####
  #The following example (three quantitative variables) displays earthquake epicenters.
  data(quakes)
  library(lattice)
  Depth <- equal.count(quakes$depth, number=8, overlap=.1)
  xyplot(lat ~ long | Depth, data = quakes)
  
  
  
  #3d scatterplot####
  library(scatterplot3d)
  scatterplot3d(quakes[,1:3],
                highlight.3d = TRUE,
                pch = 20)

  
  
  #barchart####
 #An example with a quantitative variable and 3 qualitative variables in which, if we fix the qualitative variables, we have a single observation (this is sometimes called a "factorial design").  
  data(barley)
  barchart(yield ~ variety | year * site, data=barley)
  #The "scales" argument allows you to change the axes and their ticks (here, we avoid label overlap).
  barchart(yield ~ variety | year * site, data = barley,
           ylab = "Barley Yield (bushels/acre)",
           scales = list(x = list(0, abbreviate = TRUE,
                                  minlength = 5)))

  
  dotplot(yield ~ variety | year * site, data = barley)  
  
  dotplot(variety ~ yield | year * site, data = barley)    

  


  
  y <- cars$dist
  x <- cars$speed
  vitesse <- shingle(x, co.intervals(x, number=6))
  histogram(~ x | vitesse, type = "density",
            panel = function(x, ...) {
              ps <- trellis.par.get('plot.symbol')
              nps <- ps
              nps$cex <- 1
              trellis.par.set('plot.symbol', nps)
              panel.histogram(x, ...)
              panel.densityplot(x, col = 'brown', lwd=3) 
              panel.xyplot(x = jitter(x), y = rep(0, length(x)), col='brown' )
              panel.mathdensity(dmath = dnorm,
                                args = list(mean=mean(x),sd=sd(x)),
                                lwd=3, lty=2, col='white')
              trellis.par.set('plot.symbol', ps)
            })
 
 
 
 
 
 
 
 
 
 
 
 
  