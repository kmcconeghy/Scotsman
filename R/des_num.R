#' @title des_num
#'
#' @description Exploratory data analysis of a numeric variable
#'
#' @details
#' This function will take a dataframe, provide summary statistics, distributions checks, graphs
#'
#' @param numvar dataframe object
#'
#' @return Object with generical information, statistics, ggplot2 plots
#'
#' @author Kevin W. McConeghy
#'
#' @note if no filepath specified, will default to working directory with timestamp.
#'
#' @examples
#'
#' @export
#'

des_num <- function(numvar) {

  numvar <- cars$speed

  n <- length(numvar)
  nmiss <- sum(is.na(numvar)==T)
  var_nonmiss <- numvar[!is.na(numvar)]

  #Attach dataset
  load.packages(c("moments","ggplot2","htmlTable"))

  #All stat calculations
    unique <- length(unique(var_nonmiss))
    mean <- round(mean(var_nonmiss),1)
    ptile5 <- quantile(var_nonmiss,0.05)
    ptile10 <- quantile(var_nonmiss,0.10)
    ptile25 <- quantile(var_nonmiss,0.25)
    median <- quantile(var_nonmiss,0.50)
    ptile75 <- quantile(var_nonmiss,0.75)
    ptile90 <- quantile(var_nonmiss,0.90)
    ptile95 <- quantile(var_nonmiss,0.95)
    min <- min(var_nonmiss)
    max <- max(var_nonmiss)
    sd <- round(sd(var_nonmiss),1)
    variance <- var(var_nonmiss)
    IQR <- IQR(var_nonmiss)
    skew <- round(skewness(var_nonmiss),1)
    kurtosis <- round(kurtosis(var_nonmiss),1)
    first5 <- paste(head(numvar[1:5],n=5),collapse=",")
    last5 <- paste(tail(numvar[1:5],n=5),collapse=",")

  ##Summary statistics
    var.summary<-round(cbind(n,nmiss,unique,mean,sd,variance,IQR,skew,kurtosis),2)
    var.tiles <- round(rbind(min,ptile5,ptile10,ptile25,median, ptile75,ptile90,ptile95,max),2)
    names(var.tiles)[1] <- paste("Statistic")

  #Make HTML Table
    mx <- matrix(ncol=9,nrow=1)
    colnames(mx) <-c("&nbsp;&nbsp;N&nbsp;&nbsp;",
                     "&nbsp;&nbsp;Nmiss&nbsp;&nbsp;",
                     "&nbsp;&nbsp;Min&nbsp;&nbsp;",
                     "&nbsp;&nbsp;10%&nbsp;&nbsp;",
                     "&nbsp;&nbsp;25%&nbsp;&nbsp;",
                     "&nbsp;&nbsp;50%&nbsp;&nbsp;",
                     "&nbsp;&nbsp;75%&nbsp;&nbsp;",
                     "&nbsp;&nbsp;90%&nbsp;&nbsp;",
                     "&nbsp;&nbsp;Max&nbsp;&nbsp;")
    r1 <- c(n, nmiss, min, ptile10, ptile25, median, ptile75, ptile90,max)
    mx[1,1:9] <- r1

    mx2 <- matrix(ncol=4,nrow=1)
    colnames(mx2) <- c("&nbsp;&nbsp;Mean&nbsp;&nbsp;",
                       "&nbsp;&nbsp;St. Dev.&nbsp;&nbsp;",
                       "&nbsp;&nbsp;Skewness&nbsp;&nbsp;",
                       "&nbsp;&nbsp;Kurtosis&nbsp;&nbsp;")
    mx2[1,1:4] <- r1 <-c(mean, sd, skew, kurtosis)

    #note1min5 <-
    #note2max5 <-
    outtable1 <- htmlTable(mx, caption="Table 1. Variable distribution",
                           css.cell = "padding-left: .5em; padding-right: .2em;"
                          )
    outtable2 <- htmlTable(mx2, caption="Table 2. Summary Statistics",
                           css.cell = "padding-left: .5em; padding-right: .2em;")

  #PLOTS
    gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                      labels = names(x)){
      q.function <- eval(parse(text = paste0("q", distribution)))
      d.function <- eval(parse(text = paste0("d", distribution)))
      x <- na.omit(x)
      ord <- order(x)
      n <- length(x)
      P <- ppoints(length(x))
      df <- data.frame(ord.x = x[ord], z = q.function(P, ...))

      if(is.null(line.estimate)){
        Q.x <- quantile(df$ord.x, c(0.25, 0.75))
        Q.z <- q.function(c(0.25, 0.75), ...)
        b <- diff(Q.x)/diff(Q.z)
        coef <- c(Q.x[1] - b * Q.z[1], b)
      } else {
        coef <- coef(line.estimate(ord.x ~ z))
      }

      zz <- qnorm(1 - (1 - conf)/2)
      SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
      fit.value <- coef[1] + coef[2] * df$z
      df$upper <- fit.value + zz * SE
      df$lower <- fit.value - zz * SE

      if(!is.null(labels)){
        df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
      }

      p <- ggplot(df, aes(x=z, y=ord.x)) +
        geom_point() +
        geom_abline(intercept = coef[1], slope = coef[2]) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2)
      if(!is.null(labels)) p <- p + geom_text( aes(label = label))
      return(p)
    }
  var.normqqplot <- gg_qq(var_nonmiss)

  var.hist <- ggplot(data.frame(var_nonmiss), aes(x=var_nonmiss)) +
    geom_histogram(aes(y=..density..), colour="black", fill="white") + xlab("") +
    geom_density(alpha=.1, fill="#FF6666")

  #Place in single list object
    x <- list(tables=list(outtable1,outtable2), var.summary, var.tiles, plots=list(var.hist, var.normqqplot))

  #Return object
    return(x)
}
