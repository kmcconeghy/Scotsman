#' @title boot_ATE: Bootstrapped marginal effects
#'
#' @description Estimate Average Treatment Effect with Bootstrapped CI's
#'
#' @details
#' boot_ATE will take a glm regression object and compute predicted outcomes with
#' treatment = 1 and treatment 0; It will then bootstrap the model and compute effects.
#' There is an option for block bootstrapping to compute cluster robust intervals.
#' Bootstrapped confidence intervals are percentile.
#'
#' @param model GLM regression object
#' @param treat Character string for variable to calculate ATE with
#' @param R Integer for number of bootstrap replications
#' @param block Character string for "block" to sample by (cluster robust SE's).
#' @param df Dataframe object of original sample
#'
#' @return coefficient from glm object
#' @return coefficient from bootstraps
#' @return ATE (p1- p0)
#' @return RR (p1/p0)
#' @return matrix of bootstrap replications
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#' require(geepack)
#' require(boot)
#' data(ohio)
#' model <- glm(resp ~ age + smoke, data=ohio, family=binomial())
#' b1 <- boot_ATE(model, treat="smoke", R=10000, df=ohio, block="id", type="perc")
#' b1$ate
#'
#' b1$coeff #Bootstrapped
#' confint.geeglm <- function(object, parm, level = 0.95, ...) {
#'  cc <- coef(summary(object))
#'  mult <- qnorm((1+level)/2)
#'  citab <- with(as.data.frame(cc),
#'                cbind(lwr=Estimate-mult*Std.err,
#'                      upr=Estimate+mult*Std.err))
#'  rownames(citab) <- rownames(cc)
#'  citab[parm,]
#'  }
#' confint.geeglm(g1, "smoke") #Comparison to GEE
#' @export
#'
boot_ATE <- function(model, treat, R=250, block="", df) {

  require(boot)
  require(dplyr)

  family <- model$family
  #Re-sampling function
  if (block=="") {
    boot.mod <- function(x,i, model,treat) {

      samp.df <- x[i,] #Data.Frame

      samp.glm <- try(glm(model, data=samp.df, family=family))

      if(inherits(samp.glm, "try-error"))
      {
        #error handling code, maybe just skip this iteration using
        coef <- NA
        ate <- NA
        rr <- NA
        c(coef, ate, rr)
      } else {
      #Predict, treat=1
      df2 <- samp.df
      df2[,paste(treat)] =1
      pred1. <- predict.glm(samp.glm, newdata=df2, type="response")

      #Predict, treat=0
      df2[,paste(treat)] =0
      pred0. <- predict.glm(samp.glm, newdata=df2, type="response")

      coef <- samp.glm$coefficients[paste0(treat)]
      ate <- mean(pred1.) - mean(pred0.)
      rr <- mean(pred1.) / mean(pred0.)
      c(coef, ate, rr)
      }
    }

    #Run BootStrap
    boot.m <- boot(data=df, statistic=boot.mod, R=R, model=model,treat=treat)
  } else {
    Groups = unique(df[,paste(block)])
    boot.mod <- function(x, i, model, treat, df, block, iter=0) {

      block.df <- data.frame(group = x[i])
      names(block.df) = block

      #Sample Blocks
      samp.df <- left_join(block.df, df, by=block)
      samp.glm <- try(glm(model, data=samp.df, family=family))

      if(inherits(samp.glm, "try-error"))
      {
        #error handling code, maybe just skip this iteration using
        coef <- NA
        ate <- NA
        rr <- NA
        c(coef, ate, rr)
      } else {

      #Predict, treat=1
        df2 <- samp.df
        df2[,paste(treat)] =1
        pred1. <- predict.glm(samp.glm, newdata=df2, type="response")

      #Predict, treat=0
        df2[,paste(treat)] =0
        pred0. <- predict.glm(samp.glm, newdata=df2, type="response")

      coef <- samp.glm$coefficients[paste0(treat)]
      ate <- mean(pred1.) - mean(pred0.)
      rr <- mean(pred1.) / mean(pred0.)
      c(coef, ate, rr)
      }
    }
    #Run Block BootStrap
    boot.m <- boot(data=Groups, statistic=boot.mod, R=R, model=model, treat=treat,
                   df=df, block=block)
  }

  m1.confint <- c(model$coefficients[paste0(treat)], confint(model, treat, level=0.95))
  coeff = boot.ci(boot.m, index=1, type="perc")
    coeff = c(median(boot.m$t[,1]),coeff$percent[,4], coeff$percent[,5])
    names(coeff) <- c('Coeff.','2.5%','97.5%')
  ate = boot.ci(boot.m, index=2, type="perc")
    ate   = c(median(boot.m$t[,2]), ate$percent[,4],   ate$percent[,5])
    names(ate) <- c('ATE','2.5%','97.5%')

  rr = boot.ci(boot.m, index=3, type="perc")
    rr    = c(median(boot.m$t[,3]), rr$percent[,4],    rr$percent[,5])
    names(rr) <- c('Rr','2.5%','97.5%')

  boot.iter = boot.m$t

  res = list(level=0.95, model_ci=m1.confint, coeff=coeff, ate=ate, rr=rr, boots=boot.iter)
  return(res)
}
