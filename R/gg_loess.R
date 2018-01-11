#' @title gg_loess: Loess plot for propensity scores
#' 
#' @description Loess plot with density distributions for propensity scores and outcomes on
#' top and right, respectively.
#'
#' @param x vector of propensity scores.
#' @param response the response variable.
#' @param treatment the treatment varaible as a logical type.
#' @param percentPoints.treat the percentage of treatment points to randomly plot.
#' @param percentPoints.control the percentage of control points to randomly plot.
#' @param points.treat.alpha the transparency level for treatment points.
#' @param points.control.alpha the transparency level for control points.
#' @param responseTitle the label to use for the y-axis (i.e. the name of the response variable)
#' @param treatmentTitle the label to use for the treatment legend.
#' @param ... other parameters passed to \code{\link{geom_smooth}} and
#'        \code{\link{stat_smooth}}.
#' @return a ggplot2 figure
#' @export
#' @examples
#' require(MatchIt)
#' lalonde <- lalonde
#' m.out <- matchit(treat ~ age + educ + black + hispan, data=lalonde, method="nearest")
#' df_m <- match.data(m.out)
#' gg_loess(df_m$distance, df_m$age, df_m$treat)
#'
#'

gg_loess <- function(x, response, treatment,
                           					   responseTitle='',
                           					   treatmentTitle='Treatment',
                           					   percentPoints.treat=0.2,
                           					   percentPoints.control=0.1,
                           					   points.treat.alpha=.2,
                           					   points.control.alpha=.1,
                           					   ...) {
require(ggplot2, quietly=T)
require(grid, quietly=T)
require(gridExtra, quietly=T)

    #Set up dataframe
    df = data.frame(ps=x, response=response, treatment=as.logical(treatment))
   	df.points.treat <- df[df$treatment==T,]
   	df.points.control <-  df[df$treatment==F,]
   	df.points.treat <- df.points.treat[sample(nrow(df.points.treat),nrow(df.points.treat) * percentPoints.treat),]
   	df.points.control <- df.points.control[sample(nrow(df.points.control),nrow(df.points.control) * percentPoints.control),]

   	#Generate skeleton plot
   	pmain = ggplot(df, aes(x=ps, y=response, colour=treatment)) + theme_bw() +
   	  scale_fill_brewer(palette="Set1")

   	#Add Control scatterpoints
   	if(nrow(df.points.control) > 0) {
      pmain = pmain + geom_point(data=df.points.control,aes(x=ps, y=response, colour=treatment), alpha=points.control.alpha)
   	 }

   	#Add treatment scatterpoints
    if(nrow(df.points.treat) > 0) {
     pmain = pmain + geom_point(data=df.points.treat, aes(x=ps, y=response, colour=treatment), alpha=points.treat.alpha)
    }

   	#Add Loess
    pmain = pmain + geom_smooth(method='loess') + ylab(responseTitle) + xlab("Propensity Score") +
     				theme(legend.position='none', legend.justification='left') +
            scale_colour_hue(treatmentTitle) +  xlim(range(df$ps)) + ylim(range(df$response))


   	#Top Density Curve (Propensity Score OverLap)
    ptop = ggplot(df, aes(x=ps, colour=treatment, group=treatment)) +
     				geom_density() +
     				theme(legend.position='none', axis.text.y=element_blank()) +
     				xlab(NULL) + ylab('Density') +
     				xlim(range(df$ps))

    #Right Density Curve (Response Variable OverLap)
   	pright = ggplot(df, aes(x=response, colour=treatment)) +
    				geom_density() + coord_flip() +
     				theme(legend.position='none',axis.text.x=element_blank()) +
     				xlab(NULL) + ylab('Density') + xlim(range(df$response))

   	#Make temporary dataframe by ps density
   	tmp = rbind(
     		data.frame(treatment=TRUE, y=median(density(df[df$treatment,]$ps)$y), x=median(density(df[df$treatment,]$response)$y)),
     		data.frame(treatment=FALSE, y=median(density(df[!df$treatment,]$ps)$y), x=median(density(df[!df$treatment,]$response)$y))
     	)

   	#Add Legend
   	legend <- ggplot(tmp, aes(x=x,y=y,colour=treatment)) + geom_point() +

   	    xlim(range(density(df$response)$y)) +
     		ylim(range(density(df$ps)$y)) +

   	    geom_rect(xmin=min(density(df$response)$y), xmax=max(density(df$response)$y),
                    				  ymin=min(density(df$ps)$y), ymax=max(density(df$ps)$y),
                    				  colour='white', fill='white') +

   	    theme(legend.position=c(.5,.5),
                			  axis.text.x=element_blank(), axis.text.y=element_blank(),
                			  axis.title.x=element_blank(), axis.title.y=element_blank(),
                			  axis.ticks=element_blank(),
                			  panel.background=element_blank(),
                			  panel.grid.major=element_blank(),
                			  panel.grid.minor=element_blank(),
                			  panel.spacing = element_blank()) +

   	    scale_colour_hue(treatmentTitle)

   	  hlay <- rbind(c(1,1,1,4),
   	                c(2,2,2,3),
   	                c(2,2,2,3),
   	                c(2,2,2,3))
      if (percentPoints.treat<1) {
        scatternote = paste0("Points are random sample of ",
                            percentPoints.treat*100, "% treated and ",
                            percentPoints.control*100,"% controls")
      } else {
        scatternote = ''
      }

   	  grid.arrange(ptop, pmain, pright, legend, layout_matrix=hlay,
   	               top = responseTitle,
   	               bottom=textGrob(scatternote, x=0, hjust=-0.1, vjust=0.1,
   	                               gp = gpar(fontface = "italic", fontsize=10)))

   }
