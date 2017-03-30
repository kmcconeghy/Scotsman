#' @title VarByGr
#'
#' @description Function which summarizes numeric/int by group
#'
#' @details
#' First object column 1 (group[1]), second obj column 2 (group[2])
#' to be used in generating a "table one"
#'
#' @param df Dataframe
#' @param var Variable to be summarized
#' @param group Group to summarize over
#' @param VarTypes list with three elements, normvars nonnormvars binvars
#'
#' @return List of length = groups
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#'
#' @export

VarByGr <- function(df, var, group, VarTypes) {
  #Test
  if (0==1) {
    df = mtcars
    var = "mpg"
    group = "am"
    type="norm"
  }

  df <- df %>% select(matches(var), matches(group)) %>%
    filter(is.na(var)==F & is.na(group)==F)

  #Evaluate Variable Type
    if (var %in% VarTypes$normvars) type="norm"
    if (var %in% VarTypes$nonnormvars) type="iqr"
    if (var %in% VarTypes$binvars) type="binary"

  #Calculate Mean/SD
  if (type=="norm") {
    l1 <- as.formula(paste0("~mean(",var,",na.rm=TRUE)"))
    l2 <- as.formula(paste0("~sd(",var,",na.rm=TRUE)"))

    dots <- list(l1, l2)
    summ <- df %>% group_by_(group) %>%
      summarise_(.dots=dots)

    m1 <- sprintf("%02.1f",summ[1,2])
    sd1 <- sprintf("%02.1f",summ[1,3])
    m2 <- sprintf("%02.1f",summ[2,2])
    sd2 <- sprintf("%02.1f",summ[2,3])

    c1 <- paste0(m1," (",sd1,")")
    c2 <- paste0(m2," (",sd2,")")
  }

  #Calculate med, IQR
  if (type=="iqr") {

    l1 <- as.formula(paste0("~quantile(",var,",probs=0.25,na.rm=TRUE)"))
    l2 <- as.formula(paste0("~quantile(",var,",probs=0.50,na.rm=TRUE)"))
    l3 <- as.formula(paste0("~quantile(",var,",probs=0.75,na.rm=TRUE)"))
    dots <- list(l1, l2,l3)

    summ <- df %>% group_by_(group) %>%
      summarise_(.dots=dots)

    c1 <- paste0(sprintf("%02.1f",summ[1,3]),
                 " (",sprintf("%02.1f",summ[1,2]),", ",sprintf("%02.1f",summ[1,4]),")")
    c2 <- paste0(sprintf("%02.1f",summ[2,3]),
                 " (",sprintf("%02.1f",summ[2,2]),", ",sprintf("%02.1f",summ[2,4]),")")
  }

  #Proportions
  if (type=="binary") {
    l1 <- as.formula(paste0("~sum(",var,",na.rm=TRUE)"))
    l2 <- as.formula(paste0("~mean(",var,",na.rm=TRUE)*100"))

    dots <- list(l1, l2)
    summ <- df %>% group_by_(group) %>%
      summarise_(.dots=dots)

    n1 <- format(summ[1,2],big.mark=",")
    perc1 <- sprintf("%.1f",summ[1,3])
    n2 <- format(summ[2,2],big.mark=",")
    perc2 <- sprintf("%.1f",summ[2,3])

    c1 <- paste0(n1," (",perc1,")")
    c2 <- paste0(n2," (",perc2,")")
  }
  return(list(c2,c1))
}
