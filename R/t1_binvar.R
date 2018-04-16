#' @title t1_binvar: "Table One", Binary Variable Summary 
#'
#' @description Function which summarizes dichotomous or binary
#'  variable by a named group
#'
#' @details
#' Supply a dataframe, a variable to summarize, a group variable
#'
#' @param df Dataframe
#' @param .var Variable to be summarized
#' @param .group Group to summarize over
#' @param .referent If variable logical or numeric, default reported is 1/"T", 
#' otherwise specify here.  
#' @param quietly If FALSE, function will report some internal steps
#' 
#' @return List of length = groups
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#'
#' @export
t1_binvar <- function(data, .var, .group, .referent = "1", quietly=T) {
  
  ## Tidy Evaluation
    eq_var <- rlang::enquo(.var) 
    nm_var <- paste0(rlang::quo_text(eq_var))
  
    eq_group <- rlang::enquo(.group)
    nm_group <- paste0(rlang::quo_text(eq_group))
  
  ## Sanity Checks
    stopifnot(nm_var %in% names(data))  
    stopifnot(nm_group %in% names(data))  

  ## 2 or more
    glevels <- dplyr::n_distinct(data[[nm_group]])
    if (glevels>3) {warning("4 or more levels to group variable")}
    
  ##Compute values 
  summ <- data %>% 
    group_by(UQ(eq_group)) %>%  
    summarise(n = sum(rlang::UQ(eq_var)==.referent, na.rm=T),
              prop = sum(rlang::UQ(eq_var)==.referent, na.rm=T) / n()) %>%
    arrange(-UQ(eq_group))
  
  ## Compute Scalars
  for (i in 1:glevels) {
    n_nm <- format(summ[[i, "n"]], big.mark=",")
    perc_nm <- scales::percent(summ[[i, "prop"]])
    assign(paste0("col_", i), paste0(n_nm, " (", perc_nm, ")"))
  }
  
  #Build into Row
  result <- t(mget(ls(pattern="col_")))
  
  #Report result
  if (quietly==F) {
    cat("Variable: ", nm_var, " ", 
        "Grouping: ", nm_group, "\n")
    print(paste0(result, sep=", "))
  }
  
  #Return Row
  return(result)
}