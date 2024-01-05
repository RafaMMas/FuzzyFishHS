#' Function to create a zero-order Takagi–Sugeno–Kang fuzzy rule-based systems (FRBS)
#'
#' @param ImpVariables Rhe names of the input variables in vector format
#' @param Range a matrix with as many columns as input variables with the minimum and maximum expected values for each variable
#' @param MFfunction the selected membership function
#' @param MFparameters the parameters of the membership functions. It should be a matrix with as many columns as fuzzy sets. The columns for each variable must include the variable name as indicated in ImpVariables.
#' @param Consequents the fuzzy rules consequents.
#'
#' @return an R list containing the five components of the FRBSs
#' @export
#'
#' @examples
Create.FRBS.R <- function(ImpVariables, Range, MFfunction, MFparameters, Consequents){

  list(ImpVariables, Range, MFfunction, MFparameters, Consequents)

  }

#' Lepomis gibbosus dataset
#'
"Lepomis.gibbosus.dataset"

