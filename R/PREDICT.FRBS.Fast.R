#' Function to carry out predictions with zero-order Takagi–Sugeno–Kang fuzzy rule-based systems for fish habitat evaluation.
#'
#' @param Data the evaluated dataset in data.frame format
#' @param FRBS the list compiling the elements describing a zero-order Takagi–Sugeno–Kang fuzzy rule-based system
#'
#' @return matrix with the membership of each datum to each fuzzy rule obtained using the function FUZZIFY.FRBS.Fast and the predicted habitat suitability in the last column
#' @export
#'
#' @examples
PREDICT.FRBS.Fast <- function(Data, FRBS) {
  Data <- sapply(colnames(FRBS$Range), function(i) {
    Data[Data[, i] > FRBS$Range[2, i], i] <- FRBS$Range[2, i]
    return(Data[, i])
  })

  apply(Data, 1, function(x) {
    ExpandList <- NULL
    for (ImpVariables in FRBS$ImpVariables)
    {
      ExpandList[[length(ExpandList) + 1]] <- apply(FRBS$MFparameters[, stringr::str_detect(string = colnames(FRBS$MFparameters), pattern = ImpVariables)], 2, function(parameters) {
        FRBS$MFfunction(pattern = x[ImpVariables], parameters = parameters)
      })
    }
    Memb. <- sum(apply(expand.grid(ExpandList), 1, prod), na.rm = T)
    Value <- sum(apply(expand.grid(ExpandList), 1, prod) * FRBS$Consequents, na.rm = T) / sum(apply(expand.grid(ExpandList), 1, prod), na.rm = T)
    return(cbind(round(Memb., 3), round(Value, 3)))
  })
}

#' Function to obtain the membership of each datum to each fuzzy rule
#'
#' @param Data the the dataset in data.frame format
#' @param FRBS the list compiling the elements describing a zero-order Takagi–Sugeno–Kang fuzzy rule-based system
#'
#' @return matrix with the membership of each datum to each fuzzy rule
#' @export
#'
#' @examples
FUZZIFY.FRBS.Fast <- function(Data, FRBS) {
  apply(Data, 1, function(x) {
    ExpandList <- NULL
    for (ImpVariables in FRBS$ImpVariables)
    {
      ExpandList[[length(ExpandList) + 1]] <- apply(FRBS$MFparameters[, stringr::str_detect(string = colnames(FRBS$MFparameters), pattern = ImpVariables)], 2, function(parameters) {
        FRBS$MFfunction(pattern = x[ImpVariables], parameters = parameters)
      })
    }
    Memb. <- apply(expand.grid(ExpandList), 1, prod)
    return(Memb.)
  })
}
