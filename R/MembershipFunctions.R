#' Gaussian membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
GAUSSMF <- function(pattern, parameters)
{
exp(-1/2*(pattern-parameters[1])^2/parameters[2]^2)
}

#' Gaussian bell membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
GBELLMF <- function(pattern, parameters)
{
1/(1+abs((pattern-parameters[3])/parameters[1])^(2*parameters[2]))
}

#' Triangular membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
TRIMF <- function(pattern, parameters)
{
approx(x=parameters, y = c(0,1,0), xout=pattern, method = "linear", rule=c(2, 2), ties = max)$y
}

#' Trapezoidal membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
TRAPMF <- function(pattern, parameters)
{
approx(x=parameters, y = c(0,1,1,0), xout=pattern, method = "linear", rule=c(2, 2), ties = max)$y
}

#' Sigmoidal membership functions
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
SIGMF <- function(pattern, parameters)
{
1/(1 + exp(-parameters[1] * (pattern - parameters[2])))
}

#' Double sigmoidal membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
DSIGMF <- function(pattern, parameters)
{
abs(1/(1 + exp(-parameters[1] * (pattern - parameters[2])))-1/(1 + exp(-parameters[3] * (pattern - parameters[4]))))
}

#' Product of two sigmoidal membership functions
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
PSIGMF <- function(pattern, parameters)
{
1/(1 + exp(-parameters[1] * (pattern - parameters[2])))*1/(1 + exp(-parameters[3] * (pattern - parameters[4])))
}

#' Pi membership functions
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
PIMF <- function(pattern, parameters)
{
Membership <- c()
for(z in 1:length(pattern))
{
# plateau
if(pattern[z]>=parameters[2] & pattern[z]<=parameters[3])
{y <- 1}

# Steep descend
if(pattern[z]>=mean(parameters[1:2]) & pattern[z]<parameters[2])
{y <- 1-2*((pattern[z]-parameters[2])/(diff(parameters[1:2])))^2}

if(pattern[z]>parameters[3] & pattern[z]<=mean(parameters[3:4]))
{y <- 1-2*((pattern[z]-parameters[3])/(diff(parameters[3:4])))^2}

# Smooth descend
if(pattern[z]>=parameters[1] & pattern[z]<mean(parameters[1:2]))
{y <- 2*((pattern[z]-parameters[1])/(diff(parameters[1:2])))^2}

if(pattern[z]>mean(parameters[3:4]) & pattern[z]<=parameters[4])
{y <- 2*((pattern[z]-parameters[4])/(parameters[4] - parameters[3]))^2}

# External value
if(pattern[z]<parameters[1] | pattern[z]>parameters[4])
{y <- 0}

Membership <- c(Membership,y)

}
return(Membership)
}

#' Z-shaped membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
ZMF <- function(pattern, parameters)
{
Membership <- c()
for(z in 1:length(pattern))
{
if(pattern[z]<=parameters[1])
{y <- 1}
#5
if(pattern[z]>parameters[1] & pattern[z]<=mean(parameters[1:2]))
{y <- 1-2*((pattern[z]-parameters[1])/(diff(parameters[1:2])))^2}
#6
if(pattern[z]>mean(parameters[1:2]) & pattern[z]<=parameters[2])
{y <- 2*((pattern[z]-parameters[2])/(parameters[2] - parameters[1]))^2}
#7
if(pattern[z]>parameters[2])
{y <- 0}
Membership <- c(Membership,y)
}
Membership
}

#' S-shaped membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @export
#'
#' @examples
SMF  <- function(pattern, parameters)
{
Membership <- c()
for(z in 1:length(pattern))
{if(pattern[z]<parameters[1])
{y <- 0}
#2
if(pattern[z]>=parameters[1] & pattern[z]<mean(parameters[1:2]))
{y <- 2*((pattern[z]-parameters[1])/(diff(parameters[1:2])))^2}
#3
if(pattern[z]>=mean(parameters[1:2]) & pattern[z]<parameters[2])
{y <- 1-2*((pattern[z]-parameters[2])/(diff(parameters[1:2])))^2}
#4
if(pattern[z]>=parameters[2])
{y <- 1}
Membership <- c(Membership,y)
}
Membership
}
