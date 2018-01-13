# Notes:
#
# 1. You can learn more about package authoring at:
#
#   http://r-pkgs.had.co.nz/
#   http://www.pzhao.org/zh/post/rmickey/#comment-3314475182
#   https://cosx.org/2013/11/building-r-packages-easily
#
# 2. Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#
#
# 3. Tools needed:
#  a. rtools
#  b. install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
#
# 4. load tools
# library(devtools)


#**************************************
#    1. PV of Annuities           #####
#**************************************

#' Function calculating temporary annuity values from age x to retirment age (fixed end)
#'
#' suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
#' The function computes a..{x, f - x} and s_a..{x, f - x}, x runing from a1 to aN.
#' The length of px is f - a1
#' Note that the last element is redundant, just used as a place holder.
#'
#'
#' @param px an vector of composite survivial probs from age x to x + n - 1. Length = n
#' @param i  discount rate, scalar
#' @param scale  how the annuity scale up over time. eg:
#'               1) salary scale. default is a n vector of 1, meaning no salary scale. used when calculating career based annuity
#'               2) simple COLA scale: COLA increasing at a fixed percentage very year.
#'
#' @return an n vector storing the value of temporary life annuities from age x to age x + n - 1.
#' @export
#'
#' @examples
get_tla <- function(px, i, scale = rep(1, length(px))){


	tla <- numeric(length(px))
	n <- length(tla)

	for(j in 1:n){
		v   <- 1/(1 + i)^(0:(n - j))                                   # dicount vector
		if(j < n) pxr <- cumprod(c(1, px[j:(n - 1)])) else pxr <-  1   # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
		SS  <- scale[j:n]/scale[j]                                     # scale
		tla[j] <-  sum(SS * v * pxr)                                   # computing annuity value at j
	}
	return(tla)
}
