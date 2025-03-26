#' @title Airline ticket overbooking simulator
#'
#' @description Uses binomial and normal variables to test the number of tickets an airline should sell given parameters
#'
#' @param N the total seats on the flight
#' @param gamma probability that every ticket holder shows up to board
#' @param p probability that a person who is sold a ticket shows up to board
#' @param layout include both generated plots in a single window. use this if running outside Rmd
#' @export
#'
#' @returns two plots: one showing distribution values used to calculate the number of tickets to sell with a binomial distribution, and the other with a normal approximation. Also returns a list containing these calculated values, N, gamma, and p.
#'
#' @examples
#' ntickets()
#' ntickets(N=400, gamma=0.02, p=0.95)
ntickets <- function(N=200, gamma=0.02, p=0.95, layout=FALSE) {
  # adjust font sizes
  old_par <- graphics::par(no.readonly=TRUE)
  graphics::par(cex=0.8, cex.axis=0.8, cex.lab=0.9, cex.main=1)

  # find nd
  n <- seq(N,N*1.1) #generate discrete values that cover cumulative dist curve for size N
  pd <- 1-gamma-stats::pbinom(N,n,p) #binomial cumulative dist values for each n
  nd <- N + which.min(abs(pd)) - 1 #finds first instance where pd is greater than 0. sub 1 to account for vector index starting at 1

  if (layout==TRUE) {
    graphics::layout(1:2) #puts figs into one window, use if running outside of Rmd
  }

  # find nc
  pc <- function(n) {
    return(1-gamma-stats::pnorm(N+0.5, mean=n*p, sd=sqrt(n*p*(1-p)))) #N+0.5 accounts for continuity correction of lower tail probability
  }
  nc <- stats::uniroot(f=pc, interval=c(N,N*1.1))$root

  # discrete plot
  plot(x=n, y=1-gamma-stats::pbinom(N,n,p),
       type="b",
       main=paste("Objective vs n to find optimal tickets sold\n(", nd, ") gamma=", gamma,"N=", N, "discrete"),
       xlab="n",
       ylab="Objective",
       bg="blue",
       pch=21)
  graphics::abline(v=nd, h=0,
         col="red",
         lwd=2)

  # continuous plot
  graphics::curve(pc(x), xlim=c(N,N*1.1),
        main = paste("Objective vs n to find optimal tickets sold\n(", nc, ") gamma=", gamma,"N=", N, "continuous"),
        xlab = "n",
        ylab = "Objective")
  graphics::abline(v=nc, h=0,
         col="blue")

  # restore original parameters
  graphics::par(old_par)

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
