#' @title My Normal Curve Function
#'
#' @param mu the average of the normal
#' @param sigma the standard deviation of the normal
#' @param a upper bound of lower tail probability
#'
#' @returns A plot of the density curve of the normal and a list contatining probability info
#' @export
#'
#' @examples
#' myncurve(0, 1)
#' myncurve(5.5, 2.3, 6)
myncurve = function(mu, sigma, a = 0){
  # plot curve of normal
  curve(dnorm(x,mean=mu,sd=sigma),
        xlim = c(mu-3*sigma, mu+3*sigma),
        lwd=2)

  xcurve=seq(mu-3*sigma,a,length=1000) # x values corresponding to the x - cords of points on the curve
  ycurve=dnorm(xcurve,mean=mu,sd=sigma) # Y values corresponding t0 the x values
  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="lightblue")
  # Find lower tail probability
  prob=pnorm(a,mean=mu,sd=sigma) # pnorm calculates lower tail, so -inf bound holds
  prob=round(prob,4)
  list(mu = mu, sigma = sigma, prob = prob)
}
