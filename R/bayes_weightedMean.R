wgt_mean <- function (x, weights, sigma.x = NULL, mu = NULL, mu.prior = NULL, n.mu = 50, 
          ...) 
{
  if (n.mu < 3) 
    stop("Number of prior values of theta must be greater than 2")
  if (is.null(mu)) {
    mu = seq(min(x) - sigma.x, max(x) + sigma.x, length = n.mu)
    mu.prior = rep(1/n.mu, n.mu)
  }
  mx = weighted.mean(x, weights)
  quiet = Bolstad.control(...)$quiet
  if (is.null(sigma.x)) {
    sigma.x = sd(x - mx)
    if (!quiet) {
      cat(paste("Standard deviation of the residuals :", 
                signif(sigma.x, 4), "\n", sep = ""))
    }
  }
  else {
    if (sigma.x > 0) {
      if (!quiet) {
        cat(paste("Known standard deviation :", signif(sigma.x, 
                                                       4), "\n", sep = ""))
      }
    }
    else {
      stop("The standard deviation must be greater than zero")
    }
  }
  if (any(mu.prior < 0) | any(mu.prior > 1)) 
    stop("Prior probabilities must be between 0 and 1 inclusive")
  if (round(sum(mu.prior), 7) != 1) {
    warning("The prior probabilities did not sum to 1, therefore the prior has been normalized")
    mu.prior = mu.prior/sum(mu.prior)
  }
  n.mu = length(mu)
  nx = length(x)
  snx = sigma.x^2/nx
  likelihood = exp(-0.5 * (mx - mu)^2/snx)
  posterior = likelihood * mu.prior/sum(likelihood * mu.prior)
  if (Bolstad.control(...)$plot) {
    plot(mu, posterior, ylim = c(0, 1.1 * max(posterior, 
                                              mu.prior)), pch = 20, col = "blue", xlab = expression(mu), 
         ylab = expression(Probabilty(mu)))
    points(mu, mu.prior, pch = 20, col = "red")
    legend("topleft", bty = "n", fill = c("blue", "red"), 
           legend = c("Posterior", "Prior"), cex = 0.7)
  }
  mx = sum(mu * posterior)
  vx = sum((mu - mx)^2 * posterior)
  results = list(name = "mu", param.x = mu, prior = mu.prior, 
                 likelihood = likelihood, posterior = posterior, weighted_mean = mx, 
                 var = vx, cdf = function(x, ...) cumDistFun(x, mu, posterior), 
                 quantileFun = function(probs, ...) qFun(probs, mu, posterior))
  class(results) = "Bolstad"
  invisible(results)
}
