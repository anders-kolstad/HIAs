plot_population_dist <- function(national_data) {
  national_data |>
    tidyr::unnest(population_sample) |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = population_sample), binwidth = 0.1) +
    ggplot2::facet_wrap(ggplot2::vars(distribution), scales = "free")
}

plot_domain_dist <- function(domain_data) {
  domain_data |>
    tidyr::unnest(values) |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = sample), binwidth = 0.1) +
    ggplot2::xlim(-0.2, 1.2) +
    ggplot2::facet_wrap(ggplot2::vars(sample_ID), scales = "free_y")
}


make_national_data <- function(n = 1000, seed = 123) {
  set.seed(seed)
  tibble::tibble(
    uniform = stats::runif(n, 0, 1),
    normal_centered = stats::rnorm(n, 0.5, 0.2),
    normal_high = stats::rnorm(n, 1, 0.5),
    beta_2_2 = stats::rbeta(n, 2, 2),
    beta_05_3 = stats::rbeta(n, 0.5, 3),
    beta_3_05 = stats::rbeta(n, 3, 0.5)
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::case_when(. >= 1 ~ 1, . <= 0 ~ 0, TRUE ~ .))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "distribution", values_to = "population_sample") |>
    tidyr::nest(population_sample = population_sample)
}

make_domain_data <- function(seed = 123) {
  a <- 2; b <- 5; c <- 10; d <- 30
  set.seed(seed)
  tibble::tibble(
    sample_ID = c(
      "domain_1a","domain_1b","domain_1c","domain_1d",
      "domain_2a","domain_2b","domain_2c","domain_2d",
      "domain_3a","domain_3b","domain_3c","domain_3d",
      "domain_4b","domain_4c","domain_4d",
      "domain_5b","domain_5c","domain_5d",
      "domain_6a","domain_6b","domain_6c","domain_6d"
    ),
    values = list(
      tibble::tibble(sample = rep(1, a)),
      tibble::tibble(sample = rep(1, b)),
      tibble::tibble(sample = rep(1, c)),
      tibble::tibble(sample = rep(1, d)),

      tibble::tibble(sample = seq.int(0.7, 1, length.out = a)),
      tibble::tibble(sample = seq.int(0.7, 1, length.out = b)),
      tibble::tibble(sample = seq.int(0.7, 1, length.out = c)),
      tibble::tibble(sample = seq.int(0.7, 1, length.out = d)),

      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = a)),
      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = b)),
      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = c)),
      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = d)),

      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(b, 1, 0.5))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(c, 1, 0.5))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(d, 1, 0.5))))),

      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(b, 1, 0.1))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(c, 1, 0.1))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(d, 1, 0.1))))),

      tibble::tibble(sample = rep(0.9, a)),
      tibble::tibble(sample = rep(0.9, b)),
      tibble::tibble(sample = rep(0.9, c)),
      tibble::tibble(sample = rep(0.9, d))
    )
  )
}

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