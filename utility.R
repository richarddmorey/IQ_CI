dnormposterior <- function(mu, ybar, s2, N, mu_0=0, n_0=0, nu_0=0, sig2_0=0)
{
  mu_1 = ( n_0 * mu_0 + N * ybar ) / ( n_0 + N )
  nu_1 = N + n_0
  S2_1 = nu_0*sig2_0 + (N - 1)*s2 + n_0*N/(n_0 + N) * (ybar - mu_0)^2
  sig2_1 = S2_1 / nu_1
  posterior_se = sqrt( sig2_1 / ( n_0 + N ) )
  dens = dt( (mu - mu_1)/posterior_se, nu_1 ) / posterior_se  
  return(list(
    dens = dens,
    mu = mu,
    ybar = ybar,
    s2 = s2,
    N = N,
    mu_0 = mu_0,
    n_0 = n_0,
    nu_0 = nu_0,
    sig2_0 = sig2_0,
    mu_1 = mu_1,
    nu_1 = nu_1,
    S2_1 = S2_1,
    sig2_1 = sig2_1,
    posterior_se = posterior_se
  ))
}
