// loglogistic_interval_multivariable.stan
// Interval-censored Log-Logistic AFT with vector covariate
data {
  int<lower=1>       N;
  int<lower=1>       K;
  vector<lower=0>[N] L;
  vector[N]          R;
  matrix[N, K]       X;
  vector<lower=0>[N] w;
}
transformed data {
  vector[N] wN;
  real sw = sum(w);
  if (sw > 0) {
    wN = w * (N / sw);
  } else {
    wN = rep_vector(1.0, N);
  }
}
parameters {
  real<lower=0> alpha;
  vector[K]     beta;
  real<lower=0> gamma;
}
transformed parameters {
  real log_alpha = log(alpha);
}
model {
  alpha ~ lognormal(log(5), 1);
  beta  ~ normal(0, 1);          // independent N(0,1) on each coefficient
  gamma ~ lognormal(0, 0.5);

  for (i in 1:N) {
    // Standard AFT parameterization: log(lambda_i) = log(alpha) + X[i] . beta
    real log_lambda_i = log_alpha + dot_product(X[i], beta);
    if (L[i] > 0) {
      real log_L_lambda_ratio = log(L[i]) - log_lambda_i;
      real logSL = -log1p_exp(gamma * log_L_lambda_ratio);
      // Right-censored: w * log S(L)
      if (is_inf(R[i])) {
        target += wN[i] * logSL;
      // Interval-censored: w * log[S(L) - S(R)]
      } else {
        real log_R_lambda_ratio = log(R[i]) - log_lambda_i;
        real logSR = -log1p_exp(gamma * log_R_lambda_ratio);
        target += wN[i] * log_diff_exp(logSL, logSR);
      }
    // L[i] == 0: special case for left boundary
    } else if (!is_inf(R[i])) {
      real log_R_lambda_ratio = log(R[i]) - log_lambda_i;
      real logSR = -log1p_exp(gamma * log_R_lambda_ratio);
      target += wN[i] * log1m_exp(logSR);
    }
  }
}
