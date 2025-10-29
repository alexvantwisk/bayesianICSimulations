// loglogistic_interval_aft_standard.stan
// Interval-censored Log-Logistic AFT model with standard parameterization
// Updated to follow standard AFT conventions without negative sign confusion
data {
  int<lower=1>       N;          // number of observations
  vector<lower=0>[N] L;          // left interval bounds
  vector[N]          R;          // right bounds (Inf for right-censoring)
  vector[N]          X;          // binary covariate (0/1)
  vector<lower=0>[N] w;          // survey/design weights (nonnegative)
}
transformed data {
  // Normalize weights to sum to N
  vector[N] wN;
  real sw = sum(w);
  if (sw > 0) {
    wN = w * (N / sw);
  } else {
    wN = rep_vector(1.0, N);
  }
}
parameters {
  real<lower=0> alpha;           // baseline scale parameter (median when X=0)
  real          beta;            // AFT coefficient (effect on log-scale parameter)
  real<lower=0> gamma;           // shape parameter
}
transformed parameters {
  // Pre-compute for efficiency and monitoring
  real log_alpha = log(alpha);
}
model {
  // More informative priors based on expected scale
  alpha ~ lognormal(log(5), 1);    // centered around baseline median survival ~5
  beta  ~ normal(0, 1);            // AFT coefficient prior
  gamma ~ lognormal(0, 0.5);       // tighter shape prior
  
  // Weighted log-likelihood with numerical safeguards
  // Standard AFT parameterization: log(lambda_i) = log(alpha) + beta * X_i
  for (i in 1:N) {
    if (L[i] > 0) {
      // Individual scale parameter: lambda_i = alpha * exp(beta * X_i)
      real log_lambda_i = log_alpha + beta * X[i];
      real log_L_lambda_ratio = log(L[i]) - log_lambda_i;
      real logSL = -log1p(exp(gamma * log_L_lambda_ratio));
      
      if (is_inf(R[i])) {
        // Right-censored: w * log S(L)
        target += wN[i] * logSL;
      } else {
        // Interval-censored: w * log[S(L) - S(R)]
        real log_R_lambda_ratio = log(R[i]) - log_lambda_i;
        real logSR = -log1p(exp(gamma * log_R_lambda_ratio));
        target += wN[i] * log_diff_exp(logSL, logSR);
      }
    } else {
      // L[i] == 0: special case for left boundary
      if (!is_inf(R[i])) {
        real log_lambda_i = log_alpha + beta * X[i];
        real log_R_lambda_ratio = log(R[i]) - log_lambda_i;
        real logSR = -log1p(exp(gamma * log_R_lambda_ratio));
        target += wN[i] * log1m_exp(logSR);  // log(1 - S(R))
      }
      // if L==0 and R==Inf, skip (contributes 0 to likelihood)
    }
  }
}
