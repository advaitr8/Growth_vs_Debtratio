data{
  real growth[1930];
  real growth_lag[1930];
  real regime_hi[1930];
  real debt_lag_lo[1930];
  real debt_lag_hi[1930];
  real labor[1930];
  int country_id[1930];
}
parameters{
  real beta_0[16];
  real beta_gr[16];
  real beta_dhi_int[16];
  real beta_dlo[16];
  real beta_dhi[16];
  real beta_l[16];
  real <lower = 0> sigma;
  real mu_0;
  real mu_gr;
  real mu_dhi_int;
  real mu_dlo;
  real mu_dhi;
  real mu_l;
  real <lower = 0> tau_0;
  real <lower = 0> tau_gr;
  real <lower = 0> tau_dhi_int;
  real <lower = 0> tau_dlo;
  real <lower = 0> tau_dhi;
  real <lower = 0> tau_l;
}
model{
  for(i in 1:1930){
    growth[i] ~ normal(beta_0[country_id[i]] + 
                       beta_dhi_int[country_id[i]]*regime_hi[i] +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_dlo[country_id[i]]*debt_lag_lo[i] +
                       beta_dhi[country_id[i]]*debt_lag_hi[i] +
                       beta_l[country_id[i]]*labor[i],
                       sigma);
  }
  beta_0 ~ normal(mu_0, tau_0);
  beta_gr ~ normal(mu_gr, tau_gr);
  beta_dhi_int ~ normal(mu_dhi_int, tau_dhi_int);
  beta_dlo ~ normal(mu_dlo, tau_dlo);
  beta_l ~ normal(mu_l, tau_l);
  mu_0 ~ normal(0,1);
  tau_0 ~ normal(0,1);
  mu_gr ~ normal(0,1);
  tau_gr ~ normal(0,1);
  mu_l ~ normal(0,1);
  tau_l ~ normal(0,1);
  mu_dhi_int ~ normal(0,1);
  tau_dhi_int ~ normal(0,1);
  mu_dhi ~ normal(0,1);
  tau_dhi ~ normal(0,1);
  }
generated quantities{
  real pred[1930];
  for(i in 1:1930){
    pred[i] = normal_rng(beta_0[country_id[i]] + 
                       beta_dhi_int[country_id[i]]*regime_hi[i] +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_dlo[country_id[i]]*debt_lag_lo[i] +
                       beta_dhi[country_id[i]]*debt_lag_hi[i] +
                       beta_l[country_id[i]]*labor[i],
                       sigma);
  }
}
