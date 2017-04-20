data{
  real growth[1930];
  real growth_lag[1930];
  real debt_lag[1930];
  real labor[1930];
  real fin_stress[1930];
  int country_id[1930];
}
parameters{
  real beta_0[16];
  real beta_gr[16];
  real beta_d[16];
  real beta_l[16];
  real <lower = 0> sigma;
  real alpha_d_int;
  real alpha_s[16];
  real <lower = 0> tau_alpha;
  real mu_0;
  real <lower = 0> tau_0;
  real mu_gr;
  real <lower = 0> tau_gr;
  real mu_d;
  real <lower = 0> tau_d;
  real mu_l;
  real <lower = 0> tau_l;
}
model{
  for (i in 1:1930){
    growth[i] ~ normal(beta_0[country_id[i]] +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_d[country_id[i]]*debt_lag[i] +
                       beta_l[country_id[i]]*labor[i],
                       sigma);
    beta_d[country_id[i]] ~ normal(alpha_d_int + 
                                   alpha_s[country_id[i]]*fin_stress[i], 
                                   tau_alpha);
  }
  beta_0 ~ normal(mu_0,tau_0);
  beta_gr ~ normal(mu_gr, tau_gr);
  beta_d ~ normal(mu_d, tau_d);
  beta_l ~ normal(mu_l, tau_l);
  alpha_d_int ~ normal(0,1);
  alpha_s ~ normal(0,1);
  tau_alpha ~ normal(0,1);
  mu_0 ~ normal(0,1);
  tau_0 ~ normal(0,1);
  mu_gr ~ normal(0,1);
  tau_gr ~ normal(0,1);
  mu_d ~ normal(0,1);
  tau_d ~ normal(0,1);
  mu_l ~ normal(0,1);
  tau_l ~ normal(0,1);
}
