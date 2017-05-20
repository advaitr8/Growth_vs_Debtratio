data{
  real growth[1905];
  real growth_lag[1905];
  real debt_lag[1905];
  real labor[1905];
  real exch[1905];
  real inf_rate[1905];
  real interest[1905];
  int country_id[1905];
  int fs_id[1905];
}
parameters{
  real beta_0;
  real beta_fs[6];
  real beta_gr;
  real beta_debt[16];
  real beta_l;
  real beta_ex;
  real beta_inf_rate;
  real beta_interest;
  real <lower = 0> sigma;
  real mu_fs;
  real <lower = 0> tau_fs;
  real mu_debt;
  real <lower = 0> tau_debt;
}
model{
  for(i in 1:1905){
    growth[i] ~ normal(beta_0
                       + beta_fs[fs_id[i]]
                       + beta_gr*growth_lag[i]
                       + beta_debt[country_id[i]]*debt_lag[i]
                       + beta_l*labor[i]
                       + beta_ex*exch[i]
                       + beta_inf_rate*inf_rate[i]
                       + beta_interest*interest[i], sigma);
  }
  beta_0 ~ normal(0,0.1);
  // beta_fs ~ normal(0,0.1);
  beta_fs ~ normal(mu_fs,tau_fs);
  beta_gr ~ normal(0,0.5);
  // beta_debt ~ normal(0,0.5);
  beta_debt ~ normal(mu_debt, tau_debt);
  beta_l ~ normal(0,0.5);
  beta_ex ~ normal(0,0.5);
  beta_inf_rate ~ normal(0,0.5);
  beta_interest ~ normal(0,0.5);
  mu_fs ~ normal(0,0.1);
  tau_fs ~ cauchy(0,0.1);
  mu_debt ~ normal(0,0.1);
  tau_debt ~ cauchy(0,0.1);
  sigma ~ cauchy(0,0.5);
  }

