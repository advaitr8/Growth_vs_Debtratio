data{
  real growth[1905];
  real growth_lag[1905];
  real debt_lag[1905];
  real labor[1905];
  // real fin_stress[1905];
  real exch[1905];
  real inf_rate[1905];
  real interest[1905];
  int country_id[1905];
  int t_id[1905];
  // int fs_id[1905];
}
parameters{
  real beta_0;
  real beta_gr[16];
  real beta_dtime[32];
  // real beta_dfs[8];
  real beta_dcountry[16];
  real beta_l;
  // real beta_fs;
  real beta_ex;
  real beta_inf_rate;
  real beta_interest;
  real <lower = 0> sigma;
  real mu_dtime;
  real <lower = 0> tau_dtime;
  real mu_dfs;
  real <lower = 0> tau_dfs;
  real mu_dcountry;
  real <lower = 0> tau_dcountry;
}
model{
  for(i in 1:1905){
    growth[i] ~ normal(beta_0 +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_dtime[t_id[i]] +
                       // beta_dfs[fs_id[i]] +
                       beta_dcountry[country_id[i]]*debt_lag[i] +
                       beta_l*labor[i] +
                       beta_ex*exch[i] +
                       beta_inf_rate*inf_rate[i] +
                       beta_interest*interest[i],
                       sigma);
  }
  beta_0 ~ normal(0,0.3);
  beta_gr ~ normal(0,0.3);
  beta_dtime ~ normal(mu_dtime, tau_dtime);
  // beta_dfs ~ normal(mu_dfs, tau_dfs);
  beta_dcountry ~ normal(mu_dcountry, tau_dcountry);
  beta_l ~ normal(0,0.5);
  // beta_fs ~ normal(0,5);
  beta_ex ~ normal(0,0.5);
  beta_inf_rate ~ normal(0,0.5);
  beta_interest ~ normal(0,0.5);
  mu_dtime ~ normal(0,0.08);
  tau_dtime ~ normal(0,0.01);
  // mu_dfs ~ normal(0,0.5);
  // tau_dfs ~ normal(0,0.01);
  mu_dcountry ~ normal(0,0.3);
  tau_dcountry ~ normal(0,0.01);
}
