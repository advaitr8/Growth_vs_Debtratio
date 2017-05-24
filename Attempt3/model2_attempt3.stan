data{
  real growth[1289];
  real growth_lag[1289];
  real eur_debt[1289];
  real labor[1289];
  real exch[1289];
  real inf_rate[1289];
  real interest[1289];
  int <lower = 0> country_id[1289];
  int <lower = 0> fs_id[1289];
}
parameters{
  real beta_0;
  real beta_fs[6];
  real beta_gr;
  real beta_eur_debt[16];
  real beta_l;
  real beta_ex;
  real beta_inf_rate;
  real beta_interest;
  real <lower = 0> sigma;
  real mu_fs;
  real <lower = 0> tau_fs;
  real mu_eur_debt;
  real <lower = 0> tau_eur_debt;
}
model{
  for(i in 1:1289){
    growth[i] ~ normal(beta_0
                       + beta_fs[fs_id[i]]
                       + beta_gr*growth_lag[i]
                       + beta_eur_debt[country_id[i]]*eur_debt[i]
                       + beta_l*labor[i]
                       + beta_ex*exch[i]
                       + beta_inf_rate*inf_rate[i]
                       + beta_interest*interest[i], sigma);
  }
  beta_0 ~ normal(0,0.01);
  // beta_fs ~ normal(0,0.1);
  for(i in 1:6){
  beta_fs[i] ~ normal(mu_fs,tau_fs);
  }
  beta_gr ~ normal(0,0.5);
  // beta_debt ~ normal(0,0.5);

  beta_eur_debt ~ normal(mu_eur_debt, tau_eur_debt);

  beta_l ~ normal(0,0.5);
  beta_ex ~ normal(0,0.5);
  beta_inf_rate ~ normal(0,0.5);
  beta_interest ~ normal(0,0.5);
  mu_fs ~ normal(0,0.001);
  tau_fs ~ cauchy(0,0.0001);
  mu_eur_debt ~ normal(0,0.01);
  tau_eur_debt ~ cauchy(0,0.0005);
  sigma ~ cauchy(0,0.5);
//   }
// generated quantities{
//   real y_pred[1289];
//   for(i in 1:1289){
//     y_pred[i] = normal_rng(beta_0
//                            + beta_fs[fs_id[i]]
//                            + beta_gr*growth_lag[i]
//                            + beta_debt[country_id[i]]*debt_lag[i]
//                            + beta_l*labor[i]
//                            + beta_ex*exch[i]
//                            + beta_inf_rate*inf_rate[i]
//                            + beta_interest*interest[i], sigma);
//   }
}

