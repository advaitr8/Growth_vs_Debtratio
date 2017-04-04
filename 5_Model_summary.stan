data{
  real growth[1930];
  real growth_lag[1930];
  real debt_lag[1930];
  real d_2[1930];
  real d_3[1930];
  real d_4[1930];
  real d_5[1930];
  real labor[1930];
  real fin_stress[1930];
  int country_id[1930];
}
parameters{
  real beta_0[16];
  real beta_gr[16];
  real beta_d[16];
  real beta_d2[16];
  real beta_d3[16];
  real beta_d4[16];
  real beta_d5[16];
  real beta_l[16];
  real beta_fs[16];
  real <lower = 0> sigma;
  real mu_0;
  real <lower = 0> tau_0;
  real mu_gr;
  real <lower = 0> tau_gr;
  real mu_d;
  real <lower = 0> tau_d;
  real mu_d2;
  real <lower = 0> tau_d2;
  real mu_d3;
  real <lower = 0> tau_d3;
  real mu_d4;
  real <lower = 0> tau_d4;
  real mu_d5;
  real <lower = 0> tau_d5;
  real mu_l;
  real <lower = 0> tau_l;
  real mu_fs;
  real <lower = 0> tau_fs;
  
}
model{
  for (i in 1:1930){
    growth[i] ~ normal(beta_0[country_id[i]] +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_d[country_id[i]]*debt_lag[i] +
                       beta_d2[country_id[i]]*d_2[i] +
                       beta_d3[country_id[i]]*d_3[i] +
                       beta_d4[country_id[i]]*d_4[i] +
                       beta_d5[country_id[i]]*d_5[i] +
                       beta_l[country_id[i]]*labor[i] +
                       beta_fs[country_id[i]]*fin_stress[i],
                       sigma);
  }
  beta_0 ~ normal(mu_0,tau_0);
  beta_gr ~ normal(mu_gr, tau_gr);
  beta_d ~ normal(mu_d, tau_d);
  beta_d2 ~ normal(mu_d2, tau_d2);
  beta_d3 ~ normal(mu_d3, tau_d3);
  beta_d4 ~ normal(mu_d4, tau_d4);
  beta_d5 ~ normal(mu_d5, tau_d5);
  beta_l ~ normal(mu_l, tau_l);
  beta_fs ~ normal(mu_fs, tau_fs);
  mu_0 ~ normal(0,3);
  tau_0 ~ normal(0,3);
  mu_gr ~ normal(0,3);
  tau_gr ~ normal(0,3);
  mu_d ~ normal(0,3);
  tau_d ~ normal(0,3);
  mu_d2 ~ normal(0,3);
  tau_d2 ~ normal(0,3);
  mu_d3 ~ normal(0,3);
  tau_d3 ~ normal(0,3);
  mu_d4 ~ normal(0,3);
  tau_d4 ~ normal(0,3);
  mu_d5 ~ normal(0,3);
  tau_d5 ~ normal(0,3);
  mu_l ~ normal(0,3);
  tau_l ~ normal(0,3);
  mu_fs ~ normal(0,3);
  tau_fs ~ normal(0,3);
  
}
generated quantities{
  real pred[1930];
  for (i in 1:1930){
    pred[i] = normal_rng(beta_0[country_id[i]] +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_d[country_id[i]]*debt_lag[i] +
                       beta_d2[country_id[i]]*d_2[i] +
                       beta_d3[country_id[i]]*d_3[i] +
                       beta_d4[country_id[i]]*d_4[i] +
                       beta_d5[country_id[i]]*d_5[i] +
                       beta_l[country_id[i]]*labor[i] +
                       beta_fs[country_id[i]]*fin_stress[i],
                       sigma);
  }

}
