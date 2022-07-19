#TAENIA SOLIUM IN BURUNDI - DISTRIBUTION ESTIMATES
## last update: 18/11/2021
## set seed to allow for reproducibility
set.seed(264)

## number of iterations
n <- 1e5

## epilepsy prevalence, Beta parameters
e_prev_alpha <-42+43
e_prev_beta <-(4913+3218)-(43+42)
e_prev<-rbeta(n,e_prev_alpha,e_prev_beta)

## proportion PWE with NCC based on Ag-ELISA, Uniform parameter 
prop_ncc_min <- 0.049
prop_ncc_max <- 0.383 
prop_ncc<-runif(n,prop_ncc_min,prop_ncc_max)

## hospitalization probability, Beta parameter
p_hosp_alpha <- 28
p_hosp_beta <- 352-28
p_hosp <-rbeta(n,p_hosp_alpha,p_hosp_beta)

## hospitalization duration, Uniform parameter
stay_min <- 1
stay_max <- 107
stay <-runif(n,stay_min,stay_max)

# Care seeking medical doctor, Beta parameter
p_care_doc_trad_alpha <- 59
p_care_doc_trad_beta <- 352-59
p_care<-rbeta(n,p_care_doc_trad_alpha,p_care_doc_trad_beta)

# Care without treatment, Beta parameter
p_care_ntreat_alpha <- 270
p_care_ntreat_beta <- 324-270
ntreat<-rbeta(n,p_care_ntreat_alpha,p_care_ntreat_beta)

## number of visits to healthcare provider, Uniform parameter
n_visit_med_min <- 1
n_visit_med_max <- 12
visit<-runif(n,n_visit_med_min,n_visit_med_max)

#loss of working time days per year, Gamma distribution
loss_workingtime_shape <- 4.8
loss_workingtime_rate <- 0.4
work1<-rgamma(n,loss_workingtime_shape,loss_workingtime_rate)

## probability of losing job, Beta parameter
unemployed_duetoepilepsy_alpha <- 57
unemployed_duetoepilepsy_beta <- 352-57
unemployed<-rbeta(n,unemployed_duetoepilepsy_alpha,unemployed_duetoepilepsy_beta)

## number of working days per year, Uniform parameter
working_days_min <- 220
working_days_max <- 312
working<-runif(n,working_days_min,working_days_max)

# Monthly salary USING GAMMA DISTRIBUTION
salaries_shape <-2.16
salaries_rate <-2.4e-05
salaries_total <-rgamma(n, salaries_shape, salaries_rate)

## cost of hospitalization per day,uniform parameter
price_day_hosp_min <- 5000
price_day_hosp_max <- 10000
price<-runif(n,price_day_hosp_min,price_day_hosp_max)

# value of a pig, Using gamma distribution
pig_shape<-7.5
pig_rate<-5e-05
pigs_total<-rgamma(n,pig_shape,pig_rate)

## Reduction price for a pig, Uniform parameters
price_loss_pigs_min <- 0.7
price_loss_pigs_max <- 0.8
price_loss_pigs<-runif(n,price_loss_pigs_min,price_loss_pigs_max)

## porcine cysticercosis prevalence, Beta parameter
prev_pigs_alpha <- 77
prev_pigs_beta <- 496-77
prevpig<-rbeta(n,prev_pigs_alpha,prev_pigs_beta)

## case fatality ratio, Beta distribution
ep_cfr_alpha <- 279
ep_cfr_beta <- 41575-279
fatality<-rbeta(n,ep_cfr_alpha,ep_cfr_beta)

###Using Dirichlet distribution for probability of treatment
Carbamazepine:11
Phenobarbital:4
Phenytoin:2
Valproate:13
p_treat <- c(11, 4, 2, 13)
xyz<- rdirichlet(n, p_treat)
nCarb <- xyz[,1]
nPhB<- xyz[,2]
nPhT <- xyz[,3]
nValproate <- xyz[,4]

# Summaries
Summary <-
  function(x) {
    print(c(mean = mean(x),
            quantile(x, c(.025, .975))))
  }

Summary(e_prev)
Summary(prop_ncc)
Summary(p_hosp)
Summary(stay)
Summary(p_care)
Summary(ntreat)
Summary(visit)
Summary(work1)
Summary(unemployed)
Summary(working)
Summary(salaries_total)
Summary(price)
Summary(pigs_total)
Summary(price_loss_pigs)
Summary(prevpig)
Summary(fatality)
Summary(phenoba)
Summary(carbama)
Summary(phenytoin)
Summary(valproate)
Summary(nCarb)
Summary(nPhB)
Summary(nPhT)
Summary(nValproate)












