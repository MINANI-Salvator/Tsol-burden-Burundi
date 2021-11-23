### TAENIA SOLIUM IN Burundi - ECONOMIC LOSSES
###Last update: 18/11/2021
### SETTINGS
###

## set seed to allow for reproducibility
set.seed(264)

## number of iterations
n <- 1e5

## currency conversion factor, Burundian francs to US Dollar
bif2usd<- 1915

## HUMANS

## population size
pop <- 11215578

## epilepsy prevalence, Beta parameters
e_prev_alpha <-42+43
e_prev_beta <-(4913+3218)-(43+42)
  
## proportion PWE with NCC based on Ag-ELISA, Uniform parameters 
prop_ncc_min <- 0.049
prop_ncc_max <- 0.383 

## hospitalization probability, Beta parameters
p_hosp_alpha <- 28
p_hosp_beta <- 352-28

## hospitalization duration, Uniform parameters
stay_min <- 1
stay_max <- 107

## care seeking probabilities, Beta parameters
 
# Care seeking both doctor and traditional healers
p_care_doc_trad_alpha <- 59
p_care_doc_trad_beta <- 352-59
  
 # Care without treatment
p_care_ntreat_alpha <- 270
p_care_ntreat_beta <- 324-270

## number of visits to healthcare provider, Uniform parameters
n_visit_med_min <- 1
n_visit_med_max <- 12

## number of visits to a traditional healer, fixed (NOT APPLICABLE)


## loss of working time days per year, Uniform parameters 
loss_workingtime_min <- 2
loss_workingtime_max <- 24

## probability of losing job, Beta parameters
unemployed_duetoepilepsy_alpha <- 57
unemployed_duetoepilepsy_beta <- 352-57

## number of working days per year, Uniform parameters
working_days_min <- 220
working_days_max <- 312

## proportion of active population, Fixed
active <- 0.5506

## monthly salary,Uniform parameter
monthly_salary_min <- 30000
monthly_salary_max <- 150000

## cost of medication (Consultation), Fixed
price_med <- 3000

## cost of hospitalization per day,uniform parameters
price_day_hosp_min <- 5000
price_day_hosp_max <- 10000


## price of medication by month, Fixed
price_carba <- 7200
price_PhB <- 4650
price_PhT <-1120
price_Valproate <- 10150

## number of working days per month, Fixed
n_working_day_by_month <- 26


## PIGS

##Gross national per capita income (GNI)
GNI <- 270

## pig population sizes, Fixed
n_pigs_smallscale <- 708867

## value of a pig, Uniform parameters
price_pigs_min <- 100000
price_pigs_max <- 200000

## Reduction price for a pig, Uniform parameters
price_loss_pigs_min <- 0.7
price_loss_pigs_max <- 0.8


## proportion of pigs sold per year, Fixed (Assumption)
pigs_sold <- 0.3

## porcine cysticercosis prevalence, Beta parameters
prev_pigs_alpha <- 77
prev_pigs_beta <- 496-77

###
### SIMULATIONS, HUMANS
###

## epilepsy prevalence
e_prev <- rbeta(n, e_prev_alpha, e_prev_beta)

## proportion PWE with NCC
prop_ncc <- runif(n, prop_ncc_min, prop_ncc_max)

## prevalence NCC-associated epilepsy
ep_ncc <- e_prev * prop_ncc

## epilepsy cases due to ncc
n_ncc <- ep_ncc * pop

## number of hospitalized NCC patients
p_hosp <- rbeta(n, p_hosp_alpha, p_hosp_beta)
n_hosp <- n_ncc * p_hosp

## duration of hospital stay
## .. per iteration, generate 'n_hosp' random durations, and sum them up
stay <- apply(t(n_hosp), 2, function(x) sum(runif(x, stay_min, stay_max)))

## patients not in hospital, seeking medical treatment
n_ncc2 <- n_ncc - n_hosp
n_medheal<-n_ncc2*rbeta(n,p_care_doc_trad_alpha,p_care_doc_trad_beta)
n_notreat<-n_ncc2*rbeta(n,p_care_ntreat_alpha,p_care_ntreat_beta)

## number of visits to the medical doctor
n_visit_med <- runif(n, n_visit_med_min, n_visit_med_max)

## price of traditional healer (NA)

## Epilepsy treatment probability, multinomial parameters
p_treatment <- c(0.37,0.13, 0.07, 0.43)

n_ncc3 <-(n_hosp+n_medheal)

xyz <- rmultinom(n, n_ncc3, p_treatment)
n_Carb <- xyz[1, ]
n_PhB<- xyz[2, ]
n_PhT <- xyz[3, ]
n_Valproate <- xyz[4, ]


## working days per year
working_days <- runif(n, working_days_min, working_days_max)

## loss of work, corrected
loss_workingtime <- runif(n, loss_workingtime_min, loss_workingtime_max)
n_days1 <- n_ncc * active * rbeta(n,unemployed_duetoepilepsy_alpha,unemployed_duetoepilepsy_beta) * working_days
n_days2 <- n_ncc * active * (1 - rbeta(n,unemployed_duetoepilepsy_alpha,unemployed_duetoepilepsy_beta)) * loss_workingtime
n_days_inactivity <- n_days1 + n_days2

## monthly salary
monthly_salary <- runif(n, monthly_salary_min, monthly_salary_max)


###
### SIMULATIONS, PIGS
###

## porcine cysticercosis prevalence
prev_pigs <- rbeta(n, prev_pigs_alpha, prev_pigs_beta)

## value of pig
price_pigs <- runif(n, price_pigs_min, price_pigs_max)

##Price reduction of pig
price_loss_pigs<-runif(n, price_loss_pigs_min,price_loss_pigs_max)

###
### SIMULATIONS

### COSTS, HUMANS
###

## hospitalization costs
cost_hosp <- stay * runif(n,price_day_hosp_min,price_day_hosp_max)

## healthcare provider costs
cost_med <- (n_visit_med * price_med * n_medheal)

## traditional healer costs (NA)


## medication costs

cost_medicine <- (n_Carb*price_carba) +(n_PhB*price_PhB) +(n_PhT* price_PhT)+(n_Valproate*price_Valproate)

## productivity losses
cost_inactivity <- ((n_days_inactivity * monthly_salary) / n_working_day_by_month)


###
### COSTS, PIGS
###

## number of infected pigs
n_pigs_infected <- n_pigs_smallscale * prev_pigs

## losses due to porcine cysticercosis
cost_pigs <-n_pigs_smallscale * pigs_sold * price_loss_pigs * prev_pigs * price_pigs

## Animal loss equivalents (ALE)
ALE<-usd_cost_pigs/GNI

###
### TOTAL COSTS AND CONVERSIONS
###


cost_total <- cost_hosp + cost_med + cost_inactivity + cost_medicine +
  cost_pigs

cost_ncc <- cost_total - cost_pigs

cost_by_ncc <- cost_ncc / n_ncc

usd_cost_total <- cost_total / bif2usd
usd_cost_ncc <- cost_ncc / bif2usd
usd_cost_by_ncc <- cost_by_ncc / bif2usd

usd_cost_hosp <- cost_hosp / bif2usd
usd_cost_med <- cost_med / bif2usd
usd_cost_inactivity <- cost_inactivity / bif2usd
usd_cost_medicine <- cost_medicine / bif2usd
usd_cost_pigs <- cost_pigs / bif2usd


###
### SUMMARIES
###

Summary <-
  function(x) {
    print(c(mean = mean(x),
            quantile(x, c(.025, .975))))
  }

Summary(cost_total)
Summary(cost_ncc)
Summary(cost_pigs)

Summary(usd_cost_total)
Summary(usd_cost_ncc)
Summary(usd_cost_pigs)

Summary(cost_by_ncc)
Summary(usd_cost_by_ncc)

Summary(usd_cost_hosp)
Summary(usd_cost_med)
Summary(usd_cost_medicine)
Summary(usd_cost_inactivity)

Summary(n_ncc)
Summary(n_pigs_infected)
Summary(n_pigs_infected/n_pigs_smallscale)

Summary(cost_hosp / cost_ncc)
Summary(cost_med / cost_ncc)
Summary(cost_medicine / cost_ncc)
Summary(cost_inactivity / cost_ncc)
Summary((cost_hosp + cost_med + cost_medicine)/cost_ncc)

Summary(cost_pigs / cost_total)
Summary(cost_inactivity / cost_total)
Summary((cost_hosp + cost_med + cost_medicine) /
          cost_total)

Summary(cost_hosp / cost_total)
Summary(cost_med / cost_total)
Summary(cost_medicine / cost_total)

Summary(usd_cost_ncc/usd_cost_total)
Summary(ALE)

## ALE Per 1000 persons years
Summary(ALE*1000/pop)





