
suppressPackageStartupMessages(
 expr = {
  library(survival)
  library(riskRegression)
 }
)

# Original GND functions: ----
kmdec_orig = function(dec.num, dec.name, datain, adm.cens) {
 stopped = 0
 data.sub = datain[datain[, dec.name] == dec.num, ]
 if (sum(data.sub$out) > 1) {
  avsurv = survfit(Surv(tvar, out) ~ 1, data = datain[datain[, dec.name] ==
                                                       dec.num, ], error = "g")
  avsurv.est = ifelse(min(avsurv$time) <= adm.cens, avsurv$surv[avsurv$time ==
                                                                 max(avsurv$time[avsurv$time <= adm.cens])], 1)

  avsurv.stderr = ifelse(min(avsurv$time) <= adm.cens, avsurv$std.err[avsurv$time ==
                                                                       max(avsurv$time[avsurv$time <= adm.cens])], 0)
  avsurv.stderr = avsurv.stderr * avsurv.est

  avsurv.num = ifelse(min(avsurv$time) <= adm.cens, avsurv$n.risk[avsurv$time ==
                                                                   max(avsurv$time[avsurv$time <= adm.cens])], 0)

 } else {
  return(c(0, 0, 0, 0, stopped = -1))
 }

 if (sum(data.sub$out) < 5)
  stopped = 1
 c(avsurv.est, avsurv.stderr, avsurv.num, dec.num, stopped)
}#kmdec

GND.calib_orig = function(pred, tvar, out, cens.t, groups, adm.cens) {
 tvar.t = ifelse(tvar > adm.cens, adm.cens, tvar)
 out.t = ifelse(tvar > adm.cens, 0, out)

 datause = data.frame(
  pred = pred,
  tvar = tvar.t,
  out = out.t,
  count = 1,
  cens.t = cens.t,
  dec = groups
 )
 numcat = length(unique(datause$dec))
 groups = sort(unique(datause$dec))

 kmtab = matrix(unlist(lapply(
  groups, kmdec_orig, "dec", datain = datause, adm.cens
 )), ncol = 5, byrow = TRUE)

 if (any(kmtab[, 5] == -1))
  stop(
   "Stopped because at least one of the groups contains <2 events. Consider collapsing some groups."
  )
 else if (any(kmtab[, 5] == 1))
  warning(
   "At least one of the groups contains < 5 events. GND can become unstable.\
                                        (see Demler, Paynter, Cook 'Tests of Calibration and Goodness of Fit in the Survival Setting' DOI: 10.1002/sim.6428) \
                                        Consider collapsing some groups to avoid this problem."
  )

 hltab = data.frame(
  group = kmtab[, 4],
  totaln = tapply(datause$count, datause$dec, sum),
  censn = tapply(datause$cens.t, datause$dec, sum),
  numevents = tapply(datause$out, datause$dec, sum),
  expected = tapply(datause$pred, datause$dec, sum),
  kmperc = 1 - kmtab[, 1],
  kmvar = kmtab[, 2] ^ 2,
  kmnrisk = kmtab[, 3],
  expectedperc = tapply(datause$pred, datause$dec, mean)
 )

 hltab$kmnum = hltab$kmperc * hltab$totaln
 hltab$GND_component = ifelse(hltab$kmvar == 0,
                              0,
                              (hltab$kmperc - hltab$expectedperc) ^ 2 / (hltab$kmvar))

 #print(hltab[c(1, 2, 3, 4, 10, 5, 6, 9, 7, 11)], digits = 4)

 c(
  df = numcat - 1,
  chi2gw = sum(hltab$GND_component),
  pvalgw = 1 - pchisq(sum(hltab$GND_component), numcat - 1)
 )
}#GND.calib



# Set up for tests ----

# drop rows with missing values for simplicity
data_init <- na.omit(flchain)

data_init$chapter <- NULL

n_obs_total <- nrow(data_init)
n_obs_train <- round(n_obs_total * 2/3)

set.seed(32987)

train_index <- sample(n_obs_total, size = n_obs_train)

data_train <- data_init[train_index, ]
data_test <- data_init[-train_index, ]

model_1 <- coxph(Surv(futime, death) ~ .,
                 data = data_train,
                 x = TRUE)

model_2 <- update(model_1, . ~ . - flc.grp)

model_3 <- update(model_2, . ~ . -mgus)

pred_horizon = 1000

risk_times <- c(1500, 2500, 3000)

risk_pred <- predictRisk(model_1,
                         newdata = data_test,
                         times = risk_times)

#split into deciles
risk_groups <-
 apply(risk_pred, 2, predrisk_grp_prcnt, g = 10)

#calculate the GND test

orig <- bcj <- list()

for(i in seq_along(risk_times)){

 orig[[i]] <- GND.calib_orig(pred = risk_pred[, i],
                             tvar = data_test$futime,
                             out = data_test$death,
                             groups = risk_groups[, i],
                             adm.cens = risk_times[i],
                             cens.t = 4000)

 .scalib <- scalib_initiate(pred_risk = risk_pred[, i],
                            pred_horizon = risk_times[i],
                            event_status = data_test$death,
                            event_time = data_test$futime)

 .scalib$data_inputs$group <- risk_groups[, i]

 bcj[[i]] <- scalib_test_gnd_manual(
  .scalib,
  pred_risk_col = 'pred_risk',
  verbose = 0
 )

}

add_risk_grp <- function(x, r){
 x$data_inputs$group <- r
 x
}

.s <- scalib_initiate(pred_risk = risk_pred[, i],
                pred_horizon = risk_times[i],
                event_status = data_test$death,
                event_time = data_test$futime) |>
 add_risk_grp(r = risk_groups[, i])

microbenchmark::microbenchmark(
 GND.calib_orig(pred = risk_pred[, i],
                        tvar = data_test$futime,
                        out = data_test$death,
                        groups = risk_groups[, i],
                        adm.cens = risk_times[i],
                        cens.t = 4000),
   scalib_test_gnd_manual(
    .s,
    pred_risk_col = 'pred_risk',
    verbose = 0
   )
)

# Tests ----

test_that(
 "BCJ code matches original",
 code = {
  for(i in seq_along(risk_times)){
   expect_equal(as.numeric(bcj[[i]]$GND_chisq),
                as.numeric(orig[[i]]['chi2gw']),
                ignore_attr = TRUE)
   expect_equal(as.numeric(bcj[[i]]$GND_pvalue),
                as.numeric(orig[[i]]['pvalgw']),
                ignore_attr = TRUE)
  }
 }
)


preds <- lapply(
 X = list(model_1,
          model_2,
          model_3),
 FUN = predictRisk,
 newdata = data_test,
 times = pred_horizon
)

.scalib <- scalib_initiate(
 pred_risk = preds,
 pred_horizon = pred_horizon,
 event_status = data_test$death,
 event_time = data_test$futime
) %>%
 scalib_test_gnd()





