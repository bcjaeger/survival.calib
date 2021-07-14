

library(riskRegression)
library(survival)

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

train_index <- 1:2500

risk_mdl <- coxph(data = flchain[train_index, ],
                  x = TRUE,
                  formula = Surv(futime, death)~age+sex+flc.grp+lambda)

risk_times <- c(1500, 2500, 4000)

risk_pred <- predictRisk(risk_mdl, newdata = flchain[-train_index, ],
                         times = risk_times)

#split into deciles
risk_groups <-
 apply(risk_pred, 2, predrisk_grp_prcnt, g = 10)

#calculate the GND test

orig <- bcj <- list()

for(i in seq_along(risk_times)){

 orig[[i]] <- GND.calib_orig(pred = risk_pred[, i],
                             tvar = flchain$futime[-train_index],
                             out = flchain$death[-train_index],
                             groups = risk_groups[, i],
                             adm.cens = risk_times[i],
                             cens.t = 4000)

 bcj[[i]] <- calib_test_gnd_manual(predicted_risk = risk_pred[, i],
                             event_time = flchain$futime[-train_index],
                             event_status = flchain$death[-train_index],
                             group = risk_groups[, i],
                             time_predict = risk_times[i],
                             verbose = 0)

}

# Tests ----

test_that(
 "BCJ code matches original",
 code = {
  for(i in seq_along(risk_times)){
   expect_equal(as.numeric(bcj[[i]]$statistic),
                as.numeric(orig[[i]]),
                ignore_attr = TRUE)
  }
 }
)
