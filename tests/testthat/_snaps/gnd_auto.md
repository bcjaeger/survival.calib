# printed output has not changed

    Code
      calib_test_gnd(predicted_risk = risk_pred, event_time = flchain$futime[
        -train_index], event_status = flchain$death[-train_index], time_predict = risk_times,
      group_count_init = 50, verbose = 0)
    Output
      -------------------------------------------------------
      
      - Greenwood-Nam-D'Agostino (GND) Test at t = 4000:
      
      -- Chi-square statistic: 29.434
      -- degrees of freedom: 39
      -- P-value for miscalibration: .87
      -- Warnings: None
      
      -------------------------------------------------------
      
      - Events data by group at t = 4000 (truncated):
      
      # A tibble: 5 x 4
        group_label group_n percent_expected percent_observed
              <dbl>   <dbl>            <dbl>            <dbl>
      1           1      98           0.0234           0.0656
      2           4     390           0.0305           0.0242
      3           6     195           0.0362           0.0568
      4           8     292           0.0403           0.0437
      5          12     195           0.0444           0.0460

