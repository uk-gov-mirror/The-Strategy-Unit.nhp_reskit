test_that("we can generate a plot using dummy data", {
  seed <- 87654L
  sc_data <- expect_no_error(create_demo_stepcounts_tbl(seed))
  cf_tbl <- compile_change_factor_data(
    list(step_counts = sc_data),
    "admissions",
    "ip"
  )
  make_overall_cf_plot(cf_tbl)
  # Here we have a model interaction term value of -79.1
  # When the plot is viewed at 1200x600px (default Outputs app height is 600px)
  # the grey bar is visible. Let's adjust the value and see if we can make it
  # disappear.
  mit <- "model_interaction_term"
  cf_tbl2 <- cf_tbl |>
    dplyr::mutate(
      dplyr::across("value", \(x) {
        dplyr::if_else(.data[["change_factor"]] == mit, -5, x)
      }),
      hide = .data[["total"]] - abs(.data[["value"]])
    )
  make_overall_cf_plot(cf_tbl2)

  # with a value of -5 the line is invisible to me
  cf_tbl3 <- cf_tbl |>
    dplyr::mutate(
      dplyr::across("value", \(x) {
        dplyr::if_else(.data[["change_factor"]] == mit, -25, x)
      }),
      hide = .data[["total"]] - abs(.data[["value"]])
    )
  make_overall_cf_plot(cf_tbl3)
  # with a value of -25 the line is just visible to me

  # Let's work in percentages
  max_total <- max(cf_tbl[["total"]])
  est_value <- dplyr::last(cf_tbl[["value"]]) # estimate is always on bottom row

  # we might have a situation where the estimate is negative relative to the
  # baseline, so in a chart where the baseline is not included we would need to
  # add that width on. If est_value is positive then everything is above zero
  # and we can just use max_total.
  overall_width <- max(max_total, max_total - est_value)

  # Let's set a min width as a fraction of the overall_width, and use that as
  # the basis for any adjustment to values going into the chart.
  min_width_frac <- 0.0001 # 0.01%
  min_width <- overall_width * min_width_frac

  cf_tbl2 <- cf_tbl |>
    dplyr::mutate(
      dplyr::across("value", \(x) {
        dplyr::if_else(
          .data[["change_factor"]] == mit,
          x * min_width / abs(x),
          x
        )
      }),
      dplyr::across("value", \(x) {
        dplyr::if_else(abs(x) < min_width, x * min_width / abs(x), x)
      }),
      hide = .data[["total"]] - abs(.data[["value"]])
    )
  make_overall_cf_plot(cf_tbl2)
  # In this case the min_width was ~37 so we know that we should see the line
  # as this is more than the fixed value 25 we already tested.
  # How low can we make `min_width_frac` before we lose the line visibility?

  min_width_frac <- 0.00005
  min_width <- overall_width * min_width_frac

  cf_tbl2 <- cf_tbl |>
    dplyr::mutate(
      dplyr::across("value", \(x) {
        dplyr::if_else(
          .data[["change_factor"]] == mit,
          x * min_width / abs(x),
          x
        )
      }),
      dplyr::across("value", \(x) {
        dplyr::if_else(abs(x) < min_width, x * min_width / abs(x), x)
      }),
      hide = .data[["total"]] - abs(.data[["value"]])
    )
  make_overall_cf_plot(cf_tbl2)

  # Use Matt's original data

  pqt_path <- azkit::read_azure_table(
    "modelruns",
    "https://nhpsa.table.core.windows.net/",
    filter = "dataset eq 'RGP' and scenario eq 'md-test-3-2'",
    select = "aggregated_results_path"
  ) |>
    unlist()
  con <- azkit::get_container("results")
  sc_data_md <- read_results_parquet_files(con, pqt_path, "step_counts")

  cf_tbl_md <- compile_change_factor_data(sc_data_md, "admissions", "ip")
  make_overall_cf_plot(cf_tbl_md)

  # set WLA figure to be the same size as the activity_avoidance figure
  # and see how much difference pos vs neg (yellow vs grey) makes to visibility
  wla <- "waiting_list_adjustment"
  cf_tbl_md2 <- cf_tbl_md |>
    dplyr::mutate(
      dplyr::across("value", \(x) {
        dplyr::if_else(.data[["change_factor"]] == wla, 25, x)
      }),
      # dplyr::across("value", \(x) {
      #   dplyr::if_else(abs(x) < min_width, x * min_width / abs(x), x)
      # }),
      hide = .data[["total"]] - abs(.data[["value"]])
    )
  make_overall_cf_plot(cf_tbl_md2)

  # yellow is harder to see than grey, even when it's the same value/thickness
})
