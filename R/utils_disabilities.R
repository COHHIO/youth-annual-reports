make_disabilites_start_exit_table <- function(dm, enrollment_count_per_period, exit_count_per_period) {
    counts <- dm$disabilities |>
        dplyr::mutate(
            disability_response = dplyr::replace_values(
                disability_response,
                "Alcohol use disorder" ~ "Yes",
                "Both alcohol and drug use disorders" ~ "Yes",
                "Drug use disorder" ~ "Yes"
            )
        ) |>
        dplyr::filter(disability_response == "Yes") |>
        dplyr::count(disability_type, data_collection_stage, period)

    start_data <- counts |>
        dplyr::filter(data_collection_stage == "Project start") |>
        dplyr::left_join(enrollment_count_per_period, by = "period") |>
        dplyr::mutate(stage = "Start", cell = fmt_cell(n, N))

    exit_data <- counts |>
        dplyr::filter(data_collection_stage == "Project exit") |>
        dplyr::left_join(exit_count_per_period, by = "period") |>
        dplyr::mutate(stage = "Exit", cell = fmt_cell(n, N))

    periods <- sort(unique(counts$period))
    col_order <- as.character(c(rbind(
        paste(periods, "Start", sep = "_"),
        paste(periods, "Exit", sep = "_")
    )))

    dplyr::bind_rows(start_data, exit_data) |>
        dplyr::mutate(col_name = paste(period, stage, sep = "_")) |>
        dplyr::select(disability_type, col_name, cell) |>
        tidyr::pivot_wider(names_from = col_name, values_from = cell, values_fill = "0 (0.0%)") |>
        dplyr::arrange(disability_type) |>
        dplyr::select(disability_type, dplyr::any_of(col_order)) |>
        gt::gt(rowname_col = "disability_type") |>
        gt::tab_spanner_delim(delim = "_") |>
        gt::tab_stubhead(label = "Condition") |>
        gt::cols_align(align = "right") |>
        gt::tab_style(
            style = gt::cell_text(align = "center"),
            locations = list(gt::cells_stubhead(), gt::cells_column_labels())
        ) |>
        gt::tab_style(
            style = gt::cell_text(align = "left"),
            locations = gt::cells_stub()
        ) |>
        gt::tab_style(
            style = gt::cell_borders(sides = "right", color = "black", weight = gt::px(1.5)),
            locations = list(gt::cells_stubhead(), gt::cells_stub())
        ) |>
        gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = list(
                gt::cells_stubhead(),
                gt::cells_column_spanners(),
                gt::cells_column_labels()
            )
        ) |>
        gt::cols_width(
            gt::stub() ~ gt::pct(20),
            dplyr::everything() ~ gt::pct(10)
        )
}
