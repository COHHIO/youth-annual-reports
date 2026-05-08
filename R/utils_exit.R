make_safe_exit_table <- function(exit_data) {
    relabel <- function(x) {
        dplyr::if_else(as.character(x) == "Data not collected", "NR*", as.character(x))
    }

    response_levels <- c("Yes", "No", "NR*")
    periods <- sort(unique(exit_data$period))
    col_order <- c(outer(response_levels, periods, function(r, p) paste(p, r, sep = "_")))

    period_totals <- exit_data |>
        dplyr::count(period, name = "N")

    tbl <- exit_data |>
        dplyr::mutate(
            worker = relabel(destination_safe_worker),
            client = relabel(destination_safe_client)
        ) |>
        dplyr::count(period, worker, client) |>
        dplyr::left_join(period_totals, by = "period") |>
        dplyr::mutate(
            cell = fmt_cell(n, N),
            col_name = paste(period, client, sep = "_")
        ) |>
        dplyr::select(worker, col_name, cell) |>
        tidyr::pivot_wider(names_from = col_name, values_from = cell, values_fill = "0 (0.0%)") |>
        dplyr::mutate(worker = factor(worker, levels = response_levels)) |>
        dplyr::arrange(worker) |>
        dplyr::mutate(worker = as.character(worker)) |>
        dplyr::select(worker, dplyr::any_of(col_order)) |>
        gt::gt(rowname_col = "worker") |>
        gt::tab_spanner_delim(delim = "_") |>
        gt::tab_spanner(
            label = "Participant Response",
            columns = dplyr::everything(),
            level = 2
        ) |>
        gt::tab_stubhead(label = gt::md("Worker \\\nResponse")) |>
        gt::tab_footnote(footnote = "*NR: Not Reported") |>
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
        gt::tab_style(
            style = gt::cell_borders(sides = "right", color = "#bbbbbb", weight = gt::px(0.75)),
            locations = list(
                gt::cells_body(columns = dplyr::matches("_(Yes|No)$")),
                gt::cells_column_labels(columns = dplyr::matches("_(Yes|No)$"))
            )
        ) |>
        gt::tab_style(
            style = gt::cell_borders(sides = "right", color = "black", weight = gt::px(1.5)),
            locations = list(
                gt::cells_body(columns = dplyr::matches("_NR\\*$")),
                gt::cells_column_labels(columns = dplyr::matches("_NR\\*$"))
            )
        )

    tbl
}
