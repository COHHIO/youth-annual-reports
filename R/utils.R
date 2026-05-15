counts_per_period <- function(data) {
    data |>
        dplyr::count(period, name = "N")
}

count_by_period <- function(data, col, clients_per_period) {
    col <- rlang::ensym(col)
    data |>
        dplyr::count(period, !!col) |>
        dplyr::left_join(clients_per_period, by = "period") |>
        dplyr::mutate(value = sprintf("%s (%.1f%%)", formatC(n, format = "d", big.mark = ","), n / N * 100)) |>
        dplyr::select(period, !!col, value) |>
        tidyr::pivot_wider(names_from = period, values_from = value, values_fill = "0 (0.0%)") |>
        dplyr::arrange(!!col)
}

make_start_exit_table <- function(start_data, exit_data, var, label = var) {
    start_tbl <- prep_tbl(start_data, var) |> dplyr::mutate(stage = "Start")
    exit_tbl <- prep_tbl(exit_data, var) |> dplyr::mutate(stage = "Exit")

    periods <- sort(unique(start_data$period))
    col_order <- as.character(c(rbind(
        paste(periods, "Start", sep = "_"),
        paste(periods, "Exit", sep = "_")
    )))

    col <- start_data[[var]]
    lvls <- c(if (is.factor(col)) levels(col) else sort(unique(col)), "Total")

    dplyr::bind_rows(start_tbl, exit_tbl) |>
        dplyr::mutate(
            col_name = paste(period, stage, sep = "_")
        ) |>
        dplyr::select(value, col_name, cell) |>
        tidyr::pivot_wider(names_from = col_name, values_from = cell, values_fill = "") |>
        dplyr::mutate(value = factor(value, levels = lvls)) |>
        dplyr::arrange(value) |>
        dplyr::mutate(value = as.character(value)) |>
        dplyr::select(value, dplyr::any_of(col_order)) |>
        gt::gt(rowname_col = "value") |>
        gt::tab_spanner_delim(delim = "_") |>
        gt::tab_stubhead(label = label) |>
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
                gt::cells_column_labels(),
                gt::cells_stub(rows = "Total"),
                gt::cells_body(rows = value == "Total")
            )
        ) |>
        gt::cols_width(
            gt::stub() ~ gt::pct(20),
            dplyr::everything() ~ gt::pct(10)
        )
}

merge_missing_responses <- function(x) {
    dplyr::case_when(
        x == "Client doesn't know" ~ "Data not collected",
        x == "Client doesn’t know" ~ "Data not collected",
        x == "Client prefers not to answer" ~ "Data not collected",
        x == "Client refused" ~ "Data not collected",
        x == "Worker does not know" ~ "Data not collected",
        is.na(x) ~ "Data not collected",
        TRUE ~ x
    )
}

recode_factor <- function(x, levels) {
    merge_missing_responses(x) |>
        factor(levels = levels, ordered = TRUE)
}

make_period_table <- function(data, col, label = col) {
    col_vals <- data[[col]]
    lvls <- c(if (is.factor(col_vals)) levels(col_vals) else sort(unique(col_vals[!is.na(col_vals)])), "Total")
    periods <- sort(unique(data$period))

    prep_tbl(data, col) |>
        tidyr::pivot_wider(names_from = period, values_from = cell, values_fill = "") |>
        dplyr::mutate(value = factor(value, levels = lvls)) |>
        dplyr::arrange(value) |>
        dplyr::mutate(value = as.character(value)) |>
        dplyr::select(value, dplyr::any_of(as.character(periods))) |>
        gt::gt(rowname_col = "value") |>
        gt::tab_stubhead(label = label) |>
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
                gt::cells_column_labels(),
                gt::cells_stub(rows = "Total"),
                gt::cells_body(rows = value == "Total")
            )
        ) |>
        gt::cols_width(
            gt::stub() ~ gt::pct(20),
            dplyr::everything() ~ gt::pct(10)
        )
}

fmt_cell <- function(n, total) sprintf("%s (%.1f%%)", formatC(n, format = "d", big.mark = ","), n / total * 100)

prep_tbl <- function(df, col) {
    df <- df |>
        dplyr::mutate(value = .data[[col]]) |>
        dplyr::filter(!is.na(value))

    counts <- df |>
        dplyr::count(period, value) |>
        dplyr::group_by(period) |>
        dplyr::mutate(cell = fmt_cell(n, sum(n))) |>
        dplyr::ungroup()

    totals <- df |>
        dplyr::count(period, name = "n") |>
        dplyr::mutate(value = "Total", cell = fmt_cell(n, n))

    dplyr::bind_rows(counts, totals) |> dplyr::select(period, value, cell)
}
