counts_per_period <- function(data) {
    data |>
        dplyr::count(period, name = "N")
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

