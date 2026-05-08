process_hoh_and_or_adult_data <- function(client) {
    client |>
        dplyr::filter(is_hoh_and_or_adult == TRUE) |>
        dplyr::select(personal_id, organization_id, period)
}
