# TODO: 2025-2026 uses hispanic_latinao column
process_ethnicity_data <- function(dm) {
    dm$client |>
        dplyr::select(
            personal_id,
            organization_id,
            period,
            am_ind_ak_native,
            asian,
            black_af_american,
            native_hi_pacific,
            white,
            race_none,
            hispanic_latinaox,
            hispanic_latinaeo,
            mid_east_n_african
        ) |>
        tidyr::pivot_longer(cols = -c(personal_id, organization_id, period)) |>
        dplyr::filter(value == "Yes") |>
        dplyr::filter(name != "race_none") |>
        dplyr::mutate(
            ethnicity = dplyr::case_when(
                name == "am_ind_ak_native" ~ "American Indian, Alaska Native, or Indigenous",
                name == "asian" ~ "Asian or Asian American",
                name == "black_af_american" ~ "Black, African American, or African",
                name == "native_hi_pacific" ~ "Native Hawaiian or Pacific Islander",
                name == "white" ~ "White",
                name %in% c("hispanic_latinaox", "hispanic_latinaeo") ~ "Hispanic/Latina/o",
                name == "mid_east_n_african" ~ "Middle Eastern or North African"
            )
        )
}
