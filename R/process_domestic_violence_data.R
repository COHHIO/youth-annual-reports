process_domestic_violence_data <- function(dm, hoh_and_or_adult) {
    yes_no_cols <- c(
        "domestic_violence_victim",
        "currently_fleeing"
    )

    processed <- dm$domestic_violence |>
        dplyr::mutate(
            domestic_violence_victim = dplyr::coalesce(
                domestic_violence_victim,
                domestic_violence_survivor
            )
        ) |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            data_collection_stage,
            domestic_violence_victim,
            when_occurred,
            currently_fleeing
        ) |>
        dplyr::semi_join(hoh_and_or_adult, by = c("personal_id", "organization_id", "period")) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(yes_no_cols),
                ~ recode_factor(.x, levels = c("Yes", "No", "Data not collected"))
            )
        )

    processed |>
        dplyr::filter(data_collection_stage == "Project start")
}
