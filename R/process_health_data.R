process_health_data <- function(dm, hoh_and_or_adult) {
    processed <- dm$health |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            data_collection_stage,
            general_health_status,
            dental_health_status,
            mental_health_status,
            pregnancy_status
        ) |>
        dplyr::semi_join(hoh_and_or_adult, by = c("personal_id", "organization_id", "period")) |>
        dplyr::mutate(
            dplyr::across(
                c(general_health_status, dental_health_status, mental_health_status),
                ~ recode_factor(.x, levels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Data not collected"))
            ),
            dplyr::across(
                c(pregnancy_status),
                ~ recode_factor(.x, levels = c("Yes", "No", "Data not collected"))
            )
        )

    start <- processed |>
        dplyr::filter(data_collection_stage == "Project start")

    exit <- processed |>
        dplyr::filter(data_collection_stage == "Project exit")

    start_exit <- dplyr::inner_join(
        start,
        exit,
        suffix = c("_start", "_exit"),
        by = c(
            "enrollment_id",
            "personal_id",
            "organization_id",
            "period"
        )
    )

    list(
        start = start,
        exit = exit,
        start_exit = start_exit
    )
}
