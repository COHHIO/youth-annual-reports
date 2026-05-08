process_income_data <- function(dm, hoh_and_or_adult) {
    yes_no_cols <- c(
        "income_from_any_source",
        "earned",
        "unemployment",
        "ssi",
        "ssdi",
        "va_disability_service",
        "va_disability_non_service",
        "private_disability",
        "workers_comp",
        "tanf",
        "ga",
        "soc_sec_retirement",
        "pension",
        "child_support",
        "alimony",
        "other_income_source"
    )

    processed <- dm$income |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            data_collection_stage,
            income_from_any_source,
            total_monthly_income,
            earned,
            unemployment,
            ssi,
            ssdi,
            va_disability_service,
            va_disability_non_service,
            private_disability,
            workers_comp,
            tanf,
            ga,
            soc_sec_retirement,
            pension,
            child_support,
            alimony,
            other_income_source
        ) |>
        dplyr::semi_join(hoh_and_or_adult, by = c("personal_id", "organization_id", "period")) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(yes_no_cols),
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
