process_benefits_data <- function(dm, hoh_and_or_adult) {
    yes_no_cols <- c(
        "benefits_from_any_source",
        "snap",
        "wic",
        "tanf_child_care",
        "tanf_transportation",
        "other_tanf",
        "other_benefits_source"
    )

    processed <- dm$benefits |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            data_collection_stage,
            benefits_from_any_source,
            snap,
            wic,
            tanf_child_care,
            tanf_transportation,
            other_tanf,
            other_benefits_source
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
