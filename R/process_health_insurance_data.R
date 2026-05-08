process_health_insurance_data <- function(dm) {
    yes_no_cols <- c(
        "insurance_from_any_source",
        "medicaid",
        "medicare",
        "schip",
        "va_medical_services",
        "employer_provided",
        "cobra",
        "private_pay",
        "state_health_ins",
        "indian_health_services",
        "other_insurance"
    )

    processed <- dm$benefits |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            data_collection_stage,
            insurance_from_any_source,
            medicaid,
            medicare,
            schip,
            va_medical_services,
            employer_provided,
            cobra,
            private_pay,
            state_health_ins,
            indian_health_services,
            other_insurance
        ) |>
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
