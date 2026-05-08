process_exit_data <- function(dm, hoh_and_or_adult) {
    yes_no_cols <- c(
        "exchange_for_sex",
        "asked_or_forced_to_exchange_for_sex",
        "work_place_violence_threats",
        "workplace_promise_difference",
        "coerced_to_continue_work",
        "counseling_received",
        "destination_safe_client",
        "destination_safe_worker"
    )

    processed <- dm$exit |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            project_completion_status,
            exchange_for_sex,
            count_of_exchange_for_sex,
            asked_or_forced_to_exchange_for_sex,
            work_place_violence_threats,
            workplace_promise_difference,
            coerced_to_continue_work,
            counseling_received,
            destination_safe_client,
            destination_safe_worker
        ) |>
        dplyr::semi_join(hoh_and_or_adult, by = c("personal_id", "organization_id", "period")) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(yes_no_cols),
                ~ recode_factor(.x, levels = c("Yes", "No", "Data not collected"))
            ),
            project_completion_status = project_completion_status |>
                merge_missing_responses() |>
                factor(
                    levels = c(
                        "Completed project",
                        "Client voluntarily left early",
                        "Youth voluntarily left early",
                        "Client was expelled or otherwise involuntarily discharged from project",
                        "Youth was expelled or otherwise involuntarily discharged from project",
                        "Data not collected"
                    ),
                    labels = c(
                        "Completed project",
                        "Client voluntarily left early",
                        "Client voluntarily left early",
                        "Client was expelled or otherwise involuntarily discharged from project",
                        "Client was expelled or otherwise involuntarily discharged from project",
                        "Data not collected"
                    ),
                    ordered = TRUE
                )
        )

    processed
}
