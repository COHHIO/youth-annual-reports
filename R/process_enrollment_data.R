process_enrollment_data <- function(dm, hoh_and_or_adult) {
    yes_no_cols <- c(
        "former_ward_child_welfare",
        "former_ward_juvenile_justice"
    )

    processed <- dm$enrollment |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            living_situation,
            referral_source,
            former_ward_child_welfare,
            former_ward_juvenile_justice
        ) |>
        # Bucket Living Situation categories
        dplyr::left_join(
            get_living_codes() |>
                dplyr::select(
                    description = Description,
                    living_situation_grouped = ExitCategory
                ),
            by = c("living_situation" = "description")
        ) |>
        dplyr::mutate(
            living_situation_grouped = recode_factor(
                living_situation_grouped,
                levels = c(
                    "Permanent",
                    "Temporary",
                    "Institutional",
                    "Homeless",
                    "Other",
                    "Data not collected"
                )
            ),
            referral_source = merge_missing_responses(referral_source)
        ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(yes_no_cols),
                ~ recode_factor(.x, levels = c("Yes", "No", "Data not collected"))
            )
        ) |>
        dplyr::semi_join(hoh_and_or_adult, by = c("personal_id", "organization_id", "period"))
}
