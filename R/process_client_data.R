process_client_data <- function(dm) {
    dm$client |>
        dplyr::left_join(
            dm$enrollment |>
                dplyr::summarise(
                    relationship_to_ho_h = dplyr::first(relationship_to_ho_h),
                    first_entry_date = min(entry_date),
                    .by = c("personal_id", "organization_id", "period")
                ),
            by = c("personal_id", "organization_id", "period")
        ) |>
        dplyr::select(
            personal_id,
            organization_id,
            period,
            dob,
            first_entry_date,
            relationship_to_ho_h
        ) |>
        dplyr::mutate(
            age = lubridate::time_length(
                difftime(first_entry_date, dob),
                "years"
            ) |>
                floor(),
            age_grouped = dplyr::case_when(
                age >= 25 ~ "25+",
                age >= 18 & age <= 24 ~ "18-24",
                age >= 14 & age <= 17 ~ "14-17",
                age >= 6 & age <= 13 ~ "6-13",
                age >= 0 & age <= 5 ~ "0-5",
                TRUE ~ "Data not collected"
            ),
            age_grouped = factor(
                age_grouped,
                levels = c(
                    "Data not collected",
                    "0-5",
                    "6-13",
                    "14-17",
                    "18-24",
                    "25+"
                )
            ),
            is_hoh = ifelse(relationship_to_ho_h == "Self (head of household)", TRUE, FALSE),
            is_adult = ifelse(age >= 18, TRUE, FALSE),
            is_hoh_and_or_adult = is_hoh | is_adult
        )
}
