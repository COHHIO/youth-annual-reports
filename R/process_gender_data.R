# TODO: 2025-2026 includes sex column (to be integrated to Man / Woman)
process_gender_data <- function(dm) {
    dm$client |>
        dplyr::select(
            personal_id,
            organization_id,
            period,
            female,
            male,
            no_single_gender,
            transgender,
            questioning,
            gender_none,
            woman,
            man,
            non_binary,
            culturally_specific,
            different_identity
        ) |>
        tidyr::pivot_longer(cols = -c(personal_id, organization_id, period)) |>
        dplyr::filter(value == "Yes") |>
        dplyr::filter(name != "gender_none") |>
        dplyr::mutate(
            gender = dplyr::case_when(
                name == "transgender" ~ "Transgender",
                name == "questioning" ~ "Questioning",
                name %in% c("non_binary", "no_single_gender") ~ "Non Binary",
                name == "culturally_specific" ~ "Culturally Specific",
                name %in% c("female", "woman") ~ "Woman",
                name %in% c("male", "man") ~ "Man",
                name == "different_identity" ~ "Different Identity"
            ),
            gender = factor(
                gender,
                levels = c(
                    "Man",
                    "Woman",
                    "Non Binary",
                    "Transgender",
                    "Questioning",
                    "Culturally Specific",
                    "Different Identity"
                ),
                ordered = TRUE
            )
        )
}
