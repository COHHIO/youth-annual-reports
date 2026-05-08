process_education_data <- function(dm, hoh_and_or_adult) {
    processed <- dm$education |>
        dplyr::select(
            enrollment_id,
            personal_id,
            organization_id,
            period,
            data_collection_stage,
            last_grade_completed,
            school_status
        ) |>
        dplyr::semi_join(hoh_and_or_adult, by = c("personal_id", "organization_id", "period")) |>
        dplyr::mutate(
            # Bucket Last Grade Completed categories
            last_grade_completed_grouped = last_grade_completed |>
                merge_missing_responses() |>
                factor(
                    levels = c(
                        "Vocational Degree",
                        "Graduate Degree",
                        "Bachelor's Degree",
                        "Associate's Degree",
                        "Some College",
                        "GED",
                        "Grades 12 / High school diploma",
                        "Grade 12 / High school diploma",
                        "Grades 9-11",
                        "Grades 7-8",
                        "Grades 5-6",
                        "Less than Grade 5",
                        "School program does not have grade levels",
                        "Client doesn't know",
                        "Client prefers not to answer",
                        "Client refused",
                        "Data not collected"
                    ),
                    labels = c(
                        "College Degree/Vocational",
                        "College Degree/Vocational",
                        "College Degree/Vocational",
                        "College Degree/Vocational",
                        "Some College",
                        "High school diploma/GED",
                        "High school diploma/GED",
                        "High school diploma/GED",
                        "Grades 9-11",
                        "Grades 5-8",
                        "Grades 5-8",
                        "Less than Grade 5",
                        "Data not collected",
                        "Data not collected",
                        "Data not collected",
                        "Data not collected",
                        "Data not collected"
                    ),
                    ordered = TRUE
                ),
            school_status = school_status |>
                merge_missing_responses() |>
                factor(
                    levels = c(
                        "Obtained GED",
                        "Graduated from highschool",
                        "Graduated from high school",
                        "Attending school regularly",
                        "Attending school irregularly",
                        "Suspended",
                        "Expelled",
                        "Dropped out",
                        "Client doesn't know",
                        "Client prefers not to answer",
                        "Client refused",
                        "Data not collected"
                    ),
                    labels = c(
                        "Obtained GED",
                        "Graduated from high school",
                        "Graduated from high school",
                        "Attending school regularly",
                        "Attending school irregularly",
                        "Suspended",
                        "Expelled",
                        "Dropped out",
                        "Data not collected",
                        "Data not collected",
                        "Data not collected",
                        "Data not collected"
                    )
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
