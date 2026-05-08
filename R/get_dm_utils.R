#' Create a table of served periods based on report year cycles (July 1 - June 30)
#'
#' @param start_year Numeric. The first year to include (e.g., 2020 for period 20-21)
#' @param end_year Numeric. The last year to include
#' @return A data frame with period, period_start, and period_end
create_served_periods <- function(start_year, end_year) {
    # Validate inputs
    if (start_year > end_year) {
        stop("start_year must be less than or equal to end_year")
    }

    # Generate sequence of report years
    report_years <- start_year:end_year

    # Create the periods table
    periods <- data.frame(
        period = sprintf("%04d-%04d", report_years, (report_years + 1)),
        period_start = as.Date(paste0(report_years, "-07-01")),
        period_end = as.Date(paste0(report_years + 1, "-06-30"))
    )

    return(periods)
}

#' Expand enrollment data by served periods with period-adjusted dates
#'
#' @param enrollment_data Data frame with columns: enrollment_id, entry_date, exit_date
#' @param periods_table Data frame created by create_served_periods()
#' @param period_source_map Named list mapping period labels to the authoritative
#'   source for that period (e.g., list("2021-2022" = "ryha", "2022-2023" = "ryha2024"))
#' @return Expanded data frame with one row per enrollment per period served
expand_enrollments_by_period <- function(
    enrollment_data,
    periods_table,
    period_source_map
) {
    # Build a lookup data frame from the period -> source mapping
    source_lookup <- tibble::tibble(
        period = names(period_source_map),
        source = unlist(period_source_map)
    )

    # Cross join enrollments with all periods
    expanded <- enrollment_data |>
        tidyr::crossing(periods_table)

    # Filter to keep only periods where enrollment was active
    # An enrollment is active in a period if:
    # - Entry date is before or on the period end date
    # - Exit date is after or on the period start date (or is NA/ongoing)
    filtered <- expanded |>
        dplyr::filter(
            entry_date <= period_end &
                (is.na(exit_date) | exit_date >= period_start)
        ) |>
        # Keep only rows whose source is authoritative for that period
        dplyr::semi_join(source_lookup, by = c("period", "source")) |>
        # Adjust dates to reflect what was true during each period
        dplyr::mutate(
            # Exit date for this period:
            # - NA if exit happened after period ended or never happened
            # - Otherwise, the earlier of actual exit or period end
            period_exit_date = dplyr::case_when(
                is.na(exit_date) ~ as.Date(NA), # No exit yet
                exit_date > period_end ~ as.Date(NA), # Exit after period ended
                TRUE ~ pmin(exit_date, period_end) # Exit during period
            ),
            exited_in_period = dplyr::case_when(
                is.na(period_exit_date) ~ "No",
                TRUE ~ "Yes"
            )
        ) |>
        dplyr::arrange(enrollment_id, period_start)

    return(filtered)
}
