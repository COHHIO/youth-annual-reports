read_config <- function(config_filepath) {
    yaml::read_yaml(config_filepath)
}

fetch_dm <- function(config) {
    sources <- unique(unlist(config$period_source_map))

    connections <- setNames(
        lapply(sources, function(src) {
            DBI::dbConnect(
                drv = RPostgres::Postgres(),
                dbname = src,
                host = Sys.getenv("DB_HOST"),
                port = Sys.getenv("DB_PORT"),
                user = Sys.getenv("DB_USER"),
                password = Sys.getenv("DB_PWD")
            )
        }),
        sources
    )

    all_tables <- unique(unlist(lapply(connections, DBI::dbListTables)))

    dm <- lapply(setNames(all_tables, all_tables), function(tbl) {
        tables_per_conn <- lapply(names(connections), function(src) {
            message("Reading ", src, " ", tbl)

            conn <- connections[[src]]
            if (!DBI::dbExistsTable(conn, tbl)) {
                return(NULL)
            }
            df <- DBI::dbReadTable(conn, tbl)
            df$source <- src
            df
        })
        dplyr::bind_rows(tables_per_conn) |>
            tibble::as_tibble()
    })
}

build_report_dm <- function(config, raw_dm) {
    # Enrich tables in dm with project and organization info
    project_lookup <- raw_dm$project |>
        dplyr::select(project_id, source, project_name) |>
        dplyr::mutate(
            project_name = project_name |>
                dplyr::replace_values(
                    "zzFairfield - Lancaster Fairfield CAA - ODH Youth RRH" ~ "Fairfield - Lancaster Fairfield CAA - ODH Youth RRH",
                    "ODH RRH (Family Promise)" ~ "Family Promise ODH RRH",
                    "Lavender Landing LGBTQ Youth HP ODH (CANAPI)" ~ "Lavender Landing LGBTQ Youth ODH HP",
                    "Lavender Landing LGBTQ Youth ODH HP (CANAPI)" ~ "Lavender Landing LGBTQ Youth ODH HP",
                    "ODH Central Intake HP" ~ "Central Intake ODH HP",
                )
        )

    organization_lookup <- raw_dm$organization |>
        dplyr::select(organization_id, source, organization_name) |>
        dplyr::mutate(
            organization_name = organization_name |>
                dplyr::replace_values(
                    "Community Action Program Commission of the Lancast" ~ "Community Action Program Commission of the Lancaster Fairfield County Area",
                    "Family Promise ODH RRH" ~ "Family Promise",
                    "ODH RRH (Family Promise)" ~ "Family Promise",
                    "Harmony ODH Emergency Shelter (Harmony House)" ~ "Harmony House",
                    "Lavender Landing LGBTQ Youth HP ODH (CANAPI)" ~ "CANAPI",
                    "Lavender Landing LGBTQ Youth ODH HP (CANAPI)" ~ "CANAPI",
                    "Street Outreach Services  (Shelter Care)" ~ "Shelter Care",
                    "Street Outreach Services RHY (Shelter Care)" ~ "Shelter Care",
                    "ODH Central Intake HP" ~ "United Way of Summit Medina (UWSM)",
                    "Central Intake ODH HP (UWSM)" ~ "United Way of Summit Medina (UWSM)",
                    "Toledo Lucas County Homelessness Board / Toledo HM" ~ "Toledo Lucas County Homelessness Board / Toledo HMIS",
                    "Infant Vitality Project ODH (GJCF)" ~ "Gus Johnson Community Foundation INC",
                    "Infant Vitality Project ODH TH (GJCF)" ~ "Gus Johnson Community Foundation INC",
                    "Youth Homelessness Street Outreach ODH (CoC)" ~ "Youth Homelessness Street Outreach",
                    "Youth Homelessness Street Outreach ODH (SCCoC)" ~ "Youth Homelessness Street Outreach"
                )
        )

    dm <- lapply(raw_dm, function(tbl) {
        if (("project_id" %in% names(tbl)) && (!"project_name" %in% names(tbl))) {
            tbl <- dplyr::left_join(
                tbl,
                project_lookup,
                by = c("project_id", "source")
            )
        }
        if (
            ("organization_id" %in% names(tbl)) &&
                (!"organization_name" %in% names(tbl))
        ) {
            tbl <- dplyr::left_join(
                tbl,
                organization_lookup,
                by = c("organization_id", "source")
            )
        }
        tbl
    })

    enrollment_data <- dplyr::left_join(
        # Entry data
        x = dm$enrollment |>
            dplyr::select(
                enrollment_id,
                personal_id,
                organization_id,
                organization_name,
                entry_date,
                source
            ),
        # Exit data
        y = dm$exit |>
            dplyr::select(
                enrollment_id,
                personal_id,
                organization_id,
                organization_name,
                exit_date,
                source
            ),
        by = c(
            "enrollment_id",
            "personal_id",
            "organization_id",
            "organization_name",
            "source"
        )
    )

    period_years <- as.integer(sub("-.*", "", names(config$period_source_map)))
    periods <- create_served_periods(min(period_years), max(period_years))

    served_data <- expand_enrollments_by_period(
        enrollment_data,
        periods,
        config$period_source_map
    )

    # Filter dm tables to keep only rows represented in served_data
    report_dm <- lapply(dm, function(tbl) {
        if ("enrollment_id" %in% names(tbl)) {
            served_lookup <- served_data |>
                dplyr::select(
                    source,
                    personal_id,
                    organization_id,
                    enrollment_id,
                    period,
                    exited_in_period
                )

            # Keep only rows that match a served enrollment
            result <- dplyr::inner_join(
                tbl,
                served_lookup,
                by = c("source", "personal_id", "organization_id", "enrollment_id"),
                relationship = "many-to-many"
            )

            if ("data_collection_stage" %in% names(tbl)) {
                result <- result |>
                    dplyr::filter(
                        data_collection_stage %in%
                            c("Project start", "Project exit")
                    ) |>
                    dplyr::filter_out(
                        dplyr::when_all(
                            data_collection_stage == "Project exit",
                            exited_in_period == "No"
                        )
                    )
            }

            if ("exit_date" %in% names(tbl)) {
                result <- result |>
                    dplyr::filter(exited_in_period == "Yes")
            }

            result
        } else if (all(c("personal_id", "organization_id") %in% names(tbl))) {
            served_lookup <- served_data |>
                dplyr::summarise(
                    n_enrollments = dplyr::n(),
                    .by = c(personal_id, organization_id, source, period)
                )

            result <- dplyr::inner_join(
                tbl,
                served_lookup,
                by = c("source", "personal_id", "organization_id"),
                relationship = "many-to-many"
            )

            result
        } else {
            tbl
        }
    })
}
