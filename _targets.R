# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(quarto)

# Set target options:
tar_option_set(
    packages = c("tibble") # Packages that your targets need for their tasks.
    # format = "qs", # Optionally set the default storage format. qs is fast.
    #
    # Pipelines that take a long time to run may benefit from
    # optional distributed computing. To use this capability
    # in tar_make(), supply a {crew} controller
    # as discussed at https://books.ropensci.org/targets/crew.html.
    # Choose a controller that suits your needs. For example, the following
    # sets a controller that scales up to a maximum of two workers
    # which run as local R processes. Each worker launches when there is work
    # to do and exits if 60 seconds pass with no tasks to run.
    #
    #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
    #
    # Alternatively, if you want workers to run on a high-performance computing
    # cluster, select a controller from the {crew.cluster} package.
    # For the cloud, see plugin packages like {crew.aws.batch}.
    # The following example is a controller for Sun Grid Engine (SGE).
    #
    #   controller = crew.cluster::crew_controller_sge(
    #     # Number of workers that the pipeline can scale up to:
    #     workers = 10,
    #     # It is recommended to set an idle time so workers can shut themselves
    #     # down if they are not running tasks.
    #     seconds_idle = 120,
    #     # Many clusters install R as an environment module, and you can load it
    #     # with the script_lines argument. To select a specific verison of R,
    #     # you may need to include a version string, e.g. "module load R/4.3.2".
    #     # Check with your system administrator if you are unsure.
    #     script_lines = "module load R"
    #   )
    #
    # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
    tar_target(
        name = config_filepath,
        command = "_config.yml",
        format = "file"
    ),
    tar_target(
        name = config,
        command = read_config(config_filepath)
    ),
    tar_target(
        name = raw_dm,
        command = fetch_dm(config)
    ),
    tar_target(
        name = dm,
        command = build_report_dm(config, raw_dm)
    ),
    tar_target(
        name = client_count_per_period,
        command = counts_per_period(dm$client)
    ),
    tar_target(
        name = enrollment_count_per_period,
        command = counts_per_period(dm$enrollment)
    ),
    tar_target(
        name = exit_count_per_period,
        command = counts_per_period(dm$exit)
    ),
    tar_target(
        name = client,
        command = process_client_data(dm)
    ),
    tar_target(
        name = hoh_and_or_adult,
        command = process_hoh_and_or_adult_data(client)
    ),
    tar_target(
        name = gender,
        command = process_gender_data(dm)
    ),
    tar_target(
        name = ethnicity,
        command = process_ethnicity_data(dm)
    ),
    tar_target(
        name = income,
        command = process_income_data(dm, hoh_and_or_adult)
    ),
    tar_target(
        name = benefits,
        command = process_benefits_data(dm, hoh_and_or_adult)
    ),
    tar_target(
        name = health_insurance,
        command = process_health_insurance_data(dm)
    ),
    tar_target(
        name = domestic_violence,
        command = process_domestic_violence_data(dm, hoh_and_or_adult)
    ),
    tar_target(
        name = education,
        command = process_education_data(dm, hoh_and_or_adult)
    ),
    tar_target(
        name = health,
        command = process_health_data(dm, hoh_and_or_adult)
    ),
    tar_target(
        name = employment,
        command = process_employment_data(dm, hoh_and_or_adult)
    ),
    tar_target(
        name = exit,
        command = process_exit_data(dm, hoh_and_or_adult)
    ),
    # Render the report to "_output/"
    tar_quarto(
        name = report,
        path = "report.qmd",
        execute_params = list(period = "2024")
    )
)
