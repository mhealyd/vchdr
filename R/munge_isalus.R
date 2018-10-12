.readAppointments <- function () {

  appointments <- readr::read_csv("phi/unprocessed/appointments.csv",
                                  col_type = readr::cols()) %>%
    dplyr::rename(last = 1, first = 2, chart = 6, visit_date = 7,
           visitor = 10, status = 11, type = 12) %>%
    tidyr::unite(client, last, first, sep = ", ") %>%
    tidyr::separate(visitor, c("first", "last"), sep = " ",
                    extra = "drop", fill = "right") %>%
    tidyr::unite("visitor", c("last", "first"), sep = ", ") %>%
    dplyr::mutate(visit_date = lubridate::as_date(lubridate::mdy_hm(visit_date)),
                  positive = dplyr::case_when(
                    type == "+ Pregnancy Test" ~ 1,
                    type == "- Pregnancy Test" ~ 2
                    ),
                  type = dplyr::case_when(
                    type == "+ Pregnancy Test" ~ "pregnancy test",
                    type == "- Pregnancy Test" ~ "pregnancy test",
                    type == "Office Visit" ~ "office visit",
                    type == "Pre to 3 Intake" ~ "home visit",
                    type == "Pre-3 CHW Home Visit" ~ "home visit",
                    type == "Pre-3 RN Home Visit" ~ "home visit",
                    type == "Pre-3 RN Quarterly Home Visit" ~ "status visit",
                    type == "Pre-3 RN Sub for CHW Home Visit" ~ "sub visit",
                    type == "RN Support Visit" ~ "RN visit",
                    type == "SW Home Visit" ~ "SW visit",
                    type == "SW Office Visit" ~ "SW visit"
                  ),
                  status = dplyr::case_when(
                    status == "Scheduled" ~ "scheduled",
                    status == "Confirmed" ~ "confirmed",
                    status == "No-Show" ~ "client cancellation",
                    status == "Checked-In" ~ "checked in",
                    status == "Checked-Out" ~ "checked out",
                    status == "Cancelled" ~ "visitor cancellation",
                    status == "Not Home" ~ "client cancellation",
                    status == "Reminder Sent" ~ "reminder sent",
                    status == "Arrived" ~ "arrived",
                    status == "Charting Complete" ~ "complete"
                  ),
                  visitor = replace(visitor,
                                    visitor == "Testing, Pregnancy",
                                    "Pregnancy Testing"),
                  chart = stringr::str_pad(as.character(chart), 5, "left", "0")) %>%
    dplyr::select(visit_date, visitor, chart, client, type, status, positive) %>%
    dplyr::filter(visitor %in% c("Pregnancy Testing", "Murphy, Felicia",
                                 "French, Sophie", "Harness, Nancy",
                                 "Bond, Adrienne", "Mooney, Tosha",
                                 "Christian, Mychelle", "Willis, Shamirah",
                                 "Goines, Tamika", "Mosson, Jaleene",
                                 "Chandler, Maggie"),
                  !is.na(type))

  save(appointments, file = "phi/processed/appointments.rda")

}

.readCaseManagement <- function() {

  case_management <- readr::read_csv("phi/unprocessed/case_management.csv",
                                     col_type = readr::cols()) %>%
    dplyr::filter(Program == "Pre to 3") %>%
    dplyr::select(2:4, 6:9, 14:16) %>%
    dplyr::rename(intake_date = 1, chart = 2, client = 3, visitor = 4,
                  status = 5, discharge_date = 6, discharge_reason = 7,
                  referral_source = 8, referral_date = 9,
                  first_contact_date = 10) %>%
    dplyr::mutate(chart = as.character(chart),
                  chart = stringr::str_pad(as.character(chart), 5, "left", "0")) %>%
    dplyr::select(visitor, chart, client, status, referral_source,
                  referral_date, first_contact_date, intake_date,
                  discharge_date, discharge_reason) %>%
    dplyr::mutate_at(dplyr::vars(6:9), dplyr::funs(lubridate::mdy)) %>%
    dplyr::arrange(visitor, status, client)

  save(case_management, file = "phi/processed/case_management.rda")

}

.readDemographics <- function() {

  demographics <- readr::read_csv("phi/unprocessed/demographics.csv",
                                  col_type = readr::cols()) %>%
    dplyr::rename(last = 1, first = 2, chart = 6, street = 11,
           sex = 24, marital_status = 25) %>%
    tidyr::unite(client, last, first, sep = ", ") %>%
    dplyr::mutate(address = stringr::str_c(toupper(street),
                                           stringr::str_c(toupper(city),
                                                          toupper(state), zip, sep = " "),
                                           sep = ", "),
                  birth_date = lubridate::mdy(birth_date),
                  client = replace(client, client == "NA, NA", NA),
                  race = dplyr::case_when(
                    race == "White" ~ 4,
                    race == "Native Hawaiian or Other Pacific Islander" ~ 2,
                    race == "Black or African American" ~ 3,
                    race == "Asian" ~ 2,
                    race == "American Indian or Alaska Native" ~ 1,
                    stringr::str_detect(race, ",") == TRUE ~ 7,
                    ethnicity == "Hispanic or Latino" ~ 5,
                    TRUE ~ 6
                  ),
                  chart = as.character(chart),
                  chart = stringr::str_pad(as.character(chart), 5, "left", "0"),
                  active = ifelse(active == "No", 0, 1),
                  sex = ifelse(sex == "Unknown", NA, tolower(sex)),
                  marital_status = ifelse(marital_status == "Legally Separated",
                                          "separated", ifelse(marital_status == "Unknown",
                                                              NA, tolower(marital_status)))) %>%
    dplyr::select(chart, client, active, address, zip,
                  birth_date, sex, race, marital_status)

  save(demographics, file = "phi/processed/demographics.rda")

}

.readReferrals <- function () {

  referrals <- readr::read_csv("phi/unprocessed/referrals.csv",
                               col_names = FALSE, col_types = readr::cols(), skip = 1) %>%
    dplyr::select(date = 1, chart = 2, last = 3, first = 4,
                  source = 5, other = 6) %>%
    dplyr::mutate(date = lubridate::mdy(date),
                  chart = stringr::str_pad(chart, 5, "left", "0"),
                  period = ifelse(date < lubridate::date("2018-07-25"), 0, 1),
                  source = dplyr::case_when(
                    source == "Birthright" ~ "Other",
                    source == "BNE" ~ "Other",
                    source == "Deaconness" ~ "Other",
                    source == "ECHO" ~ "Other",
                    source == "ECLC" ~ "Other",
                    source == "LL" ~ "Other",
                    source == "SF" ~ "Other",
                    TRUE ~ source
                  ))

  save(referrals, file = "phi/processed/referrals.rda")

}
