p3_read <- function(review = TRUE, load = FALSE) {

  .readP3001()

  .readP3002()

  .readP3003()

  .readP3004a()

  .readP3004b()

  .readAppointments()

  .readCaseManagement()

  .readDemographics()

  .readReferrals()

  if (review == TRUE) {

    load("phi/temp/P3001.rda", .GlobalEnv)
    load("phi/temp/P3002.rda", .GlobalEnv)
    load("phi/temp/P3003.rda", .GlobalEnv)
    load("phi/temp/P3004a.rda", .GlobalEnv)
    load("phi/temp/P3004b.rda", .GlobalEnv)

  } else {

    message("Intake and status data saved to //phi/temp/")

  }

  if (load == TRUE) {

    load("phi/processed/appointments.rda", .GlobalEnv)
    load("phi/processed/case_management.rda", .GlobalEnv)
    load("phi/processed/demographics.rda", .GlobalEnv)
    load("phi/processed/referrals.rda", .GlobalEnv)

  } else {

    message("Client EMR data saved to //phi/processed/")

  }

}
