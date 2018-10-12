.readP3001 <- function () {

  P3001 <- readr::read_csv("phi/unprocessed/P3001.csv",
                                            col_types = readr::cols()) %>%
    tidyr::unite("name_pcg", c("last_pcg", "first_pcg"), sep = ", ") %>%
    tidyr::unite("visitor", c("last_visitor", "first_visitor"), sep = ", ") %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::one_of(c("visit_date", "dob_pcg", "edd", "ldd",
                      "pnc_date", "folic_acid_date", "review_date"))),
      dplyr::funs(lubridate::as_date(.))) %>%
    dplyr::mutate(status = "PI-00",
                  id_pcg = stringr::str_pad(as.character(id_pcg), 5, "left", "0"),
                  age_pcg = lubridate::year(lubridate::as.period(
                    lubridate::interval(dob_pcg, visit_date))),
                  primigravida = ifelse(as.numeric(childbirths) == 0, 1, 0),
                  gest_age_mother = round(as.numeric(lubridate::as.duration(
                    lubridate::interval(edd - lubridate::weeks(40),
                                        visit_date))) / 604800, digits = 1),
                  trimester = dplyr::case_when(
                    gest_age_mother < 14 ~ 1,
                    dplyr::between(gest_age_mother, 14, 27.9) ~ 2,
                    dplyr::between(gest_age_mother, 28, 43) ~ 3
                    ),
                  race_pcg = ifelse(hispanic_pcg == 1, 5, race_pcg),
                  bmi_pre = (wt_pre_mother / ht_mother^2) * 703,
                  spacing = round(as.numeric(lubridate::as.duration(
                    lubridate::interval(ldd, edd - lubridate::weeks(40)))) / 2628288,
                    digits = 1),
                  lead_mother = ifelse(
                    lead_mother_01 == 1 | lead_mother_02 == 1 | lead_mother_03 == 1 |
                      lead_mother_04 == 1 | lead_mother_05 == 1 | lead_mother_06 == 1 |
                      lead_mother_07 == 1 | lead_mother_08 == 1 | lead_mother_09 == 1,
                    1, 0),
                  proc_days = lubridate::day(lubridate::as.period(
                    lubridate::interval(visit_date, review_date)))) %>%
    dplyr::mutate_at(dplyr::vars(snap:tanf, food_barrier_01:food_barrier_05,
                                 depression:mental_illness_othr,
                                 diabetes:chr_dz_othr, gdm:sb),
                     dplyr::funs(replace(., . == "Off", NA))) %>%
    dplyr::mutate_at(dplyr::vars(dcs, uninsured_pcg:private_pcg,
                                 dplyr::ends_with("_opt")),
                     dplyr::funs(replace(., . == "Off", 0)))

  P3001 <- P3001 %>%
    dplyr::full_join(.vars2ListCol(P3001, id_pcg,
                                   snap:tanf, "gov_benefits"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3001, id_pcg,
                                   depression:mental_illness_othr, "psych_do"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3001, id_pcg,
                                   diabetes:chr_dz_othr, "chr_dz"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3001, id_pcg,
                                   gdm:preg_cx_othr, "preg_cx"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3001, id_pcg,
                                   gdm_hx:preg_cx_othr_hx, "preg_cx_hx"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3001, id_pcg,
                                   ptb:sb, "preg_outcome_hx"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3001, id_pcg,
                                   food_barrier_01:food_barrier_05, "food_barriers"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3001, id_pcg,
                                    bf_ed:toxo_ed, "ed_neg", "NEG"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3001, id_pcg,
                                    bf_ed:toxo_ed, "ed_pos", "POS"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3001, id_pcg,
                                    baby_me_ref:wic_ref, "ref_d", "D"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3001, id_pcg,
                                    baby_me_ref:wic_ref, "ref_a", "A"),
                     by = "id_pcg") %>%
    dplyr::select(suppressWarnings(dplyr::one_of(p3_variables)))

  save(P3001, file = "phi/temp/P3001.rda")

}

.readP3002 <- function () {

  P3002 <- readr::read_csv("phi/unprocessed/P3002.csv",
                           col_types = readr::cols()) %>%
    tidyr::unite("name_pcg", c("last_pcg", "first_pcg"), sep = ", ") %>%
    tidyr::unite("visitor", c("last_visitor", "first_visitor"), sep = ", ") %>%
    dplyr::mutate(review_date = ifelse(exists("review_date"), review_date, NA)) %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::one_of(c("visit_date", "dob_pcg", "edd", "pnc_date",
                      "folic_acid_date", "review_date"))),
      dplyr::funs(lubridate::as_date(.))) %>%
    dplyr::mutate(id_pcg = stringr::str_pad(as.character(id_pcg), 5, "left", "0"),
                  proc_days = lubridate::day(lubridate::as.period(
                    lubridate::interval(visit_date, review_date)))) %>%
    dplyr::mutate_at(dplyr::vars(snap:tanf, gdm:preg_cx_othr,
                                 food_barrier_01:food_barrier_05),
                     dplyr::funs(replace(., . == "Off", NA))) %>%
    dplyr::mutate_at(dplyr::vars(dcs, uninsured_pcg:private_pcg,
                                 dplyr::ends_with("_opt")),
                     dplyr::funs(replace(., . == "Off", 0)))

  P3002 <- P3002 %>%
    dplyr::full_join(.vars2ListCol(P3002, id_pcg,
                                   snap:tanf, "gov_benefits"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3002, id_pcg,
                                   gdm:preg_cx_othr, "preg_cx"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3002, id_pcg,
                                   food_barrier_01:food_barrier_05, "food_barriers"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3002, id_pcg,
                                    bf_ed:toxo_ed, "ed_neg", "NEG"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3002, id_pcg,
                                    bf_ed:toxo_ed, "ed_pos", "POS"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3002, id_pcg,
                                    baby_me_ref:wic_ref, "ref_d", "D"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol2(P3002, id_pcg,
                                    baby_me_ref:wic_ref, "ref_a", "A"),
                     by = "id_pcg") %>%
    dplyr::select(suppressWarnings(dplyr::one_of(p3_variables)))

  save(P3002, file = "phi/temp/P3002.rda")

}

.readP3003 <- function () {

  P3003 <- readr::read_csv("phi/unprocessed/P3003.csv",
                           col_types = readr::cols()) %>%
    tidyr::unite("name_child", c("last_child", "first_child"), sep = ", ") %>%
    tidyr::unite("name_pcg", c("last_pcg", "first_pcg"), sep = ", ") %>%
    tidyr::unite("visitor", c("last_visitor", "first_visitor"), sep = ", ") %>%
    dplyr::mutate(ldd = ifelse(exists("ldd"), ldd, NA),
                  pnc_date = ifelse(exists("pnc_date"), pnc_date, NA),
                  folic_acid_date = ifelse(exists("folic_acid_date"), folic_acid_date, NA),
                  wcv_01mo = ifelse(exists("wcv_01mo"), wcv_01mo, NA),
                  wcv_02mo = ifelse(exists("wcv_02mo"), wcv_02mo, NA),
                  childbirths = ifelse(exists("childbirths"), childbirths, NA),
                  wt_pre_mother = ifelse(exists("wt_pre_mother"), wt_pre_mother, NA)) %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::one_of(c("visit_date", "dob_child", "dob_pcg", "edd", "ldd",
                      "pnc_date", "folic_acid_date", "review_date"))),
      dplyr::funs(lubridate::as_date(.))) %>%
    dplyr::mutate(status = "CI-00",
                  id_child = stringr::str_pad(as.character(id_child), 5, "left", "0"),
                  id_pcg = stringr::str_pad(as.character(id_pcg), 5, "left", "0"),
                  age_child = lubridate::month(lubridate::as.period(
                    lubridate::interval(dob_child, visit_date))),
                  age_pcg = lubridate::year(lubridate::as.period(
                    lubridate::interval(dob_pcg, visit_date))),
                  primigravida = ifelse(as.numeric(childbirths) == 0, 1, 0),
                  race_pcg = ifelse(hispanic_pcg == 1, 5, race_pcg),
                  bmi_pre = (wt_pre_mother / ht_mother^2) * 703,
                  bmi = (wt_mother / ht_mother^2) * 703,
                  diabetes_risk = ifelse(
                    diabetes_screen_02 == 1 | diabetes_screen_03 == 1, 1, 0),
                  spacing = ifelse(ldd == lubridate::as_date("1900-01-01"), NA,
                                   round(as.numeric(lubridate::as.duration(
                                     lubridate::interval(ldd, edd - lubridate::weeks(40)))) / 2628288,
                                     digits = 1)),
                  lead_mother = ifelse(
                    lead_mother_01 == 1 | lead_mother_02 == 1 | lead_mother_03 == 1 |
                      lead_mother_04 == 1 | lead_mother_05 == 1 | lead_mother_06 == 1 |
                      lead_mother_07 == 1 | lead_mother_08 == 1 | lead_mother_09 == 1,
                    1, 0),
                  wt_child = round(
                    ifelse(is.na(wt_child_oz), wt_child_lbs * 453.59237,
                           (wt_child_lbs + wt_child_oz / 16) * 453.59237), 0),
                  race_child = ifelse(hispanic_child == 1, 5, race_child),
                  proc_days = lubridate::day(lubridate::as.period(
                    lubridate::interval(visit_date, review_date)))) %>%
    dplyr::mutate_at(dplyr::vars(snap:tanf, food_barrier_01:food_barrier_05,
                                 depression:other_mental_illness,
                                 diabetes:chr_dz_othr, gdm:sb),
                     dplyr::funs(replace(., . == "Off", NA))) %>%
    dplyr::mutate_at(dplyr::vars(dcs, uninsured_pcg:private_pcg,
                                 uninsured_child:private_child,
                                 dplyr::ends_with("_opt")),
                     dplyr::funs(replace(., . == "Off", 0)))

  P3003 <- P3003 %>%
    dplyr::full_join(.vars2ListCol(P3003, id_pcg,
                                   snap:tanf, "gov_benefits"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3003, id_pcg,
                                   depression:other_mental_illness, "psych_do"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3003, id_pcg,
                                   diabetes:chr_dz_othr, "chr_dz"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3003, id_pcg,
                                   gdm:preg_cx_othr, "preg_cx"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3003, id_pcg,
                                   gdm_hx:preg_cx_othr_hx, "preg_cx_hx"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3003, id_pcg,
                                   ptb:sb, "preg_outcome_hx"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3003, id_pcg,
                                   food_barrier_01:food_barrier_05, "food_barriers"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3003, id_child,
                                   wcv_01mo:wcv_02mo, "well_child"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3003, id_child,
                                    bf_ed:toxo_ed, "ed_neg", "NEG"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3003, id_child,
                                    bf_ed:toxo_ed, "ed_pos", "POS"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3003, id_child,
                                    baby_me_ref:wic_ref, "ref_d", "D"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3003, id_child,
                                    baby_me_ref:wic_ref, "ref_a", "A"),
                     by = "id_child") %>%
    dplyr::select(suppressWarnings(dplyr::one_of(p3_variables)))

  save(P3003, file = "phi/temp/P3003.rda")

}

.readP3004a <- function () {

  P3004a <- readr::read_csv("phi/unprocessed/P3004a.csv",
                           col_types = readr::cols()) %>%
    tidyr::unite("name_child", c("last_child", "first_child"), sep = ", ") %>%
    tidyr::unite("name_pcg", c("last_pcg", "first_pcg"), sep = ", ") %>%
    tidyr::unite("visitor", c("last_visitor", "first_visitor"), sep = ", ") %>%
    dplyr::mutate(wcv_01mo = ifelse(exists("wcv_01mo"), wcv_01mo, NA),
                  wcv_02mo = ifelse(exists("wcv_02mo"), wcv_02mo, NA),
                  wcv_04mo = ifelse(exists("wcv_04mo"), wcv_04mo, NA),
                  wcv_06mo = ifelse(exists("wcv_06mo"), wcv_06mo, NA),
                  review_date = ifelse(exists("review_date"), review_date, NA)) %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::one_of(c("visit_date", "dob_child", "dob_pcg", "review_date"))),
      dplyr::funs(lubridate::as_date(.))) %>%
    dplyr::mutate(id_child = stringr::str_pad(as.character(id_child), 5, "left", "0"),
                  id_pcg = stringr::str_pad(as.character(id_pcg), 5, "left", "0"),
                  proc_days = lubridate::day(lubridate::as.period(
                    lubridate::interval(visit_date, review_date)))) %>%
    dplyr::mutate_at(dplyr::vars(snap:tanf, food_barrier_01:food_barrier_05),
                     dplyr::funs(replace(., . == "Off", NA))) %>%
    dplyr::mutate_at(dplyr::vars(dcs, uninsured_pcg:private_pcg,
                                 uninsured_child:private_child,
                                 dplyr::ends_with("_opt")),
                     dplyr::funs(replace(., . == "Off", 0)))

  P3004a <- P3004a %>%
    dplyr::full_join(.vars2ListCol(P3004a, id_pcg,
                                   snap:tanf, "gov_benefits"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3004a, id_pcg,
                                   food_barrier_01:food_barrier_05, "food_barriers"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3004a, id_child,
                                   wcv_01mo:wcv_06mo, "well_child"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004a, id_child,
                                    bf_ed:toxo_ed, "ed_neg", "NEG"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004a, id_child,
                                    bf_ed:toxo_ed, "ed_pos", "POS"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004a, id_child,
                                    baby_me_ref:wic_ref, "ref_d", "D"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004a, id_child,
                                    baby_me_ref:wic_ref, "ref_a", "A"),
                     by = "id_child") %>%
    dplyr::select(suppressWarnings(dplyr::one_of(p3_variables)))

  save(P3004a, file = "phi/temp/P3004a.rda")

}

.readP3004b <- function () {

  P3004b <- readr::read_csv("phi/unprocessed/P3004b.csv",
                            col_types = readr::cols()) %>%
    tidyr::unite("name_child", c("last_child", "first_child"), sep = ", ") %>%
    tidyr::unite("name_pcg", c("last_pcg", "first_pcg"), sep = ", ") %>%
    tidyr::unite("visitor", c("last_visitor", "first_visitor"), sep = ", ") %>%
    dplyr::mutate(wcv_09mo = ifelse(exists("wcv_09mo"), wcv_09mo, NA),
                  wcv_12mo = ifelse(exists("wcv_12mo"), wcv_12mo, NA),
                  wcv_15mo = ifelse(exists("wcv_15mo"), wcv_15mo, NA),
                  wcv_18mo = ifelse(exists("wcv_18mo"), wcv_18mo, NA),
                  lead_child_01 = ifelse(exists("lead_child_01"), lead_child_01, NA),
                  lead_child_02 = ifelse(exists("lead_child_02"), lead_child_02, NA),
                  lead_child_03 = ifelse(exists("lead_child_03"), lead_child_03, NA),
                  lead_child_04 = ifelse(exists("lead_child_04"), lead_child_04, NA),
                  lead_child_05 = ifelse(exists("lead_child_05"), lead_child_05, NA),
                  lead_child_06 = ifelse(exists("lead_child_06"), lead_child_06, NA),
                  lead_child_07 = ifelse(exists("lead_child_07"), lead_child_07, NA),
                  lead_child_08 = ifelse(exists("lead_child_08"), lead_child_08, NA),
                  lead_child_09 = ifelse(exists("lead_child_09"), lead_child_09, NA),
                  review_date = ifelse(exists("review_date"), review_date, NA)) %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::one_of(c("visit_date", "dob_child", "dob_pcg", "review_date"))),
      dplyr::funs(lubridate::as_date(.))) %>%
    dplyr::mutate(id_child = stringr::str_pad(as.character(id_child), 5, "left", "0"),
                  id_pcg = stringr::str_pad(as.character(id_pcg), 5, "left", "0"),
                  lead_child = ifelse(
                    lead_child_01 == 1 | lead_child_02 == 1 | lead_child_03 == 1 |
                      lead_child_04 == 1 | lead_child_05 == 1 | lead_child_06 == 1 |
                      lead_child_07 == 1 | lead_child_08 == 1 | lead_child_09 == 1,
                    1, 0),
                  proc_days = lubridate::day(lubridate::as.period(
                    lubridate::interval(visit_date, review_date)))) %>%
    dplyr::mutate_at(dplyr::vars(snap:tanf, food_barrier_01:food_barrier_05),
                     dplyr::funs(replace(., . == "Off", NA))) %>%
    dplyr::mutate_at(dplyr::vars(dcs, uninsured_pcg:private_pcg,
                                 uninsured_child:private_child,
                                 dplyr::ends_with("_opt")),
                     dplyr::funs(replace(., . == "Off", 0)))

  P3004b <- P3004b %>%
    dplyr::full_join(.vars2ListCol(P3004b, id_pcg,
                                   snap:tanf, "gov_benefits"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3004b, id_pcg,
                                   food_barrier_01:food_barrier_05, "food_barriers"),
                     by = "id_pcg") %>%
    dplyr::full_join(.vars2ListCol(P3004b, id_child,
                                   wcv_09mo:wcv_18mo, "well_child"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004b, id_child,
                                    bf_ed:toxo_ed, "ed_neg", "NEG"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004b, id_child,
                                    bf_ed:toxo_ed, "ed_pos", "POS"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004b, id_child,
                                    baby_me_ref:wic_ref, "ref_d", "D"),
                     by = "id_child") %>%
    dplyr::full_join(.vars2ListCol2(P3004b, id_child,
                                    baby_me_ref:wic_ref, "ref_a", "A"),
                     by = "id_child") %>%
    dplyr::select(suppressWarnings(dplyr::one_of(p3_variables)))

  save(P3004b, file = "phi/temp/P3004b.rda")

}
