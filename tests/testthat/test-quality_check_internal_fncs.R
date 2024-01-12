test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that(".check_date_works", {
  expect_equal(object = LTMFNpackage:::.check_date("2023-10-01"), "")

  expect_equal(object = LTMFNpackage:::.check_date("2023-10-00"), "date_invalid/")
  expect_equal(object = LTMFNpackage:::.check_date("2023-10-00"), "date_invalid/")
  expect_equal(object = LTMFNpackage:::.check_date("2023-00-01"), "date_invalid/")
  expect_equal(object = LTMFNpackage:::.check_date("2023-02-30"), "date_invalid/")

  expect_equal(object = LTMFNpackage:::.check_date("01-01-2023"), "date_invalid/")
  expect_equal(object = LTMFNpackage:::.check_date("Jan 1 2023"), "date_invalid/")
  expect_equal(object = LTMFNpackage:::.check_date("teststring"), "date_invalid/")

  #BEWARE OF DATES COMING IN WITH POSITx formatting already
  #expect_equal(object = LTMFNpackage:::.check_date(Sys.Date()))

  expect_equal(object = LTMFNpackage:::.check_date(""), "date_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_date(NA), "date_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_date(NULL), "date_not_entered/")

  expect_equal(object = LTMFNpackage:::.check_date("2022-10-01"), "date_out_of_range/")
  expect_equal(object = LTMFNpackage:::.check_date(as.character(Sys.Date()+366)), "date_out_of_range/")
})

test_that(".check_time_works", {
  expect_equal(object = LTMFNpackage:::.check_time("00:00:00"), "")
  expect_equal(object = LTMFNpackage:::.check_time("1:00:00"), "")

  expect_equal(object = LTMFNpackage:::.check_time("00:00-00"), "time_invalid/")
  expect_equal(object = LTMFNpackage:::.check_time("00/00:00"), "time_invalid/")
  expect_equal(object = LTMFNpackage:::.check_time("1:00:00 PM"), "time_invalid/")
  expect_equal(object = LTMFNpackage:::.check_time("00:00"), "time_invalid/")
  expect_equal(object = LTMFNpackage:::.check_time("teststring"), "time_invalid/")

  expect_equal(object = LTMFNpackage:::.check_time(""), "time_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_time(NA), "time_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_time(NULL), "time_not_entered/")
})

test_that(".check_site_works", {
  expect_equal(object = LTMFNpackage:::.check_site("JOY"), "")
  expect_equal(object = LTMFNpackage:::.check_site("LWE"), "")

  expect_equal(object = LTMFNpackage:::.check_site("lwe"), "site_invalid/")
  expect_equal(object = LTMFNpackage:::.check_site("JB"), "site_invalid/")
  expect_equal(object = LTMFNpackage:::.check_site("LSP"), "site_invalid/")
  expect_equal(object = LTMFNpackage:::.check_site("lac-saint-pierre"), "site_invalid/")

  expect_equal(object = LTMFNpackage:::.check_site(""), "site_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_site(NA), "site_not_entered/")
})

test_that(".check_equip_works", {
  expect_equal(object = LTMFNpackage:::.check_equip("VR2Tx"), "")
  expect_equal(object = LTMFNpackage:::.check_equip("AMdo"), "")

  expect_equal(object = LTMFNpackage:::.check_equip("vr2tx"), "equip_invalid/")
  expect_equal(object = LTMFNpackage:::.check_equip("receiver"), "equip_invalid/")
  expect_equal(object = LTMFNpackage:::.check_equip("490668"), "equip_invalid/")
  expect_equal(object = LTMFNpackage:::.check_equip(490668), "equip_invalid/")

  expect_equal(object = LTMFNpackage:::.check_equip(""), "equip_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_equip(NA), "equip_not_entered/")
})

test_that(".check_serial_works", {
  expect_equal(object = LTMFNpackage:::.check_serial("490656"), "")
  expect_equal(object = LTMFNpackage:::.check_serial(490656), "")
  expect_equal(object = LTMFNpackage:::.check_serial("012246"), "")


  expect_equal(object = LTMFNpackage:::.check_serial("vr2tx"), "serial_invalid/")
  expect_equal(object = LTMFNpackage:::.check_serial("receiver"), "equip_invalid/")

  #is this an issue??
  expect_equal(object = LTMFNpackage:::.check_serial(012246), "serial_invalid/")
  expect_equal(object = LTMFNpackage:::.check_serial(490999), "serial_invalid/")

  expect_equal(object = LTMFNpackage:::.check_serial(""), "serial_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_serial(NA), "serial_not_entered/")
})

test_that(".check_equip_serial_match_works", {
  expect_equal(object = LTMFNpackage:::.check_equip_serial_match(equip = "VR100", serial = "012246"), "")

  expect_equal(object = LTMFNpackage:::.check_equip_serial_match(equip = "VR100", serial = "490666"), "equip_serial_nomatch/")
  expect_equal(object = LTMFNpackage:::.check_equip_serial_match(equip = "VR100", serial = "490666"), "equip_serial_nomatch/")
})

test_that(".check_stnid_works", {
  expect_equal(object = LTMFNpackage:::.check_stnid("RT-LWE-01-A"), "")
  expect_equal(object = LTMFNpackage:::.check_stnid("GA-JEA-99-Z"), "")
  expect_equal(object = LTMFNpackage:::.check_stnid("GR-LWE-01-A"), "")
  expect_equal(object = LTMFNpackage:::.check_stnid(NA), "")

  expect_equal(object = LTMFNpackage:::.check_stnid("rt-lwe-01-a"), "stnid_invalid/")
  expect_equal(object = LTMFNpackage:::.check_stnid(490999), "stnid_invalid/")
  expect_equal(object = LTMFNpackage:::.check_stnid("RT-LWE-01-!"), "stnid_invalid/")
  expect_equal(object = LTMFNpackage:::.check_stnid("RT/LWE/01-/A"), "stnid_invalid/")
  expect_equal(object = LTMFNpackage:::.check_stnid("RT-LWE-01"), "stnid_invalid/")
  expect_equal(object = LTMFNpackage:::.check_stnid("LWE-01-A"), "stnid_invalid/")
})

test_that(".check_action_works", {
  expect_equal(object = LTMFNpackage:::.check_action("On"), "")
  expect_equal(object = LTMFNpackage:::.check_action("deployed"), "")
  expect_equal(object = LTMFNpackage:::.check_action("dataDownload"), "")

  expect_equal(object = LTMFNpackage:::.check_action(NA), "action_not_entered/")

  expect_equal(object = LTMFNpackage:::.check_action("on"), "action_invalid/")
  expect_equal(object = LTMFNpackage:::.check_action("otherAction"), "action_invalid/")
  expect_equal(object = LTMFNpackage:::.check_action(490668), "action_invalid/")
})

test_that(".check_deploy_works", {
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = "GA",site = "LWE"), "")
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = "GA",site = "lab"), "")
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = "GA",site = NA), "")

  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = NA,site = NA, mandatory = TRUE), "")
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = "",site = NA), "")
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = NA,site = "lab"), "")
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = "",site = "lab"), "")
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = NA,site = "other"), "")
  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = NA,site = "base"), "")

  expect_equal(object = LTMFNpackage:::.check_deploy(deploy = NA,site = "LWE", mandatory = TRUE), "deploy_not_entered/")

  expect_equal(object = LTMFNpackage:::.check_deploy("on", site = NA), "deploy_invalid/")
  expect_equal(object = LTMFNpackage:::.check_deploy("on", site = "lab"), "deploy_invalid/")
  expect_equal(object = LTMFNpackage:::.check_deploy(490668, site = "lab"), "deploy_invalid/")
})

test_that(".check_lat_works", {
  expect_equal(object = LTMFNpackage:::.check_lat(lat = 45.9), "")
  expect_equal(object = LTMFNpackage:::.check_lat(lat = "45.9"), "")

  expect_equal(object = LTMFNpackage:::.check_lat(lat = 45.0), "lat_out_of_range/")
  expect_equal(object = LTMFNpackage:::.check_lat(lat = 55.6), "lat_out_of_range/")

  expect_equal(object = LTMFNpackage:::.check_lat(lat = "lat_value"), "lat_invalid/")
  expect_equal(object = LTMFNpackage:::.check_lat(lat = Sys.Date()), "lat_out_of_range/")
})

test_that(".check_lon_works", {
  expect_equal(object = LTMFNpackage:::.check_lon(lon = -79.4), "")
  expect_equal(object = LTMFNpackage:::.check_lon(lon = "45.9"), "")

  expect_equal(object = LTMFNpackage:::.check_lon(lon = -79.6), "lon_out_of_range/")
  expect_equal(object = LTMFNpackage:::.check_lon(lon = -72.4), "lon_out_of_range/")

  expect_equal(object = LTMFNpackage:::.check_lon(lon = "lon_value"), "lon_invalid/")
  expect_equal(object = LTMFNpackage:::.check_lon(lon = Sys.Date()), "lon_out_of_range/")
})

test_that(".check_depth_works", {
  expect_equal(object = LTMFNpackage:::.check_depth(depth = 2), "")
  expect_equal(object = LTMFNpackage:::.check_depth(depth = "2"), "")

  expect_equal(object = LTMFNpackage:::.check_depth(depth = -2), "depth_out_of_range/")
  expect_equal(object = LTMFNpackage:::.check_depth(depth = "-2"), "depth_out_of_range/")

  expect_equal(object = LTMFNpackage:::.check_depth(depth = "depth_value"), "depth_invalid/")
  expect_equal(object = LTMFNpackage:::.check_depth(depth = Sys.Date()), "depth_out_of_range/")
})

test_that(".check_crew_works", {
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "ND"), "")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "NAD"), "")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "NAD, ND, NAD, ND"), "")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "NAD, NAD, NAD, NAD"), "")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "ND, ND, ND, ND"), "")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "ND,ND,ND,ND"), "")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "NAD,NAD,NAD,NAD"), "")

  expect_equal(object = LTMFNpackage:::.check_crew(crew = "NADA"), "crew_invalid/")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "NA/ DA"), "crew_invalid/")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "N,N"), "crew_invalid/")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = "N,,"), "crew_invalid/")

  expect_equal(object = LTMFNpackage:::.check_crew(crew = ""), "crew_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_crew(crew = NA), "crew_not_entered/")
})

test_that(".check_capture_method_works", {
  expect_equal(object = LTMFNpackage:::.check_capture_method(capture_method = "fyke"), "")
  expect_equal(object = LTMFNpackage:::.check_capture_method(capture_method = "other"), "")

  expect_equal(object = LTMFNpackage:::.check_capture_method(capture_method = "Fyke"), "capture_invalid/")
  expect_equal(object = LTMFNpackage:::.check_capture_method(capture_method = "spear"), "capture_invalid/")

  expect_equal(object = LTMFNpackage:::.check_capture_method(capture_method = ""), "capture_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_capture_method(capture_method = NA), "capture_not_entered/")
})

test_that(".check_species_works", {
  expect_equal(object = LTMFNpackage:::.check_species(species = "BKTR"), "")
  expect_equal(object = LTMFNpackage:::.check_species(species = "bycatch"), "")

  expect_equal(object = LTMFNpackage:::.check_species(species = "brook trout"), "species_invalid/")
  expect_equal(object = LTMFNpackage:::.check_species(species = "bktr"), "species_invalid/")

  expect_equal(object = LTMFNpackage:::.check_species(species = ""), "species_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_species(species = NA), "species_not_entered/")
})

test_that(".check_temp_works", {
  expect_equal(object = LTMFNpackage:::.check_temp(temp = 20), "")
  expect_equal(object = LTMFNpackage:::.check_temp(temp = "20"), "")

  expect_equal(object = LTMFNpackage:::.check_temp(temp = 25.1), "temp_out_of_range/")
  expect_equal(object = LTMFNpackage:::.check_temp(temp = -4.1), "temp_out_of_range/")

  expect_equal(object = LTMFNpackage:::.check_temp(temp = "", mandatory = TRUE), "temp_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_temp(temp = NA, mandatory = TRUE), "temp_not_entered/")
})

test_that(".check_length_works", {
  expect_equal(object = LTMFNpackage:::.check_length(length = 200), "")
  expect_equal(object = LTMFNpackage:::.check_length(length = "200"), "")

  expect_equal(object = LTMFNpackage:::.check_length(length = 149), "length_out_of_range/")
  expect_equal(object = LTMFNpackage:::.check_length(length = 1501), "length_out_of_range/")

  expect_equal(object = LTMFNpackage:::.check_length(length = ""), "length_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_length(length = NA), "length_not_entered/")
})

test_that(".check_weight_works", {
  expect_equal(object = LTMFNpackage:::.check_weight(weight = 120), "")
  expect_equal(object = LTMFNpackage:::.check_weight(weight = "120"), "")

  expect_equal(object = LTMFNpackage:::.check_weight(weight = 70), "weight_out_of_range/")
  expect_equal(object = LTMFNpackage:::.check_weight(weight = 30001), "weight_out_of_range/")

  expect_equal(object = LTMFNpackage:::.check_weight(weight = ""), "weight_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_weight(weight = NA), "weight_not_entered/")
})

test_that(".check_sex_works" , {
  expect_equal(object = LTMFNpackage:::.check_sex(sex = "MM"), "")
  expect_equal(object = LTMFNpackage:::.check_sex(sex = "M"), "")
  expect_equal(object = LTMFNpackage:::.check_sex(sex = "unk"), "")

  expect_equal(object = LTMFNpackage:::.check_sex(sex = "100"), "sex_invalid/")

  expect_equal(object = LTMFNpackage:::.check_sex(sex = ""), "sex_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_sex(sex = NA), "sex_not_entered/")
})

test_that(".check_sex_works", {
  expect_equal(object = LTMFNpackage:::.check_sex(sex = "MM"), "")
  expect_equal(object = LTMFNpackage:::.check_sex(sex = "unk"), "")

  expect_equal(object = LTMFNpackage:::.check_sex(sex = "M"), "sex_invalid/")
  expect_equal(object = LTMFNpackage:::.check_sex(sex = "100"), "sex_invalid/")

  expect_equal(object = LTMFNpackage:::.check_sex(sex = ""), "sex_not_entered/")
  expect_equal(object = LTMFNpackage:::.check_sex(sex = NA), "sex_not_entered/")
})

