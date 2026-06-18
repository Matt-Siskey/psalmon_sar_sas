get_returns <-function(drive, wria_region){
  # sort(unique(odbcListDrivers()[[1]]))
  # con <- odbcConnect("DFWDBOLYPWSQL3")
  # con <- odbcConnect("busprod_hatcheries")
  # con <- odbcConnect("BUSPROD")
  # con <- odbcConnect("DFWDBOLYPWSQL3\\BUSPROD")
  # close(con)
  # confb <- DBI::dbConnect(odbc::odbc(), DSN = "DFWDBOLYPWSQL3")
  # confb <- DBI::dbConnect(odbc::odbc(), DSN = "busprod_hatcheries")
  # confb <- DBI::dbConnect(odbc::odbc(), DSN = "BUSPROD")
  # confb2 <- dbConnect(odbc(),
  #                  Driver = "SQL Server", 
  #                  Server = "DFWDBOLYPWSQL3\\BUSPROD",
  #                  Database = "Hatcheries",
  #                  Trusted_Connection = "True")
  
  ### Read in FishBooks pull
  returns <-read.csv(paste0(drive,"Data/trap_estimates.csv")) # Trap Estimates for all WDFW facilities.xlsx from Caitlynne Bishop (FishBooks)
  # returns_alt <-read.csv(paste0(drive,"RMIS_recoveries_WA_chinook_1980-2025.csv")) # RMIS db query
  returns_HOR_agg <-aggregate(Adults~Species+BY+Reporting.Facility, data=dplyr::filter(returns,Origin=="H"), FUN=sum)
  colnames(returns_HOR_agg) <-c("species", "trap_yr", "hatchery", "returns")
  # Add species_names
  specCode_rename_ret <-c("CK"="chinook", "SO"="sockeye", "SH"="steelhead", "CO"="coho", "CT"="cutthroat", "CH"="chum", "PK"="pink")
  returns_HOR_agg$species_name <-as.character(specCode_rename_ret[returns_HOR_agg$species])
  
  ### Tried using returns$stock but there are no definitions I can find & don't match releases$release_location_rmis_region & basin (e.g., ELWH vs. ELDU)
  # Instead, need to assign WRIA, then RMIS region based on WRIA
  # Clean returns_H_agg$hatchery names & assign wria to each
  returns_rename <-c("baker lk hatchery"       = "baker lake",        "barnaby slough pd"            = "barnaby slough",
                     "beaver cr hatchery"      = "beaver creek",      "bingham cr hatchery"          = "bingham creek",
                     "bogachiel hatchery"      = "bogachiel",         "cedar river hatchery"         = "cedar river",
                     "chambers cr hatchery"    = "chambers creek",    "chelan hatchery"              = "chelan",
                     "chiwawa hatchery"        = "chiwawa",           "cottonwood cr pond"           = "cottonwood creek",
                     "coulter cr rearing pond" = "coulter creek",     "cowlitz salmon hatchery"      = "cowlitz_salmon",
                     "cowlitz trout hatchery"  = "cowlitz_trout",     "dayton acclima. pond"         = "dayton",
                     "dungeness hatchery"      = "dungeness",         "eastbank hatchery"            = "eastbank",
                     "eells springs"           = "eells springs",     "elk creek trap"               = "elk creek",
                     "elochoman hatchery"      = "elochoman",         "elochoman sill trap"          = "elochoman sill",
                     "elwha hatchery"          = "elwha",             "fallert cr hatchery"          = "fallert creek",
                     "forks creek hatchery"    = "forks creek",       "foster rd trap"               = "foster road",
                     "garrison hatchery"       = "garrison springs",  "george adams hatchery"        = "george adams",
                     "glenwood springs"        = "glenwood springs",  "grays river hatchery"         = "grays river",
                     "hoodsport hatchery"      = "hoodsport",         "humptulips hatchery"          = "humptulips",
                     "hupp springs rearing"    = "hupp springs",      "hurd cr hatchery"             = "hurd creek",
                     "icy cr hatchery"         = "icy creek",         "issaquah hatchery"            = "issaquah",
                     "kalama falls hatchery"   = "kalama falls",      "kendall cr hatchery"          = "kendall creek",
                     "klickitat hatchery"      = "klickitat",         "klickitat hatchery (ykfp)"    = "klickitat_ykfp",
                     "lakewood hatchery"       = "lakewood",          "lewis river hatchery"         = "lewis river",
                     "lk aberdeen hatchery"    = "lake aberdeen",     "lyons ferry hatchery"         = "lyons ferry",
                     "marblemount hatchery"    = "marblemount",       "mayr brothers rearin"         = "mayr brothers",
                     "mcallister hatchery"     = "mcallister",        "mckernan hatchery"            = "mckernan",
                     "merwin dam fcf"          = "merwin dam",        "merwin hatchery"              = "merwin",
                     "methow hatchery"         = "methow",            "minter cr hatchery"           = "minter creek",
                     "modrow trap"             = "modrow",            "morse creek hatchery"         = "morse creek",
                     "naselle hatchery"        = "naselle",           "nemah hatchery"               = "nemah",
                     "north toutle fcf"        = "north toutle_fcf",  "north toutle hatchery"        = "north toutle",
                     "omak hatchery"           = "omak",              "onalaska hs(onalask"          = "onalaska",
                     "palmer hatchery"         = "palmer",            "percival cove net pn"         = "percival cove",
                     "priest rapids hatchery"  = "priest rapids",     "puyallup hatchery"            = "puyallup",
                     "reiter ponds"            = "reiter ponds",      "ringold springs hatchery"     = "ringold springs",
                     "samish hatchery"         = "samish",            "satsop springs ponds"         = "satsop springs",
                     "skamania hatchery"       = "skamania",          "skookumchuck dam"             = "skookumchuck dam",
                     "skookumchuck hatchery"   = "skookumchuck",      "solduc hatchery"              = "sol duc",
                     "soos creek hatchery"     = "soos creek",        "speelyai hatchery"            = "speelyai",
                     "sunset falls fcf"        = "sunset falls",      "tacoma power wynoochee r dam" = "wynoochee dam",
                     "tokul cr hatchery"       = "tokul creek",       "tucannon hatchery"            = "tucannon",
                     "tumwater falls hatchery" = "tumwater falls",    "twisp acclimation pd"         = "twisp",
                     "voights cr hatchery"     = "voights creek",     "wallace r hatchery"           = "wallace river",
                     "washougal hatchery"      = "washougal",         "washougal river fish weir"    = "washougal weir",
                     "wells hatchery"          = "wells",             "whatcom cr hatchery"          = "whatcom creek",
                     "whitehorse pond"         = "whitehorse")
  returns_HOR_agg$hatchery_clean <-as.character(returns_rename[tolower(returns_HOR_agg$hatchery)])
  # assign wria so we can pull ages from within the wria for each return (often hatcheries don't have ages for a given return yr)
  hatch_wria <-c("baker lake"=4, "barnaby slough"=4, "beaver creek"=25, "bingham creek"=22, "bogachiel"=20, "cedar river"=8,
                 "chambers creek"=12, "chelan"=47, "chiwawa"=45, "cottonwood creek"=35, "coulter creek"=15, "cowlitz_salmon"=26,
                 "cowlitz_trout"=26, "dayton"=32, "dungeness"=18, "eastbank"=44, "eells springs"=16, "elk creek"=22, "elochoman"=25,
                 "elochoman sill"=25, "elwha"=18, "fallert creek"=27, "forks creek"=24, "foster road"=25, "garrison springs"=12,
                 "george adams"=16, "glenwood springs"=2, "grays river"=25, "humptulips"=22, "hoodsport"=16, "hupp springs"=15,
                 "hurd creek"=18, "icy creek"=9, "issaquah"=8, "kalama falls"=27, "kendall creek"=1, "klickitat"=30,
                 "klickitat_ykfp"=30,"lake aberdeen"=22, "lakewood"=12,"lewis river"=27, "lyons ferry"=35, "marblemount"=4,
                 "mayr brothers"=22, "mcallister"=11, "mckernan"=16, "merwin dam"=27, "merwin"=27, "methow"=48, "minter creek"=15,
                 "modrow"=27, "morse creek"=18, "naselle"=24, "nemah"=24, "north toutle_fcf"=26, "north toutle"=26, "omak"=49,
                 "onalaska"=23, "palmer"=9, "percival cove"=13, "priest rapids"=36, "puyallup"=10, "reiter ponds"=7,
                 "ringold springs"=36, "samish"=3, "satsop springs"=22, "skamania"=28, "skookumchuck dam"=23, "skookumchuck"=23,
                 "sol duc"=20, "soos creek"=9, "speelyai"=27, "sunset falls"=7, "wynoochee dam"=22, "tokul creek"=7, "tucannon"=35,
                 "tumwater falls"=13, "twisp"=48, "voights creek"=10, "wallace river"=7, "washougal"=28, "washougal weir"=28,
                 "wells"=47, "whatcom creek"=1, "whitehorse"=5)
  returns_HOR_agg$wria <-as.character(hatch_wria[returns_HOR_agg$hatchery_clean])
  # Add RMIS region based on WRIAs
  returns_HOR_agg$region <-as.character(wria_region[returns_HOR_agg$wria])
  # returns_H_agg_keep <-dplyr::filter(returns_H_agg, hatchery_clean != "NA")
  # returns_H_agg_NoAge <-returns_H_agg[!complete.cases(returns_H_agg$hatchery_clean),] # returns for hatcheries w/ no age data
  # returns_H_agg_NoAge$hatchery_clean <-tolower(returns_H_agg_NoAge$hatchery)
  
  write.csv(returns_HOR_agg, paste0(drive,"Data/returns_HOR_agg.csv"))
  
  return(returns_HOR_agg)
  print("got returns")
  
}