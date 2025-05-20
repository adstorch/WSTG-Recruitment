urlCont <- getURL(
  'https://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no=14211720&legacy=&referred_module=sw&period=&begin_date=1996-03-12&end_date=2025-03-12'
)

#- beginning year
begYr <- 1989
#- ending year
endYr <- 2024
#- beginning month/day
begMD <- '5/11'
#- ending month/day
endMD <- '7/31'
smonth <- strsplit(begMD, "[/]")[[1]][1]
emonth <- strsplit(endMD, "[/]")[[1]][1]
sday <- strsplit(begMD, "[/]")[[1]][2]
eday <- strsplit(endMD, "[/]")[[1]][2]

'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=14211720&legacy=&referred_module=sw&period=&begin_date=2022-03-13&end_date=2025-03-13'


will_urlCont <- getURL(
  paste0(
    'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=14211720&legacy=&referred_module=sw&period=&begin_date=',
    begYr,
    '-',
    if_else(
      nchar(smonth) == 1,
      paste0(
        '0',smonth
      ),
      smonth
    ),
    '-',
    if_else(
      nchar(sday) == 1,
      paste0(
        '0',sday
      ),
      sday
    ),
    '&end_date=',
    endYr,
    '-',
    if_else(
      nchar(emonth) == 1,
      paste0(
        '0',emonth
      ),
      emonth
    ),
    '-',
    if_else(
      nchar(eday) == 1,
      paste0(
        '0',eday
      ),
      eday
    )
  )
)

will_clean.dat <- gsub('<br />', '', will_urlCont)
will_finale.dat <- read.csv(textConnection(will_clean.dat)) |> 
  tail(-39) |> 
  rename(
    string = 1
  ) |> 
  separate_wider_delim(
    col = string,
    delim = "\t",
    names = c(
      'agency_cd',	
      'site_no'	,
      'datetime',	
      '172765_00060_00003',
      '172765_00060_00003_cd',
      '172771_00010_00001',
      '172771_00010_00001_cd',
      '172772_00010_00002',
      '172772_00010_00002_cd',
      '172773_00010_00003',
      '172773_00010_00003_cd'
    )
  ) |> 
  select(
    agencyID = agency_cd,
    date_ph = datetime,
    outflow_ph = '172765_00060_00003',
    temp_c_ph = '172773_00010_00003'
  ) |> 
  mutate(
    res = 'will',
    date = date_ph,
    month = month(date_ph),
    day = day(date_ph),
    year = year(date_ph),
    outflow = as.numeric(outflow_ph)/100,
    spill = NA,
    spill_perc = NA,
    inflow = NA,
    temp_sc = NA,
    temp_c = as.numeric(temp_c_ph),
    barop = NA,
    disgas = NA,
    disgas_perc = NA,
    turb = NA,
    elev = NA
  ) |> 
  select(
    -c(
      1:4
      )
  )

  str(will_finale.dat)
  