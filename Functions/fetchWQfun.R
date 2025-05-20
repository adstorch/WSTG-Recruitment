# retrieve data -----------------------------------------------------------
EpCovFetch_wrap <- function(res,begYr,endYr,startDate,endDate){
  
  ## create blank data frame to populate iteratively
  inp.rec.dat = data.frame(
    res = character(),
    date = character(),
    month = numeric(),
    day = numeric(),
    year = numeric(),
    outflow = numeric(),
    spill = numeric(),
    spill_perc = numeric(),
    inflow = numeric(),
    temp_sc = numeric(),
    temp_c = numeric(),
    barop = numeric(),
    disgas = numeric(),
    disgas_perc = numeric(),
    turb = numeric(),
    elev = numeric()
  )

  EpCovFetch <- function(year, loc=c("CCIW","TDDO","JHAW","MCPW"), begMD, endMD){
    loc <- match.arg(loc)
    smonth <- strsplit(begMD, "[/]")[[1]][1]
    emonth <- strsplit(endMD, "[/]")[[1]][1]
    sday <- strsplit(begMD, "[/]")[[1]][2]
    eday <- strsplit(endMD, "[/]")[[1]][2]
    
    urlCont <- getURL(
      paste0(
        'https://www.cbr.washington.edu/dart/cs/php/rpt/river_daily.php?sc=1&outputFormat=csv&year=',
        year,
        '&proj=',
        loc,
        '&span=no&startdate=',
        smonth,
        '%2F',
        sday,
        '&enddate=',
        emonth,
        '%2F',
        eday,
        '&syear=',
        year,
        '&eyear=',
        year
      )
    )
    
    clean.dat <- gsub('<br />', '', urlCont)
    finale.dat <- read.csv(textConnection(clean.dat)) |> 
      head(-14) |> 
      rename(
        res = 1,
        date = 2,
        outflow = 3,
        spill = 4,
        spill_perc = 5,
        inflow = 6,
        temp_sc = 7,
        temp_c = 8,
        barop = 9,
        disgas = 10,
        disgas_perc = 11,
        turb = 12,
        elev = 13
      ) |> 
      mutate(
        # res = if_else(
        #   res == 'CCIW','BON',
        #   if_else(
        #     res == 'TDDO','TDA',
        #     if_else(
        #       res == 'JHAW','JDA',
        #       if_else(
        #         res == 'MCPW','MCN',NA
        #       )
        #     )
        #   )
        # ),
        month = month(date),
        day = day(date),
        year = year(date)
      ) |> 
      relocate(
        c(
          month,
          day,
          year
        ),
        .after = date
      )
    
    inp.rec.dat <<- bind_rows(
      inp.rec.dat,
      finale.dat
    )
  }
  
  begMD <- startDate
  endMD <- endDate
  
  for(loc in res){
    for(year in begYr:endYr){
      tryCatch({
      EpCovFetch(year,loc,begMD,endMD)
      }, error=function(e){})
    }
  }
  inp.dat <<- inp.rec.dat
}

