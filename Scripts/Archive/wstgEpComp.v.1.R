# call/install packages ---------------------------------------------------
## package list
packages <- c(
  'dplyr',
  'tidyr',
  'ggplot2',
  'openxlsx',
  'lubridate',
  'janitor'
)

## install or load packages
if (!require(install.load)) {
  install.packages('install.load')
}

install.load::install_load(packages)

# read/manipulate data ----------------------------------------------------
ep.dat <- read.xlsx(
  'Data\\Archive\\Recruitment_Ep_Table.xlsx',
  colNames = TRUE
) |> 
  pivot_longer(cols = 2:9) |> 
  arrange(name,Year)

read_daily_WQFun <- function(file){
  read.xlsx(
    paste0(
      'Data\\Archive\\',
      file,
      '_WQ_Data.xlsx'
    ),
    colNames = TRUE
  ) |> 
    mutate(
      Date = excel_numeric_to_date(Date)
    ) |> 
    rename(
      date = 1,
      outflow = 2,
      spill = 3,
      spill_prop = 4,
      temp = 5,
      bpress = 6,
      disgas = 7,
      disgas_prop = 8,
      elev = 9
    ) |>
    group_by(date) |> 
    summarize(
      across(
        outflow:elev,
        ~ mean(.x, na.rm = TRUE)
      )
    ) |> 
    mutate(
      res = file
    )
}


list <- list(
  'BON',
  'JDA',
  'MCN',
  'TDA'
)

lapply(list, function(l) do.call('myfunc', l))

wqDaily.dat <- read_daily_WQFun('BON') |>
  bind_rows(
    read_daily_WQFun('JDA')  
  ) |> 
  bind_rows(
    read_daily_WQFun('MCN')
  ) |> 
  bind_rows(
    read_daily_WQFun('TDA')
  ) |> 
  mutate(
    spill_prop = spill_prop/100,
    disgas_prop = disgas_prop/100
  )

read_ann_WQFun <- function(file){
  read.xlsx(
    paste0(
      'Data\\Archive\\',
      file,
      '_WQ_Data.xlsx'
    ),
    colNames = TRUE
  ) |> 
    mutate(
      Date = excel_numeric_to_date(Date)
    ) |> 
    rename(
      date = 1,
      outflow = 2,
      spill = 3,
      spill_prop = 4,
      temp = 5,
      bpress = 6,
      disgas = 7,
      disgas_prop = 8,
      elev = 9
    ) |>
    group_by(
      year = year(date)
    ) |> 
    summarize(
      across(
        outflow:elev,
        ~ mean(.x, na.rm = TRUE)
      )
    ) |> 
    mutate(
      res = file
    )
}

wqAnn.dat <- read_ann_WQFun('BON') |>
  bind_rows(
    read_ann_WQFun('JDA')  
  ) |> 
  bind_rows(
    read_ann_WQFun('MCN')
  ) |> 
  bind_rows(
    read_ann_WQFun('TDA')
  ) |> 
  mutate(
    spill_prop = spill_prop/100,
    disgas_prop = disgas_prop/100
  )


  
