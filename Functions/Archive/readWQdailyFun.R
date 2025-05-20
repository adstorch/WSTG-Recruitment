# read-in water quality/quantity data and summarize (daily mean) ----------
read_daily_WQFun <- function(i,begMo,endMo,begDay,endDay){
  read.xlsx(
    paste0(
      'Data\\Independent\\',
      i
    ),
    colNames = TRUE
  ) |> 
    mutate(
      Date = excel_numeric_to_date(Date),
      month = month(Date),
      day = day(Date)
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
      elev = 9,
      month = 10,
      day = 11
    ) |>
    filter(
      month >= begMo & month <= endMo & day >= begDay & day <= endDay
    ) |> 
    group_by(date) |> 
    summarize(
      across(
        outflow:elev,
        ~ mean(.x, na.rm = TRUE)
      )
    ) |> 
    mutate(
      res = stri_sub(i,1,3)
    )
}
