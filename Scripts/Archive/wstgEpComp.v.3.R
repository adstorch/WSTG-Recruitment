# setup -------------------------------------------------------------------
## call/install packages
### package list
packages <- c(
  'dplyr',
  'tidyr',
  'ggplot2',
  'openxlsx',
  'lubridate',
  'janitor',
  'stringi',
  'gamlss',
  'mgcv',
  'mgcv.helper',
  'EnvStats',
  'htmltools',
  'copula'
)

### install or load packages
if (!require(install.load)) {
  install.packages('install.load')
}

install.load::install_load(packages)

## source functions
### daily summary
source(
  'Functions\\readWQdailyFun.R'
)

### annual summary
source(
  'Functions\\readWQannFun.R'
)

### model selection (all subsets)
source(
  'Functions\\modSelFun.R'
)

## specify data date (m/d) range
startMD <- '4/1'
endMD <- '7/31'

# read/manipulate data ----------------------------------------------------
## response var (Ep)
ep.dat <- read.xlsx(
  ### specify .xlsx FILE containing estimates of response var
  'Data\\Response\\Recruitment_Ep_Table.xlsx',
  colNames = TRUE
) |> 
  pivot_longer(cols = 2:9) |> 
  arrange(name,Year) |> 
  rename(
    year = 1,
    res = 2,
    ep = 3
  )

## independent vars
### spec files to read
file.list = list.files(
  ###- specify DIRECTORY containing .xlsx files with independent vars
  'Data\\Independent',
  ".xlsx"
  ) |> 
  as.list()

### consolidate data on a daily time step
###- Excel files cannot be open when running these lines
wqDaily.dat <- do.call(
  rbind,
  lapply(
    file.list, 
    read_daily_WQFun,
    begMo = as.numeric(substr(startMD,1,1)),
    endMo = as.numeric(substr(endMD,1,1)),
    begDay = as.numeric(substr(startMD,3,4)),
    endDay = as.numeric(substr(endMD,3,4))
  )
)

### consolidate data on an annual time step
###- Excel files cannot be open when running these lines
wqAnn.dat <- do.call(
  rbind,
  lapply(
    file.list, 
    read_ann_WQFun,
    begMo = as.numeric(substr(startMD,1,1)),
    endMo = as.numeric(substr(endMD,1,1)),
    begDay = as.numeric(substr(startMD,3,4)),
    endDay = as.numeric(substr(endMD,3,4))
  )
)

### summarize data to fit model(s)
fit.dat <- ep.dat |> 
  filter(
    res == 'BON'
  ) |> 
  left_join(
    wqAnn.dat |> 
      filter(
        res == 'TDA'
      ),
    by = 'year',
    keep = TRUE
  ) |> 
  bind_rows(
    ep.dat |> 
      filter(
        res == 'TDA'
      ) |> 
      left_join(
        wqAnn.dat |> 
          filter(
            res == 'JDA'
          ),
        by = 'year',
        keep = TRUE
      )
  ) |> 
  bind_rows(
    ep.dat |> 
      filter(
        res == 'JDA'
      ) |> 
      left_join(
        wqAnn.dat |> 
          filter(
            res == 'MCN'
          ),
        by = 'year',
        keep = TRUE
      )
  ) |> 
  bind_rows(
    ep.dat |> 
      filter(
        res == 'LCR'
      ) |> 
      left_join(
        wqAnn.dat |> 
          filter(
            res == 'BON'
          ),
        by = 'year',
        keep = TRUE
      )
  ) |> 
  select(
    -year.y
  ) |> 
  rename(
    year = 1,
    res = 2,
    wqDam = 12
  ) |> 
  relocate(
    12,
    .after = 2
  ) |> 
  mutate(
    wqDam = if_else(
      res == 'BON',
      'TDA',
      if_else(
        res == 'TDA',
        'JDA',
        if_else(
          res == 'JDA',
          'MCN',
          if_else(
            res == 'LCR',
            'BON',
            NA
          )
        )
      )
    )
  )

# fit model(s) ------------------------------------------------------------
## fit full model with fixed effects only
full.modFE <- gamlss(ep ~ outflow + temp + disgas,  
                     family = BEZI, 
                     data = fit.dat |> 
                       na.omit(),
                     trace = F)

## view summary of model fit
summary(full.modFE)

## model selection
### stepwise
mod.sel <- stepGAIC(
  full.modFE,
  direction = 'both',
  scope = list(lower = ~1, upper = ~ outflow + temp + disgas),
  trace = TRUE
)

## review model path
mod.sel$anova

### all subsets
###- this is essentially to ground truth the stepGAIC function (above)
allSSfun()

## fit reduced model
red.modFE <- gamlss(ep ~ outflow + temp,  
                    family = BEZI, 
                    data = fit.dat |> 
                      na.omit(),
                    trace = F) 

summary(red.modFE)

## fit reduced model with year random effect
red.modFEre <- gamlss(
  ep ~ outflow + temp + re(random=~1|year),  
                    family = BEZI, 
                    data = fit.dat |> 
                      na.omit(),
                    trace = F
  )

summary(red.modFEre)

## fit reduced model with year and reservoir random effect
red.modFEre2 <- gamlss(
  ep ~ outflow + temp + re(random=~1 | year) + re(random=~1 | res) ,  
  family = BEZI, 
  data = fit.dat |> 
    na.omit(),
  trace = F
)

summary(red.modFEre2)

## compare models with random effects versus selected model
reCompTbl <- data.frame(
  form = c(
    as.character(red.modFE$call)[2],
    as.character(red.modFEre$call)[2],
    as.character(red.modFEre2$call)[2]
  ),
  AIC = c(
    red.modFE$aic,
    red.modFEre$aic,
    red.modFEre2$aic
  )
) |> 
  arrange(AIC)

### print output table to viewer
html_print(
  pre(
    paste0(
      capture.output(
        print(reCompTbl)
        ), 
      collapse="\n"
      )
    )
  )

# generate and summarize predictions --------------------------------------
## test for independence
# u <- pobs(
#   fit.dat |> 
#     na.omit() |> 
#     select(outflow),
#   fit.dat |> 
#     na.omit() |> 
#     select(temp)
# ) |> 
#   as.copula(dim=2)

## simulations
### structure output data frame
pred.out <- data.frame(
  res = character(),
  year = numeric(),
  outflow = numeric(),
  temp = numeric(),
  pred = numeric(),
  stringsAsFactors = TRUE
)

### specify function to generate predictions
fixPredFun <- function(resNum,i){
  newdata = data.frame(
    outflow = rep(i, 29),
    temp = seq(min(fit.dat$temp, na.rm = TRUE),max(fit.dat$temp, na.rm = TRUE),0.320),
    res = rep(resNum,29),
    year = sample(
      min(
        fit.dat |> 
          na.omit() |> 
          select(year)
      ):
        max(
          fit.dat |> 
            na.omit() |> 
            select(year)
        ),
      29,
      replace = TRUE
    )
  )
  
  pred.dat <- gamlss::predictAll(
    red.modFEre2,
    newdata = newdata,
    what = 'mu',
    type = 'response'
  ) |> 
    as.data.frame() |> 
    select(mu) |> 
    mutate(
      res = newdata$res,
      year = newdata$year,
      outflow = newdata$outflow,
      temp = newdata$temp
    ) |> 
    relocate(
      pred = mu,
      .after = last_col()
    )
  
  pred.out <<- rbind(
    pred.out,
    pred.dat
  )
}

### loop across reservoirs and levels of outflow
outFun <- for(resNum in c('BON','TDA','JDA','LCR')){
  for(i in seq(min(fit.dat$outflow, na.rm = TRUE),max(fit.dat$outflow, na.rm = TRUE),10)){
    fixPredFun(resNum,i)
  }
}

## summarize outputout.summ <- pred.out |>
  filter(
    temp >= 9 & temp <= 10 & pred >= 0.1
  ) |>  
  group_by(res) |> 
  summarise(mOutflow = mean(outflow))
  