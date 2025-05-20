# source functions --------------------------------------------------------
### call packages
source(
  'Functions\\packHandlerFun.R'
)

### fetch covariate data from DART
source(
  'Functions\\fetchWQfun.R'
)

# ### model selection (all subsets)
# source(
#   'Functions\\modSelFun.R'
# )

# setup -------------------------------------------------------------------
## call/install packages
packHandler()

# read/manipulate data ----------------------------------------------------
## response var (Ep)
ep.dat <- readRDS(
  'Data\\Response\\EpData.rds'
) |> 
  pivot_longer(cols = 2:9) |> 
  arrange(name,Year) |> 
  rename(
    year = 1,
    res = 2,
    ep = 3
  )

## independent vars
### retrieve data
###- specify function parameters as list
params <- list(
  #- projects from which to retrieve data
  ##- comment out the ones you don't want
  c(
    ##- Bonneville
    'CCIW',
    ##- The Dalles
    'TDDO',
    ##- John Day
    'JHAW',
    ##- McNary
    'MCPW'
  ),
  #- beginning year
  1989,
  #- ending year
  2024,
  #- beginning month/day
  '5/1',
  #- ending month/day
  '7/31'
)

###- query DART and output data
do.call(
  EpCovFetch_wrap,
  params
  )

### consolidate data on an annual time step for each project
wqAnn.dat <- inp.dat |> 
  group_by(
    across(
      all_of(
        c(
          'res',
          'year'
        )
      )
    )
  ) |>
  summarize(
    across(
      outflow:elev,
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = 'drop'
  )

## summarize data to fit model(s)
fit.dat <- ep.dat |> 
  filter(
    res == 'BON'
  ) |> 
  left_join(
    wqAnn.dat |> 
      filter(
        res == 'TDDO'
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
            res == 'JHAW'
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
            res == 'MCPW'
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
            res == 'CCIW'
          ),
        by = 'year',
        keep = TRUE
      )
  ) |> 
  select(
    -year.y,
    -res.y
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
    ),
    wqRef = if_else(
      wqDam == 'TDA',
      'TDDO',
      if_else(
        wqDam == 'JDA',
        'JHAW',
        if_else(
          wqDam == 'MCN',
          'MCPW',
          if_else(
            wqDam == 'BON',
            'CCIW',
            NA
          )
        )
      )
    ) 
  ) |> 
  filter(
    wqRef %in% params[[1]]
    ) |> 
  select(
    -wqRef,
    -temp_sc,
    -turb
  )

# fit model(s) ------------------------------------------------------------
## fit full model with fixed effects only
full.modFE <- gamlss(ep ~ outflow + temp_c + disgas,  
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
  scope = list(lower = ~1, upper = ~ outflow + temp_c + disgas),
  trace = TRUE
)

## review model path
mod.sel$anova

# ### all subsets
# ###- this is essentially to ground truth the stepGAIC function (above)
# allSSfun()

## fit reduced model
red.modFE <- gamlss(ep ~ outflow + temp,  
                    family = BEZI, 
                    data = fit.dat |> 
                      na.omit(),
                    trace = F) 

summary(red.modFE)

## fit reduced model with year random effect
red.modFEre <- gamlss(
  ep ~ outflow + temp_c + re(random=~1|year),  
                    family = BEZI, 
                    data = fit.dat |> 
                      na.omit(),
                    trace = F
  )

summary(red.modFEre)

## fit reduced model with year and reservoir random effect
red.modFEre2 <- gamlss(
  ep ~ outflow + temp_c + re(random=~1 | year) + re(random=~1 | res) ,  
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
#     select(temp_c)
# ) |> 
#   as.copula(dim=2)

## simulations
### structure output data frame
pred.out <- data.frame(
  res = character(),
  year = numeric(),
  outflow = numeric(),
  temp_c = numeric(),
  pred = numeric(),
  stringsAsFactors = TRUE
)

### specify function to generate predictions
fixPredFun <- function(resNum,i){
  newdata = data.frame(
    outflow = rep(i, 29),
    temp_c = seq(min(fit.dat$temp_c, na.rm = TRUE),max(fit.dat$temp_c, na.rm = TRUE),0.320),
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
      temp_c = newdata$temp_c
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

## summarize output
out.summ <- pred.out |>
  filter(
    temp_c >= 9 & temp_c <= 10 & pred >= 0.1
  ) |>  
  group_by(res) |> 
  summarise(mOutflow = mean(outflow))
  