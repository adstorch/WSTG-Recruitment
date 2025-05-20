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
  'EnvStats'
)

### install or load packages
if (!require(install.load)) {
  install.packages('install.load')
}

install.load::install_load(packages)

# install.packages("remotes")
# remotes::install_github("samclifford/mgcv.helper")

## source functions
### daily summary
source(
  'Functions\\readWQdailyFun.R'
)

### annual summary
source(
  'Functions\\readWQannFun.R'
)

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
wqDaily.dat <- do.call(
  rbind,
  lapply(
  file.list, 
  read_daily_WQFun
  )
)

### consolidate data on an annual time step
wqAnn.dat <- do.call(
  rbind,
  lapply(
    file.list, 
    read_ann_WQFun
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
###- this is essentially to groundtruth the stepGAIC function (above)
mod1 <- gamlss(ep ~ outflow + temp + disgas,  
               family = BEZI, 
               data = fit.dat |> 
                 na.omit(),
               trace = F)

mod1.out <- data.frame(
  mod = 'mod1',
  AIC = mod1$aic,
  form = as.character(mod1$call)[2]
)

mod2 <- gamlss(ep ~ outflow + temp,  
               family = BEZI, 
               data = fit.dat |> 
                 na.omit(),
               trace = F)

mod2.out <- data.frame(
  mod = 'mod2',
  AIC = mod2$aic,
  form = as.character(mod2$call)[2]
)

mod3 <- gamlss(ep ~ outflow + disgas,  
               family = BEZI, 
               data = fit.dat |> 
                 na.omit(),
               trace = F)

mod3.out <- data.frame(
  mod = 'mod3',
  AIC = mod3$aic,
  form = as.character(mod3$call)[2]
)

mod4 <- gamlss(ep ~ temp + disgas,  
               family = BEZI, 
               data = fit.dat |> 
                 na.omit(),
               trace = F)

mod4.out <- data.frame(
  mod = 'mod4',
  AIC = mod4$aic,
  form = as.character(mod4$call)[2]
)

mod5 <- gamlss(ep ~ outflow,  
               family = BEZI, 
               data = fit.dat |> 
                 na.omit(),
               trace = F)

mod5.out <- data.frame(
  mod = 'mod5',
  AIC = mod5$aic,
  form = as.character(mod5$call)[2]
)

mod6 <- gamlss(ep ~ temp,  
               family = BEZI, 
               data = fit.dat |> 
                 na.omit(),
               trace = F)

mod6.out <- data.frame(
  mod = 'mod6',
  AIC = mod6$aic,
  form = as.character(mod6$call)[2]
)

mod7 <- gamlss(ep ~ disgas,  
               family = BEZI, 
               data = fit.dat |> 
                 na.omit(),
               trace = F)

mod7.out <- data.frame(
  mod = 'mod7',
  AIC = mod7$aic,
  form = as.character(mod7$call)[2]
)

ssComp <- bind_rows(
  mod1.out,
  mod2.out,
  mod3.out,
  mod4.out,
  mod5.out,
  mod6.out,
  mod7.out,
) |> 
  arrange(AIC) |> 
  print()

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
  ep ~ outflow + temp + re(random=~1|year) + re(random=~1|res),  
  family = BEZI, 
  data = fit.dat |> 
    na.omit(),
  trace = F
)

summary(red.modFEre2)




# generate and summarize predictions --------------------------------------
## estimate distribution parameters for random effects
### output estimates of random effects from model
###- year
year.refs <- red.modFEre$mu.coefSmo[[1]]$coefficients$random |> 
  as.data.frame() |> 
  rename(
    est = 1
  )

###- reservoir
res.refs <- red.modFEre$mu.coefSmo[[2]]$coefficients$random |> 
  as.data.frame() |> 
  rename(
    est = 1
  )

### estimate parameters for normal distribution
###- year
ranefsYear.params <- enorm(
  method = "mle",
  year.refs |>
    pull(est) 
)

###- res
ranefsRes.params <- enorm(
  method = "mle",
  res.refs |>
    pull(est) 
)

i <- 318.8288
resNum <- 'LCR'
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
  19,
  replace = TRUE
)
      
      
      
      ):max(fit.dat$year), 19, replace = TRUE)

seq(min(fit.dat$outflow, na.rm = TRUE),max(fit.dat$outflow, na.rm = TRUE),15)


pred.out <- data.frame(
  res = character(),
  year = numeric(),
  outflow = numeric(),
  temp = numeric(),
  pred = numeric(),
  stringsAsFactors = TRUE
)

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
    red.modFEre,
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

outFun <- for(resNum in c('BON','TDA','JDA','LCR')){
  for(i in seq(min(fit.dat$outflow, na.rm = TRUE),max(fit.dat$outflow, na.rm = TRUE),10)){
    fixPredFun(resNum,i)
  }
}
  
out.summ <- pred.out |> 
  group_by(
    flow = outflow
    ) |> 
  summarize(
      pred = mean(pred),
      meanTemp = mean(temp)
  )




  x <-  pred.out$outflow
  y <-  pred.out$temp
  z <-  pred.out$pred

x <- -10:10
y <- -10:10
z <- sqrt(outer(x ^ 2, y ^ 2, "+"))

contour(x, y, z,
        nlevels = 20) 

library(rgl)
persp3d(x, y, z, col="skyblue")


  function(){
  pred.out <- lapply(
    seq(min(fit.dat$outflow, na.rm = TRUE),max(fit.dat$outflow, na.rm = TRUE),2),
    fixPredFun,
    arg1 = 'BON'
  )
  print(pred.out)
}



gamlss::predictAll(
  red.modFEre,
  newdata = data.frame(outflow = 318.8288, temp = 18.44777, year = 2002, res = 'BON')
  )

max(fit.dat$temp, na.rm = TRUE)

seq(min(fit.dat$temp, na.rm = TRUE),max(fit.dat$temp, na.rm = TRUE),0.320)
seq(min(fit.dat$outflow, na.rm = TRUE),max(fit.dat$outflow, na.rm = TRUE),10)

seq(9,19,0.55)

  str(fit.dat)
  
  library(gtools)
  
  
  
  string <- "abc"
  
  permutations(string)
  
  string <- c('out','temp','gas')

  combn(string,2)
  