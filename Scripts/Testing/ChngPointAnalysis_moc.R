# source functions --------------------------------------------------------
### call packages
source(
  'Functions\\packHandlerFun.R'
)

# setup -------------------------------------------------------------------
## call/install packages
packHandler()

# data steps --------------------------------------------------------------
## read in response var
cpResp.dat <- readRDS(
  'Data\\Response\\EpData.rds'
)

## define URL parameters
### start month
sMonth <- 5
### start day
sDay <- 1
### end month
eMonth <- 7
### end day
eDay <- 31

## extract predictor (outflow)
urlCont <- getURL(
  paste0(
    'https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csv&',
    'year%5B%5D=2024&',
    'year%5B%5D=2023&',
    'year%5B%5D=2022&',
    'year%5B%5D=2021&',
    'year%5B%5D=2020&',
    'year%5B%5D=2019&',
    'year%5B%5D=2018&',
    'year%5B%5D=2017&',
    'year%5B%5D=2016&',
    'year%5B%5D=2015&',
    'year%5B%5D=2014&',
    'year%5B%5D=2013&',
    'year%5B%5D=2012&',
    'year%5B%5D=2011&',
    'year%5B%5D=2010&',
    'year%5B%5D=2009&',
    'year%5B%5D=2008&',
    'year%5B%5D=2007&',
    'year%5B%5D=2006&',
    'year%5B%5D=2005&',
    'year%5B%5D=2004&',
    'year%5B%5D=2003&',
    'year%5B%5D=2002&',
    'year%5B%5D=2001&',
    'year%5B%5D=2000&',
    'year%5B%5D=1999&',
    'year%5B%5D=1998&',
    'year%5B%5D=1997&',
    'year%5B%5D=1996&',
    'year%5B%5D=1995&',
    'year%5B%5D=1994&',
    'year%5B%5D=1993&',
    'year%5B%5D=1992&',
    'year%5B%5D=1991&',
    'year%5B%5D=1990&',
    'year%5B%5D=1989&',
    'loc%5B%5D=MCN&',
    'data%5B%5D=Outflow&',
    'startdate=',
    sMonth,
    '%2F',
    sDay,
    '&enddate=',
    eMonth,
    '%2F',
    eDay,
    '&avgyear=0&',
    'consolidate=1&',
    'grid=1&',
    'y1min=0&',
    'y1max=&',
    'y2min=&',
    'y2max=&',
    'size=large'
  )
)

clean.dat <- gsub('<br />', '', urlCont)

cp.dat <- read.csv(textConnection(clean.dat)) |> 
  head(-4) |>
  filter(
    mm.dd >= '4/1'
  ) |> 
  summarise(
    across(
      where(
        is.numeric
      ), 
      mean
    )
  ) |> 
  pivot_longer(
    cols = starts_with("X"),
    values_to = "meanOut"
  ) |> 
  mutate(
    name = as.numeric(
      substr(
        name,
        2,
        5
      )
    )
  ) |>
  rename(
    Year = 1
  ) |> 
  full_join(
    cpResp.dat
  ) |> 
  mutate(
    cat = if_else(
      Year <= 2011,
      '< 2012',
      '> 2011'
    )
  ) |> 
  arrange(
    Year
  )

# change point analysis ---------------------------------------------------
## Bonneville
### manipulate data
bon_cp.dat <- cp.dat |>
  mutate(
    cat2 = if_else(
      cat == '< 2012',
      0,
      1
    )
  ) |> 
  select(
    BON,
    meanOut,
    cat2
  ) |> 
  na.omit() |>
  rename(
    ep = 1
  ) |> 
  # arrange(
  #   meanOut
  # ) |> 
  # filter(
  #   cat2 == 0
  # ) |>
mutate(
  ep_trans = logit(ep + 0.000001)
) |> 
  arrange(
    ep
  )

# ### two-plateaus model
# ###- define the model
# bon_tp.mod <-  list(
#   BON ~ 1,
#   ~ 1
#   )
# 
# ###- fit model
# bon_tp.mod_fit <- mcp(
#   bon_tp.mod,
#   par_x = 'meanOut',
#   data = bon_cp.dat
# )
# 
# ###- output summary
# summary(
#   bon_tp.mod_fit
# )
# 
# ###- plot output
# bon_tp.plot <- plot(
#   bon_tp.mod_fit,
#   q_fit = TRUE,
#   cp_dens = FALSE
# ) +
#   theme_bw()+
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(
#       color = 'black'
#     ),
#     axis.title.y = element_text(
#       size = 14
#     ),
#     axis.title.x = element_text(
#       size = 14
#     ),
#     axis.text.x = element_text(
#       size = 14
#     ),
#     axis.text.y = element_text(
#       size = 14
#     ),
#     axis.ticks.length=unit(
#       2.0,
#       'mm'
#     ),
#     legend.position = 'none',
#     legend.title = element_blank(),
#     plot.margin = unit(
#       c(0.15,0.20,0.10,0.10), 
#       'in'
#     )
#   ) +
#   labs(
#     x = 'Mean Annual Outflow @ McNary Dam (kcfs)', 
#     y = 'Recruitment Index', 
#     title = 'Bonneville'
#   ) +
#   geom_vline(
#     xintercept = summary(bon_tp.mod_fit)$mean[1]
#   ) +
#   geom_point(size = 3.0)
# 
# png(
#   filename=paste0(
#     'Output\\Figures\\Change Point\\bon_tpPlot',
#     '.png'
#   ),
#   type='cairo',
#   units='in',
#   width=9,
#   height=6,
#   res=300
# )
# 
# print(bon_tp.plot)
# dev.off()
# print(bon_tp.plot)

### change point in slopes
###- detect the number of change points
bon_chDetect = ecp::e.divisive(
  as.matrix(bon_cp.dat$ep),
  sig.lvl = 0.05,
  R = 499
)

bon_chDetect$estimates

###- output summary
summary(
  bon_chDetect
)

###- define the model (based on the number of cps detected)
bon_slp.mod <- list(
  ep ~ 1,
  ~ 0 + meanOut,
  ~ 0 + meanOut
)

###- fit model
bon_slp.mod_fit <- mcp(
  bon_slp.mod,
  data = bon_cp.dat,
  sample = "both", 
  iter = 100000
)

# hypothesis(bon_slp.mod_fit, "cp_2 > 239 & cp_1 < 261")

###- output summary
summary(
  bon_slp.mod_fit
)

###- plot parameters
plot_pars(
  bon_slp.mod_fit,
  regex_pars = "cp_"
  )

###- plot output
bon_slp.plot <- plot(
  bon_slp.mod_fit,
  q_fit = TRUE,
  cp_dens = FALSE
) +
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(
      color = 'black'
    ),
    axis.title.y = element_text(
      size = 14
    ),
    axis.title.x = element_text(
      size = 14
    ),
    axis.text.x = element_text(
      size = 14
    ),
    axis.text.y = element_text(
      size = 14
    ),
    axis.ticks.length=unit(
      2.0,
      'mm'
    ),
    legend.position = 'none',
    legend.title = element_blank(),
    plot.margin = unit(
      c(0.15,0.10,0.10,0.10), 
      'in'
    )
  )+
  labs(
    x = 'Mean Annual Outflow @ McNary Dam (kcfs)', 
    y = 'Ep', 
    title = 'Bonneville'
  ) +
  geom_vline(
    xintercept = summary(bon_slp.mod_fit)$mean[1]
  ) +
  geom_vline(
    xintercept = summary(bon_slp.mod_fit)$mean[2]
  ) +
  geom_point(size = 3.0)

png(
  filename=paste0(
    'Output\\Figures\\Change Point\\bon_slpPlot',
    '.png'
  ),
  type='cairo',
  units='in',
  width=9,
  height=6,
  res=300
)

print(bon_slp.plot)
dev.off()
print(bon_slp.plot)

# ### model selection/comparison
# cpComp_mod1 <- list(
#   ep ~ 1,
#   ~1
#   )
# 
# cpComp_mod2 <- list(
#   ep ~ 0 + meanOut,
#   ~ 0 + meanOut,
#   ~ 0 + meanOut
# )
# 
# cpComp_mod3 <- list(
#   ep ~ 1,
#   ~ 0 + meanOut,
#   ~ 0 + meanOut
# )
#   
# cpComp_fit1 <- mcp(
#   cpComp_mod1,
#   data = bon_cp.dat,
#   sample = "both", 
#   iter = 100000,
#   par_x = 'meanOut'
# )
# 
# plot(cpComp_fit1)
# 
# cpComp_fit2 <- mcp(
#   cpComp_mod2,
#   data = bon_cp.dat,
#   sample = "both", 
#   iter = 100000
# )
# 
# cpComp_fit3 <- mcp(
#   cpComp_mod3,
#   data = bon_cp.dat,
#   sample = "both", 
#   iter = 100000
# )
# 
# plot(cpComp_fit3)
# 
# cpComp_fit1$loo = loo(cpComp_fit1)
# cpComp_fit2$loo = loo(cpComp_fit2)
# cpComp_fit3$loo = loo(cpComp_fit3)
# loo::loo_compare(
#   cpComp_fit1$loo, 
#   # cpComp_fit2$loo,
#   cpComp_fit3$loo
#   )



## The Dalles
### manipulate data
tda_cp.dat <- cp.dat |>
  mutate(
    cat2 = if_else(
      cat == '< 2012',
      0,
      1
    )
  ) |> 
  select(
    TDA,
    meanOut,
    cat2
  ) |> 
  na.omit() |>
  rename(
    ep = 1
  ) |> 
  # arrange(
  #   meanOut
  # ) |> 
  # filter(
  #   cat2 == 0
  # ) |>
  mutate(
    ep_trans = logit(ep + 0.000001)
  ) |> 
  arrange(
    ep
  )

###- detect the number of change points
tda_chDetect <-  ecp::e.divisive(
  as.matrix(tda_cp.dat$ep),
  sig.lvl = 0.05,
  R = 499
)

###- output summary
summary(
  tda_chDetect
)

# ### two-plateaus model
# ###- define the model
# tp_tda.mod <-  list(
#   ep ~ 1,
#   ~ 1
# )
# 
# ###- fit model
# tp.mod_fit <- mcp(
#   tp_tda.mod,
#   par_x = 'meanOut',
#   data = tda_cp.dat
# )
# 
# ###- output summary
# summary(
#   tp.mod_fit
# )
# 
# ###- plot output
# plot(
#   tp.mod_fit,
#   q_fit = TRUE,
#   cp_dens = FALSE
# ) +
#   theme_bw()+
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(
#       color = 'black'
#     ),
#     axis.title.y = element_text(
#       size = 14
#     ),
#     axis.title.x = element_text(
#       size = 14
#     ),
#     axis.text.x = element_text(
#       size = 14
#     ),
#     axis.text.y = element_text(
#       size = 14
#     ),
#     axis.ticks.length=unit(
#       2.0,
#       'mm'
#     ),
#     legend.position = 'none',
#     legend.title = element_blank(),
#     plot.margin = unit(
#       c(0.15,0.10,0.10,0.10), 
#       'in'
#     )
#   )+
#   labs(
#     x = 'Mean Annual Outflow @ McNary Dam (kcfs)', 
#     y = 'Recruitment Index', 
#     title = 'The Dalles'
#   ) +
#   geom_vline(
#     xintercept = summary(tp.mod_fit)$mean[1]
#   ) +
#   geom_point(size = 3.0)

### change point in slopes
###- define the model
slp_tda.mod <- list(
  ep ~ 1,
  ~ 0 + meanOut,
  ~ 0 + meanOut
)

###- fit model
slp_tda.mod_fit <- mcp(
  slp_tda.mod,
  data = tda_cp.dat,
  sample = "both", 
  iter = 100000
)

###- output summary
summary(
  slp_tda.mod_fit
)

###- plot output
tda_slp.plot <- plot(
  slp_tda.mod_fit,
  q_fit = TRUE,
  cp_dens = FALSE
) +
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(
      color = 'black'
    ),
    axis.title.y = element_text(
      size = 14
    ),
    axis.title.x = element_text(
      size = 14
    ),
    axis.text.x = element_text(
      size = 14
    ),
    axis.text.y = element_text(
      size = 14
    ),
    axis.ticks.length=unit(
      2.0,
      'mm'
    ),
    legend.position = 'none',
    legend.title = element_blank(),
    plot.margin = unit(
      c(0.15,0.10,0.10,0.10), 
      'in'
    )
  )+
  labs(
    x = 'Mean Annual Outflow @ McNary Dam (kcfs)', 
    y = 'Ep', 
    title = 'The Dalles'
  ) +
  geom_vline(
    xintercept = summary(slp_tda.mod_fit)$mean[1]
  ) +
  geom_vline(
    xintercept = summary(slp_tda.mod_fit)$mean[2]
  ) +
  geom_point(size = 3.0)

png(
  filename=paste0(
    'Output\\Figures\\Change Point\\tda_slpPlot',
    '.png'
  ),
  type='cairo',
  units='in',
  width=9,
  height=6,
  res=300
)

print(tda_slp.plot)
dev.off()
print(tda_slp.plot)







model2 = list(ep~1, 1~1, 1~1)  # three intercept-only segments

fit_mcp = mcp(model2, data = cp.dat, par_x = "meanOut")
summary(fit_mcp)

plot(fit_mcp)

# Get example data and fit it
ex = mcp_example("demo")
fit = mcp(model2, data = cp.dat)

ex$data

summary(fit)
plot(fit)

model = list(
  ep ~ 1,
  ~ 0 + meanOut + I(meanOut^3)
)
ex = mcp_example("quadratic")
fit = mcp(model, cp.dat)
plot(fit)

# Define the model
model = list(
  response ~ 1,  # plateau (int_1)
  ~ 0 + time,    # joined slope (time_2) at cp_1
  ~ 1 + time     # disjoined slope (int_3, time_3) at cp_2
)

# Get example data and fit it
ex = mcp_example("demo")
fit = mcp(model, data = ex$data)

library(changepoint)

data <- cp.dat$ep
m.pm <- cpt.meanvar(data,penalty = 'SIC',test.stat = 'Exponential')

data[m.pm@cpts]

library(segmented)
fit_lm = lm(ep ~ meanOut, data = tda_cp.dat)  # intercept-only model
fit_segmented = segmented(fit_lm, seg.Z = ~meanOut, npsi = 2)  # Two change points along x

summary(fit_segmented)
plot(fit_segmented)

install.packages("bcp")
