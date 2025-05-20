# source functions --------------------------------------------------------
### call packages
source(
  'Functions\\packHandlerFun.R'
)

# setup -------------------------------------------------------------------
## call/install packages
packHandler()

ep.dat <- readRDS(
  'Data\\Response\\EpData.rds'
)

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
    'startdate=4%2F1&',
    'enddate=7%2F31&',
    'avgyear=0&',
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

rb.dat <- read.csv(textConnection(clean.dat)) |> 
  head(-4) |>
  filter(
    mm.dd >= '5/1'
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
    ep.dat
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

bonFun <- function(x){
  0.67/(1 + exp(-(x-223.86)/15.38))
}

bon.sims <- lapply(
  seq(100,410,1),
  bonFun
) |>
  as.data.frame() |>
  t() |> 
  as.data.frame() |> 
  remove_rownames() |> 
  rename(
    sims = 1
  ) |> 
  mutate(
    outflow = seq(100,410,1)
  )

tdaFun <- function(x){
  0.64/(1 + exp(-(x-245.18)/24.52))
}

tda.sims <- lapply(
  seq(100,410,1),
  tdaFun
) |>
  as.data.frame() |>
  t() |> 
  as.data.frame() |> 
  remove_rownames() |> 
  rename(
    sims = 1
  ) |> 
  mutate(
    outflow = seq(100,410,1)
  )

# plots -------------------------------------------------------------------
## Bonneville
bonEpFlow.plot <- ggplot(
  data = na.omit(
    dplyr::select(
      rb.dat,
      c(
        Year,
        meanOut,
        BON,
        cat
      )
    )
  ) |> dplyr::filter(Year <= 2024),
  aes(
    x = meanOut,
    y = BON
  )
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
    legend.position = 'right',
    legend.title = element_blank(),
    plot.margin = unit(
      c(0.15,0.10,0.10,0.10), 
      'in'
    )
  )+
  labs(
    x = 'Mean Annual Outflow @ McNary Dam (kcfs)', 
    y = 'Recruitmentr Index', 
    title = 'Bonneville'
  ) +
  theme(
    plot.title = element_text(hjust = 0.0,size = 16)
  ) +
  geom_point(
    aes(
      colour = factor(cat)
    ),
    size = 4.0
  ) +
  geom_line(
    data = bon.sims,
    aes(
      x = outflow, 
      y = sims
    ), 
    linewidth = 1.0) +
  # geom_smooth(
  #   formula = y ~ s(x, k = 4, bs = 'cs'),
  #   method = 'gam',
  #   linewidth = 0.7
  # ) +
  stat_smooth(
    formula = y ~ x,
    method='glm',
    color='orange', 
    se=FALSE,
    method.args = list(family=binomial)
  ) +
  scale_y_continuous(
    limits=c(
      0.0,
      1.0
      ),
    breaks = seq(
      0.0,
      1.0,
      0.2
      )
  ) +
  scale_x_continuous(
    limits=c(100,410),
    breaks = seq(100,410,50)
  )

png(
  filename=paste0(
    'Output\\Figures\\RB Figures\\bonRelPlot',
    '.png'
    ),
    type='cairo',
    units='in',
    width=9,
    height=6,
    res=300
  )

print(bonEpFlow.plot)
dev.off()
print(bonEpFlow.plot)

## The Dalles
tdaEpFlow.plot <- ggplot(
  data = na.omit(
    dplyr::select(
      rb.dat,
      c(
        Year,
        meanOut,
        TDA,
        cat
      )
    )
  ) |> dplyr::filter(Year <= 2024),
  aes(
    x = meanOut,
    y = TDA
  )
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
    legend.position = 'right',
    legend.title = element_blank(),
    plot.margin = unit(
      c(0.15,0.10,0.10,0.10),
      'in')
  )+
  labs(
    x = 'Mean Annual Outflow @ McNary Dam (kcfs)',
    y = 'Recruitmentr Index', 
    title = 'The Dalles'
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.0,
      size = 16
    )
  ) +
  geom_point(
    aes(
      colour = factor(cat)
    ),
    size = 4.0
    ) +
  geom_line(
    data = tda.sims,
    aes(
      x = outflow, 
      y = sims
    ),
    linewidth = 1.0
  ) +
  # geom_smooth(
  #   formula = y ~ s(x, k = 4, bs = 'cs'),
  #   method = 'gam',
  #   linewidth = 0.7
  # ) +
  stat_smooth(
    formula = y ~ x,
    method='glm', 
    color='orange', 
    se=FALSE,
    method.args = list(family=binomial)
  ) +
  scale_y_continuous(
    limits=c(0.0,1.0),
    breaks = seq(0.0,1.0,0.2)
  ) +
  scale_x_continuous(
    limits=c(100,410),
    breaks = seq(100,410,50)
  )

png(
  filename=paste0(
    'Output\\Figures\\RB Figures\\tdaRelPlot',
    '.png'
    
    ),
    type='cairo',
    units='in',
    width=9,
    height=6,
    res=300)

print(tdaEpFlow.plot)
dev.off()
print(tdaEpFlow.plot)