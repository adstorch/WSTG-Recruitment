# assess all subset models of a full model fit with three covariat --------
## this function should be made more generic
allSSfun <- function(){
  mod0 <- gamlss(ep ~ 1,  
                 family = BEZI, 
                 data = fit.dat |> 
                   na.omit(),
                 trace = F)
  
  mod0.out <- data.frame(
    mod = 'mod0',
    AIC = mod0$aic,
    form = as.character(mod0$call)[2]
  )
  
  mod1 <- gamlss(ep ~ outflow + temp_c + disgas,  
                 family = BEZI, 
                 data = fit.dat |> 
                   na.omit(),
                 trace = F)
  
  mod1.out <- data.frame(
    mod = 'mod1',
    AIC = mod1$aic,
    form = as.character(mod1$call)[2]
  )
  
  mod2 <- gamlss(ep ~ outflow + temp_c,  
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
  
  mod4 <- gamlss(ep ~ temp_c + disgas,  
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
  
  mod6 <- gamlss(ep ~ temp_c,  
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
    mod0.out,
    mod1.out,
    mod2.out,
    mod3.out,
    mod4.out,
    mod5.out,
    mod6.out,
    mod7.out,
  ) |> 
    arrange(AIC)
  
  ### print output table to viewer
  html_print(
    pre(
      paste0(
        capture.output(
          print(ssComp)
        ), 
        collapse="\n"
      )
    )
  )
    
}
