#Sveta Uzhova

#cleaning all 02 scripts from thesis-su repo for reproducibility

#input: all rfu files data/tpc_processed_all_rfus.csv
#output: predicted tpc gr for each flask data/output-norberg.csv

# packages ----------------------------------------------------------------
remotes::install_github("ctkremer/mleTools")
remotes::install_github("ctkremer/growthTools")

library(growthTools)
library(minpack.lm)
library(knitr)
library(rootSolve)
library(broom)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(gridExtra)
library(grid)
library(png)
library(cowplot)
library(purrr)

# data import rfus --------------------------------------------------------
allrfu <- read_csv("data/tpc_processed_all_rfus.csv") %>% 
  mutate(rfu = as.numeric(RFU)) %>% 
  mutate(log_rfu = log(RFU))

# growth rates at all temps except 42 -----------------------------------------------------
allrfu2 <- allrfu %>% 
  filter(treatment != "blank_blank")

allrfu_no42 <- allrfu2 %>% 
  filter(!grepl("_42", unique_well))

unique_well_list <- unique(allrfu_no42$unique_well) 

output <- data.frame(mu = numeric(), se = numeric(), unique_well = character()) #adding treatment = character() creates a column with NA values

for (i in seq_along(unique_well_list)) { 
  tryCatch({
    a.i <- allrfu2 %>%
      filter(unique_well == unique_well_list[i])  # Filter for the specific well
    
    # Compute growth rate
    growth_results <- get.growth.rate(x = a.i$days, 
                                      y = a.i$log_rfu, 
                                      id = a.i$unique_well, 
                                      plot.best.Q = FALSE, 
                                      methods = c('sat'))
    
    # Store results in a data frame
    results <- data.frame(mu = growth_results$best.slope, 
                          se = growth_results$best.se, 
                          unique_well = unique(a.i$unique_well))
    
    # Append results to output
    output <- bind_rows(output, results)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", conditionMessage(e)))
    # Continue to the next iteration without stopping the loop
  })
}

# Return the final output after the loop completes
output

#write_csv(output, "data/output-no42.csv")
output_no42 <- read.csv("data/output-no42.csv")

# growth rate at 42 deg ---------------------------------------------------
gdat <- allrfu %>% 
  group_by(unique_well, treatment) %>% 
  do(grs=get.growth.rate(x=.$days,y=.$log_rfu,
                         id=.$unique_well,plot.best.Q=F))  

sumgdat <- gdat %>% summarise(unique_well, treatment, mu=grs$best.slope,best.model=grs$best.model,
                              best.se=grs$best.se)
View(sumgdat)

gdat42 <- sumgdat %>% 
  filter(grepl("42", unique_well)) %>% 
  filter(treatment != "blank_blank")

#write_csv(gdat42, "data/gdat42.csv")
output_only42 <- read.csv("data/gdat42.csv")

# combining growth rates at all temps -------------------------------------
col_no42 <- data.frame(allrfu_no42$unique_well, allrfu_no42$treatment) %>% 
  rename("unique_well" = "allrfu_no42.unique_well") %>% 
  rename("treatment" = "allrfu_no42.treatment")

together_no42 <- left_join(output_no42, col_no42)

output_only42 <- output_only42 %>%
  rename ("se" = "best.se") %>%
  select(-"best.model")

mock_ouput <- bind_rows(together_no42, output_only42)
View(mock_ouput)

mock_ouput <- mock_ouput %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp"), sep = "_") %>% 
  unite(plate, well, col = "unique_well", sep = "_")


m2 <- mock_ouput %>% 
  distinct()

# fitting tpc -------------------------------------------------------------
get_topt <- function(df){
  grfunc<-function(x){
    -nbcurve(x, z = df$z[[1]],w = df$w[[1]],a = df$a[[1]],b = df$b[[1]])
  }
  optinfo<-optim(c(x=df$z[[1]]),grfunc)
  opt <-c(optinfo$par[[1]])
  maxgrowth <- c(-optinfo$value)
  results <- data.frame(topt = opt, rmax = maxgrowth)
  return(results)
}

get_tmax <- function(df){
  uniroot.all(function(x) nbcurve(x, z = df$z[[1]],w = df$w[[1]],a = df$a[[1]], b = df$b[[1]]),c(30,150))
}

get_tmin <- function(df){
  uniroot.all(function(x) nbcurve(x, z = df$z[[1]],w = df$w[[1]],a = df$a[[1]], b = df$b[[1]]),c(-40,25))
}


nbcurve<-function(temp,z,w,a,b){
  res<-a*exp(b*temp)*(1-((temp-z)/(w/2))^2)
  res
}

prediction_function <- function(df) {
  tpc <-function(x){
    res<-(df$a[[1]]*exp(df$b[[1]]*x)*(1-((x-df$z[[1]])/(df$w[[1]]/2))^2))
    res
  }
  
  pred <- function(x) {
    y <- tpc(x)
  }
  
  x <- seq(0, 50, by = 0.1)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    dplyr::rename(temperature = x, 
                  growth = preds)
}

m2 %>% 
  mutate(temperature = as.numeric(tpctemp)) %>% 
  ggplot(aes(x = temperature, y = mu, color = treatment)) + geom_point() +
  facet_wrap(~ treatment)

fitting_function <- function(df) {
  
  fit1 <- nlsLM(mu ~ a*exp(b*temp)*(1-((temp-z)/(w/2))^2),
                data = df,
                start= list(z= 25,w= 25,a= 0.2, b= 0.1),
                lower = c(z = -20, w= 0, a = -0.2, b = 0),
                upper = c(z = 40, w= 120,a =  2, b = 2),
                control = nls.control(maxiter=1024, minFactor=1/204800000))
  
  out_1 <- tidy(fit1) %>% 
    select(estimate, term) %>%  
    spread(key = term, value = estimate)
  
  preds_ind10 <- prediction_function(out_1)
  
  topt <- get_topt(out_1)
  tmin <- get_tmin(out_1)
  tmax <- get_tmax(out_1)
  
  output <- bind_cols(temp = preds_ind10$temperature, predicted_growth = preds_ind10$growth, topt = topt, tmax = tmax)
  return(output)
  
}


df_split <- m2 %>% 
  mutate(temp = as.numeric(tpctemp)) %>% 
  split(.$treatment)


output_all <- df_split %>% 
  map_df(fitting_function, .id = "treatment")

#write_csv(output_all, "data/output-norberg.csv")
