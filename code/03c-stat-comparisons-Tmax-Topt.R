#Sveta Uzhova
#T opt and T max

#Note: original script (03-stat-comparisons) uses all treatments (14C, 30C, 6F, 48F)
#Note: here, only the constants are copied

# packages ----------------------------------------------------------------
library(car)

# data --------------------------------------------------------------------
#import
output_norberg <- read.csv("data/output-norberg.csv")

#cleaning up
output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")

#visualise
output_norberg2 %>% 
  rename("Treatment" = "incubator") %>% 
  ggplot(aes(temp, predicted_growth, colour = Treatment)) + 
  geom_line() + 
  ylim(0, 1.5) + 
  theme_minimal() + 
  xlab("Temperature") + 
  ylab("Predicted growth") + 
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    strip.text = element_text(face = "bold")  # Makes facet labels (temp) bold
  )

###########################################################################
# T max -------------------------------------------------------------------
# make T max
#using r max as basis
unique_tmax_df <- output_norberg2 %>%
  distinct(tmax, .keep_all = TRUE)

# test for normality ------------------------------------------------------
unique_tmax_df %>% 
  filter(incubator == "14C") %>% 
  pull(tmax) %>%  
  shapiro.test() #W = 0.91431, p-value = 0.3854
unique_tmax_df %>% 
  filter(incubator == "30C") %>% 
  pull(tmax) %>%  
  shapiro.test() #W = 0.94291, p-value = 0.6399

# ANOVA -------------------------------------------------------------------
#test for variance
leveneTest(tmax ~ incubator, data = unique_tmax_df)
#homogeneity of variances is met

#ANOVA
tmax_anova <- aov(tmax ~ incubator, data = unique_tmax_df)
summary(tmax_anova)
#F = 0.863, num df = 3.000, denom df = 28, p-value = 0.472
#NO sig diff between t max among incubators

###########################################################################
# T opt -------------------------------------------------------------------
#make T opt
#using r max as basis
unique_topt_df <- output_norberg2 %>%
  distinct(topt, .keep_all = TRUE)

# test for normality ------------------------------------------------------
unique_topt_df %>% 
  filter(incubator == "14C") %>% 
  pull(topt) %>%  
  shapiro.test() #W = 0.96107, p-value = 0.8203
unique_topt_df %>% 
  filter(incubator == "30C") %>% 
  pull(topt) %>%  
  shapiro.test() #W = 0.94653, p-value = 0.6762

# ANOVA -------------------------------------------------------------------
#test for variance
leveneTest(tmax ~ incubator, data = unique_tmax_df)
#homogeneity of variances is met

#ANOVA
topt_anova <- aov(topt ~ incubator, data = unique_topt_df, var.equal = FALSE)
summary(topt_anova)
#F = 3.956, p = 0.018
#sig diff between t max among incubators

# Tukey HSD ---------------------------------------------------------------
topt_tukey_result <- TukeyHSD(topt_anova)
print(topt_tukey_result)
#only sig is 30C>14C, p = 0.0099190


