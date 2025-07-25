#Sveta Uzhova
#T breadth and r max

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
# T breadth ---------------------------------------------------------------
#make T breadth
output_norberg3 <- output_norberg2 %>%
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>% 
  separate(col = trt, into = c("incubator", "flask"), sep = "_")

#visualise
output_norberg3 %>% 
  ggplot(aes(incubator, t_breadth)) + geom_point()

# test for normality ------------------------------------------------------
shapiro.test(output_norberg3$t_breadth) #W = 0.99237, p-value = 0.9978

output_norberg3 %>% 
  filter(incubator == "14C") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.98135, p-value = 0.9693
output_norberg3 %>% 
  filter(incubator == "30C") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.96121, p-value = 0.8216

# ANOVA -------------------------------------------------------------------
#test for variance
leveneTest(t_breadth ~ incubator, data = output_norberg3)
#variances are assumed to be homogeneous

#ANOVA
tbread_anova <- aov(t_breadth ~ incubator, data = output_norberg3)
summary(tbread_anova)

# Tukey HSD test ----------------------------------------------------------
tukey_result <- TukeyHSD(tbread_anova)
print(tukey_result)
#significant p values: 30C-14C, 48F-14C, 6F-30C

###########################################################################
# r max -------------------------------------------------------------------
#make r max
unique_rmax_df <- output_norberg2 %>%
  distinct(rmax, .keep_all = TRUE)

# test for normality ------------------------------------------------------
unique_rmax_df %>% 
  filter(incubator == "14C") %>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.82612, p-value = 0.05408
unique_rmax_df %>% 
  filter(incubator == "30C") %>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.94799, p-value = 0.6909

# Welch ANOVA -------------------------------------------------------------
#test for variance
leveneTest(rmax ~ incubator, data = unique_rmax_df)
#homogeneity of variances is violated

#ANOVA
#Welch’s ANOVA is a robust alternative to ANOVA that does not assume equal variances
oneway.test(rmax ~ incubator, data = unique_rmax_df, var.equal = FALSE)
#F = 15.797, num df = 3.000, denom df = 15.315, p-value = 5.944e-05
#sig diff between r max among incubators

# post hoc: Games Howell --------------------------------------------------
games_howell_results <- unique_rmax_df %>%
  games_howell_test(rmax ~ incubator)
View(games_howell_results)
#significant pairs: 14C-30C, 14C-48F, 14C-6F
