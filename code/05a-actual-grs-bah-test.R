#Sveta Uzhova
#ACTUAL growth rates at 14 deg and at 30 deg

# packages ----------------------------------------------------------------
library(ggpubr)

# data --------------------------------------------------------------------
#import
actual <- read.csv("data/growthtool-gdat-sum.csv")

#cleaning up
actual <- actual %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp")) %>% 
  unite("unique_well", "plate", "well", sep = "_") %>% 
  rename("temp" = "tpctemp")

#pull out constant at 14 and 30 deg
###14 deg
act_t14C_14deg <- actual %>%
  filter(incubator == "14C", temp == 14.0) %>%
  select(incubator, flask, mu)
act_t14C_14deg$mu <- as.numeric(act_t14C_14deg$mu)

act_t30C_14deg <- actual %>%
  filter(incubator == "30C", temp == 14.0) %>%
  select(incubator, flask, mu)
act_t30C_14deg$mu <- as.numeric(act_t30C_14deg$mu)

###30 deg
act_t14C_30deg <- actual %>%
  filter(incubator == "14C", temp == 30.0) %>%
  select(incubator, flask, mu)
act_t14C_30deg$mu <- as.numeric(act_t14C_30deg$mu)

act_t30C_30deg <- actual %>%
  filter(incubator == "30C", temp == 30.0) %>%
  select(incubator, flask, mu)
act_t30C_30deg$mu <- as.numeric(act_t30C_30deg$mu)

# test for normality ------------------------------------------------------
###14 deg
shapiro.test(act_t14C_14deg$mu) #W = 0.96331, p-value = 0.5083
shapiro.test(act_t30C_14deg$mu) #W = 0.93776, p-value = 0.1454

###30 deg
shapiro.test(act_t14C_30deg$mu) #W = 0.96316, p-value = 0.505
shapiro.test(act_t30C_30deg$mu) #W = 0.8792, p-value = 0.00801
#not normally distributed
# t test ------------------------------------------------------------------
###14 deg
t.test(act_t14C_14deg$mu, act_t30C_14deg$mu) #p-value = 2.12e-12
t.test(act_t14C_14deg$mu, act_t30C_14deg$mu, alternative = "greater") #p-value = 1.06e-12
#act: 14C is sig greater than 30C at 14 deg

# Wilcox ------------------------------------------------------------------
###30 deg
wilcox.test(act_t14C_30deg$mu, act_t30C_30deg$mu) #W = 507, p-value = 1.295e-06
#30 different

# graphing with chat ------------------------------------------------------
###Note: final figure is in script 04
# Prepare data
act_plot_data <- actual %>%
  filter(incubator %in% c("14C", "30C")) %>% 
  filter(temp %in% c(14.0, 30.0)) %>%
  select(incubator, temp, flask, mu) %>%
  mutate(temp = factor(temp, levels = c(14, 30)))  # Ensure proper ordering

# Manually define p-values for t-test and Wilcoxon
p_values <- data.frame(
  temp = factor(c(14, 30)),  # Ensure matching factor levels
  group1 = "14C",
  group2 = "30C",
  p = c(1.06e-12, 1.295e-06),  # t-test for 14°C, Wilcoxon for 30°C
  label = c("***", "***")  # Significance stars
)

# Plot with manual significance labels
ggplot(act_plot_data, aes(x = incubator, y = mu, fill = incubator)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +  # Scatter points
  facet_wrap(~temp) +  # Separate panels for each temp
  theme_minimal() +
  labs(
    x = "Incubator",
    y = "Predicted growth",
    fill = "Incubator"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823")) +  # Custom colors
  geom_text(data = p_values, aes(x = 1.5, y = max(act_plot_data$mu) + 0.1, label = label), inherit.aes = FALSE)
