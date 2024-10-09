setwd("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Data")

library(readxl)
library(dplyr)
library(tidyverse)
library(purrrlyr)
library(ggpubr)
library(ggplot2)
library(tidyr)
library(forcats)

my_comparisons <- list( c("CP_O", "CP_Y"), c("CP_O", "TD_O"), c("CP_O", "TD_Y") )

vo2 <- read_excel("Anthropometrics.xlsx", 
                      sheet = "vo2")

#vo2_mean <- subset(vo2, weight >0) %>%
 #     group_by(group) %>%
  #    summarise_at(vars(vo2_peak_norm,six_mwt_oxy_norm, treadmill), 
   #                list(name = mean, sd = sd), na.rm = TRUE)

# Figures -----------------------------------------------------------------

# Try to do one colour for the CP groups and one for TD's with differences in gradients
# Keep a differnece between men and women

# VO2-max normalized

vo2_norm <- ggplot(data = subset(vo2, group !="0"), 
                   aes(x = group, y = vo2_peak_norm, col = group)) +
      geom_point(aes(shape = as.factor(pairs)), 
                 position = position_dodge(width=.5), size = 5, alpha = 0.6) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(15,85)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = expression('VO'['2']*'max normalized to weight'),
           subtitle = expression('VO'['2']*'max normalized to weight'))
vo2_norm


# VO2-peak normalized

vo2_peak_norm <- ggplot(data = subset(vo2, group !="0"), 
                  aes(x = group, y = vo2_peak_norm, col = group)) +
      geom_point(aes(shape = as.factor(pairs)), 
                 position = position_dodge(width=.5), size = 5, alpha = 0.6) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(10,85)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = expression('VO'['2']*'peak normalized to weight [L/min/kg]'),
           title = expression('VO'['2']*'peak normalized to weight'))
vo2_peak_norm
ggsave(dpi = 600, scale = 1.5,
       "V02 normalized to weight.tiff")

# raw vo2-max

vo2_max <- ggplot(data = subset(vo2, group !="0"), 
                  aes(x = group, y = vo2max, position = id, col = sex)) +
      geom_point(position=position_dodge(width=0.2)) +
      geom_boxplot(aes(position = group), alpha = 0.1, color = "grey") + 
      theme_pubr(legend = "none") +
      labs(x = "Group", y = expression('VO'['2']*'max'),
           subtitle = "")
vo2_max

# VO2-peak normalized

vo2_peak <- ggplot(data = subset(vo2, group !="0"), 
                   aes(x = group, y = vo2peak, position = id, col = sex)) +
      geom_point(position=position_dodge(width=0.2)) +
      geom_boxplot(aes(position = group), alpha = 0.1, color = "grey") + 
      theme_pubr(legend = "none") +
      labs(x = "Group", y = expression('VO'['2']*'peak'),
           subtitle = "")
vo2_peak
ggsave(dpi = 600, scale = 1.5,
       "V02 peak.tiff")

# ventilation max

ventilation <- ggplot(data = subset(vo2, group !="0"), 
                  aes(x = group, y = ventilation_max, position = id, col=sex)) +
      geom_point(position=position_dodge(width=0.2)) +
      geom_boxplot(aes(position = group), alpha = 0.1, color = "grey") + 
      theme_pubr(legend = "none") +
      labs(x = "Group", y = ('Maximal ventilation in l/min'),
           subtitle = "Ventilation expressed in liter / minute")
ventilation
ggsave(dpi = 600, scale = 1.5,
       "V02 maximal ventilation.tiff")


# Walking trials figures --------------------------------------------------

# Here the figures connecting vo2-max measures to the walking trials. 
# To make these figures, I pivot my table to long format and change some
# variable into factors

long_vo2 <- dplyr::select(vo2,c(1:2,13,30:32,34,36, 44)) %>%
      rename("Treadmill at 100 %" = 4,
             "Treadmill at 150 %" = 5,
             "Treadmill at 120 %" = 6,
             "Overground walking" = 7,
             "Arm support needed" = 8) %>%
      pivot_longer(cols = c(4:7))
long_vo2$name <- as.factor(long_vo2$name)
long_vo2$`Arm support needed` <- as.factor(long_vo2$`Arm support needed`)


walking_percentage <- ggplot(data = subset(long_vo2, group !="0" & value !=0), 
                  aes(x = name, y = value, col = group, position = group)) +
      geom_point(aes(group = id, shape = as.factor(pairs)),
                 position=position_dodge(width=.75), alpha = 1, size = 2.5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.75)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.75)) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      labs(y = expression('Percentage of VO'['2']*'peak'), x = "",
           subtitle = "")
walking_percentage
ggsave(dpi = 600, scale = 1.5,
       "Walking % of vo2peak.tiff")

long_cow <- dplyr::select(vo2,c(1:2, 41, 43:44, 48:50)) %>%
      rename("Overground walking" = 3,
             "Treadmill at 150 %" = 7,
             "Treadmill at 120 %" = 8,
             "Treadmill at 100 %" = 6) %>%
      pivot_longer(cols = c(3,6,7,8))
long_cow$name <- as.factor(long_cow$name)

walking_cow <- ggplot(data = subset(long_cow, group !="0" & value !=0), 
                      aes(x = name, y = value, col = group, position = group)) +
      geom_point(aes(group = id, shape = as.factor(pairs)),
                 position=position_dodge(width=.75), alpha = 1, size = 2.5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.75)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.75)) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      labs(y = expression('Caloric cost of walking one meter [J/kg/m]'), x = "",
           subtitle = "")
walking_cow
ggsave(dpi = 600, scale = 1.5,
       "All CoW.tiff")

overground <- vo2_peak_norm <- ggplot(data = subset(vo2, group !="0"), 
                                      aes(x = group, y = energy_CoW, col = group)) +
      geom_point(aes(shape = as.factor(pairs)), 
                 position = position_dodge(width=.5), size = 5, alpha = 0.6) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(1,14)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = expression('Caloric cost of walking one meter [J/kg/m]'),
           title = expression('Cost of overground walking during the six-minute-walking test'))
overground
ggsave(dpi = 600, scale = 1.5,
       "overground CoW.tiff")



# Statistical analysis of CoW ---------------------------------------------

# Testing group and between group differences in % of max on a treadmill and overground.

kruskal.test(vo2$perc_vo2_max ~ vo2$group)
pairwise.wilcox.test(vo2$perc_vo2_max, vo2$group, p.adjust.method = "bonf")
kruskal.test(vo2$walking_pc_100 ~ vo2$group)
pairwise.wilcox.test(vo2$walking_pc_100, vo2$group, p.adjust.method = "bonf")
kruskal.test(vo2$walking_pc_120 ~ vo2$group)
pairwise.wilcox.test(vo2$walking_pc_120, vo2$group, p.adjust.method = "bonf")
kruskal.test(vo2$walking_pc_150 ~ vo2$group)
pairwise.wilcox.test(vo2$walking_pc_150, vo2$group, p.adjust.method = "bonf")

# Testing Cost of walking overground and on treadmill

kruskal.test(vo2$energy_CoW ~ vo2$group)
pairwise.wilcox.test(vo2$energy_CoW, vo2$group, p.adjust.method = "bonf")
kruskal.test(vo2$cow_100 ~ vo2$group)
pairwise.wilcox.test(vo2$cow_100, vo2$group, p.adjust.method = "bonf")
kruskal.test(vo2$cow_120 ~ vo2$group)
pairwise.wilcox.test(vo2$cow_120, vo2$group, p.adjust.method = "bonf")
kruskal.test(vo2$cow_150 ~ vo2$group)
pairwise.wilcox.test(vo2$cow_150, vo2$group, p.adjust.method = "bonf")

# Testing treadmill walking speed. As the higher speeds are a % of the initial, only the initial speed will be tested.

kruskal.test(overall$treadmill ~ overall$group)
pairwise.wilcox.test(overall$treadmill, overall$group, p.adjust.method = "bonf")
treadmill_mean <- subset(overall, treadmill >0) %>%
      group_by(group) %>%
      summarise_at(vars(treadmill), list(name = mean, sd = sd))
