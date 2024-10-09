setwd("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Data")

library(readxl)
library(dplyr)
library(tidyverse)
library(purrrlyr)
library(ggpubr)
library(ggplot2)
library(ggcorrplot)

my_comparisons <- list( c("CP_O", "CP_Y"), c("CP_O", "TD_O"), c("CP_O", "TD_Y") )

contrex <- read_excel("Anthropometrics.xlsx", 
                              sheet = "contrex")
vo2 <- read_excel("Anthropometrics.xlsx", 
                      sheet = "vo2")
general <- read_excel("Anthropometrics.xlsx", 
                  sheet = 1)

icc <- read_excel("icc.xlsx")
icc <- select(icc, -id)
ICC(icc)


#Finding maximum values for each contraction

contrex_max <- contrex
contrex_max <- contrex_max %>%
      rowwise() %>% 
      mutate(q_iso_max = max(q_iso1, q_iso2, q_iso3,na.rm = TRUE),
             q_60_max = max(q_60_1,q_60_2,na.rm = TRUE),
             q_120_max = max(q_120_1,q_120_1,na.rm = TRUE),
             h_iso_max = max(h_iso1,h_iso2,h_iso3,na.rm = TRUE),
             h_60_max = max(h_60_1,h_60_2,na.rm = TRUE),
             h_120_max = max(h_120_1,h_120_1,na.rm = TRUE),
             a_iso_max = max(a_iso1, a_iso2, a_iso3,na.rm = TRUE),
             a_60_max = max(a_60_1,a_60_2,na.rm = TRUE),
             a_120_max = max(a_120_1,a_120_1,na.rm = TRUE)) 
contrex_max
# Using the next bit to transform -infinite' to NA instead. They're easier to handle
contrex_max[sapply(contrex_max, is.infinite)] <- NA
#Normalizing maximal values to weight

contrex_max_norm <- contrex_max %>%
      rowwise() %>%
      mutate(q_iso_max_norm = q_iso_max / weight, na.rm = TRUE,
             q_60_max_norm = q_60_max / weight, na.rm = TRUE,
             q_120_max_norm = q_120_max / weight, na.rm = TRUE,
             h_iso_max_norm = h_iso_max / weight, na.rm = TRUE,
             h_60_max_norm = h_60_max / weight, na.rm = TRUE,
             h_120_max_norm = h_120_max / weight, na.rm = TRUE,
             a_iso_max_norm = a_iso_max / weight, na.rm = TRUE,
             a_60_max_norm = a_60_max / weight, na.rm = TRUE,
             a_120_max_norm = a_120_max / weight,na.rm = TRUE) %>%
      mutate(iso_combined = (q_iso_max_norm + h_iso_max_norm + a_iso_max_norm)/3,na.rm = TRUE,
             c_60_combined = (q_60_max_norm + h_60_max_norm + a_60_max_norm)/3,na.rm = TRUE,
             c_120_combined = (q_120_max_norm + h_120_max_norm + a_120_max_norm)/3,na.rm = TRUE,
             a_combined = (a_iso_max_norm + a_60_max_norm + a_120_max_norm)/3,na.rm = TRUE,
             h_combined = (h_iso_max_norm + h_60_max_norm + h_120_max_norm)/3,na.rm = TRUE,
             q_combined = (q_iso_max_norm + q_60_max_norm + q_120_max_norm)/3,na.rm = TRUE) %>%
      mutate(quad_ham_ratio = (h_iso_max_norm/q_iso_max_norm)*100,
             quad_ham_all = (h_combined/q_combined)*100)



# Combined analysis -------------------------------------------------------

# Combined ANOVA / Kruskal-Wallis

kruskal.test(iso_combined ~ group, data = contrex_max_norm)
pairwise.wilcox.test(contrex_max_norm$iso_combined, contrex_max_norm$group, p.adjust.method = "BH")

kruskal.test(c_60_combined ~ group, data = contrex_max_norm)
pairwise.wilcox.test(contrex_max_norm$c_60_combined, contrex_max_norm$group, p.adjust.method = "BH")

kruskal.test(c_120_combined ~ group, data = contrex_max_norm)
pairwise.wilcox.test(contrex_max_norm$c_120_combined, contrex_max_norm$group, p.adjust.method = "BH")

kruskal.test(q_combined ~ group, data = contrex_max_norm)
pairwise.wilcox.test(contrex_max_norm$q_combined, contrex_max_norm$group, p.adjust.method = "BH")

kruskal.test(h_combined ~ group, data = contrex_max_norm)
pairwise.wilcox.test(contrex_max_norm$h_combined, contrex_max_norm$group, p.adjust.method = "BH")

kruskal.test(a_combined ~ group, data = contrex_max_norm)
pairwise.wilcox.test(contrex_max_norm$a_combined, contrex_max_norm$group, p.adjust.method = "BH")

# Combined figures

iso <- ggplot(data = subset(contrex_max_norm, group !="0"), 
              aes(x = group, y = iso_combined, col = group)) +
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
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = "Maximal force normalized to weight",
           subtitle = "Average isometric force")
iso
ggsave(dpi = 600, scale = 1.5,
       "iso.tiff")

c_60 <- ggplot(data = subset(contrex_max_norm, group !="0"), 
               aes(x = group, y = c_60_combined, col = group)) +
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
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = "Maximal force normalized to weight",
           subtitle = "Average concentric force at 60°/s")
c_60
ggsave(dpi = 600, scale = 1.5,
       "combined at 60.tiff")

c_120 <- ggplot(data = subset(contrex_max_norm, group !="0"), 
            aes(x = group, y = c_120_combined, col = group)) +
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
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = "Maximal force normalized to weight",
           subtitle = "Average concentric force at 120°")
c_120
ggsave(dpi = 600, scale = 1.5,
       "combined at 120.tiff")

quad <- ggplot(data = subset(contrex_max_norm, group !="0"), 
               aes(x = group, y = q_combined, col = group)) +
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
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = "Maximal force normalized to weight",
           subtitle = "Average force in quadriceps")
quad

ham <- ggplot(data = subset(contrex_max_norm, group !="0"), 
              aes(x = group, y = c_120_combined, col = group)) +
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
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = "Maximal force normalized to weight",
           subtitle = "Average force in hamstrings")
ham

ankle <- ggplot(data = subset(contrex_max_norm, group !="0"), 
                aes(x = group, y = c_120_combined, col = group)) +
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
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "Group", y = "Maximal force normalized to weight",
           subtitle = "Average force in ankle")
ankle

# Quadriceps --------------------------------------------------------------

# Quadriceps group means

contrex_max_norm_means <- subset(contrex_max_norm, q_iso_max_norm >0) %>%
      group_by(group) %>%
      summarise_at(vars(q_iso_max_norm, q_60_max_norm, q_120_max_norm,
                        h_iso_max_norm, h_60_max_norm, h_120_max_norm,
                        a_iso_max_norm, a_60_max_norm, a_120_max_norm,
                        quad_ham_ratio, quad_ham_all), 
                   list(mean = mean, sd = sd), na.rm=TRUE)

kruskal.test(contrex_max_norm$quad_ham_ratio ~ contrex_max_norm$group)
pairwise.wilcox.test(contrex_max_norm$quad_ham_ratio, contrex_max_norm$group,
                        p.adjust.method = "BH")
kruskal.test(contrex_max_norm$quad_ham_all ~ contrex_max_norm$group)
pairwise.wilcox.test(contrex_max_norm$quad_ham_all, contrex_max_norm$group,
                     p.adjust.method = "BH")


# Quadriceps anova



# Quadriceps figures

quad_iso <- ggplot(data = subset(contrex_max_norm, group !="0"), 
            aes(x = group, y = q_iso_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) + 
      scale_y_continuous(limits = c(1,6)) +
      scale_x_discrete(labels = c("CP_O" = "",
                                  "TD_O" = "",
                                  "CP_Y" = "",
                                  "TD_Y" = "")) +
      labs(x = "", 
            y = "Quadriceps",
           subtitle = "Isometric")
quad_iso
ggsave(dpi = 600, scale = 1.5,
       "quad iso.tiff")
# Here a way to change the y or x for a figure setup - Won't be useful as 
# we need to change the text every time anyway.
#quad_iso %+% aes(y = q_60_max_norm)

quad_60 <- ggplot(data = subset(contrex_max_norm, group !="0"),
                  aes(x = group, y = q_60_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(0.4,4)) +
      scale_x_discrete(labels = c("CP_O" = "",
                                  "TD_O" = "",
                                  "CP_Y" = "",
                                  "TD_Y" = ""))  +
      labs(x = "", y = "",
           subtitle = ('60° / s'))
quad_60
ggsave(dpi = 600, scale = 1.5,
       "quad 60.tiff")

quad_120 <- ggplot(data = subset(contrex_max_norm, group !="0"),
                   aes(x = group, y = q_120_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.99) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(0.4,3.5)) +
      scale_x_discrete(labels = c("CP_O" = "",
                                  "TD_O" = "",
                                  "CP_Y" = "",
                                  "TD_Y" = ""))  +
      labs(x = "", y = "",
           subtitle = ('120° / s'))
quad_120
ggsave(dpi = 600, scale = 1.5,
       "quad 120.tiff")

# Hamstring ---------------------------------------------------------------

# Hamstring anova

# Hamstring figures

ham_iso <- ggplot(data = subset(contrex_max_norm, group !="0"),
                  aes(x = group, y = h_iso_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(0,2.6)) +
      scale_x_discrete(labels = c("CP_O" = "",
                                  "TD_O" = "",
                                  "CP_Y" = "",
                                  "TD_Y" = ""))  +
      labs(x = "", 
            y = "Hamstring",
           #subtitle = "Hamstring"
           )
ham_iso
ggsave(dpi = 600, scale = 1.5,
       "ham iso.tiff")
      

ham_60 <- ggplot(data = subset(contrex_max_norm, group !="0"),
                 aes(x = group, y = h_60_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(0,2.1)) +
      scale_x_discrete(labels = c("CP_O" = "",
                                  "TD_O" = "",
                                  "CP_Y" = "",
                                  "TD_Y" = ""))  +
      labs(x = "", y = "",
      #     subtitle = ('Dynamic force (60°/ s) from the hamstring musculature')
      )
ham_60
ggsave(dpi = 600, scale = 1.5,
       "ham 60.tiff")

ham_120 <- ggplot(data = subset(contrex_max_norm, group !="0"),
                  aes(x = group, y = h_120_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(0,2.2)) +
      scale_x_discrete(labels = c("CP_O" = "",
                                  "TD_O" = "",
                                  "CP_Y" = "",
                                  "TD_Y" = ""))  +
      labs(x = "", y = "",
      #     subtitle = ('Dynamic force (120°) from the hamstring musculature')
           )
ham_120
ggsave(dpi = 600, scale = 1.5,
       "ham 120.tiff")

# Ankle -------------------------------------------------------------------

# Ankle anova

# Ankle figures

ankle_iso <- ggplot(data = subset(contrex_max_norm, group !="0"),
                    aes(x = group, y = a_iso_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +
      scale_y_continuous(limits = c(0,3)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "", 
            y = "Ankle",
           #subtitle = "Ankle"
           )
ankle_iso
ggsave(dpi = 600, scale = 1.5,
       "ankle iso.tiff")


ankle_60 <- ggplot(data = subset(contrex_max_norm, group !="0"),
                   aes(x = group, y = a_60_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(0,2)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "", y = "",
           #subtitle = ('Dynamic force (60°/ s) from the ankle musculature')
           )
ankle_60
ggsave(dpi = 600, scale = 1.5,
       "ankle 60.tiff")

ankle_120 <- ggplot(data = subset(contrex_max_norm, group !="0"),
                    aes(x = group, y = a_120_max_norm, col = group)) +
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
      stat_compare_means(method = "kruskal.test", label = "p.format",
                         label.y.npc = 0.98) +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) +  
      scale_y_continuous(limits = c(0,1.6)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "", y = "",
          # subtitle = ('Dynamic force (120°) from the ankle musculature')
          )
ankle_120
ggsave(dpi = 600, scale = 1.5,
       "ankle 120.tiff")

# Testing different figure configurations:

# 9 plot setup
ggarrange(quad_iso, quad_60, quad_120,
          ham_iso, ham_60, ham_120,
          ankle_iso, ankle_60, ankle_120
          )
ggsave(dpi = 600,scale = 1.5, "9-plot.tiff")

# 3 plot setup - Isometric, 60 and 120
blank <- ggplot()
ggarrange(
      ggarrange(blank, iso, blank, nrow = 1, widths = c(0.5,1,0.5), legend = "none"),
      ggarrange(c_60, c_120, blank, nrow = 1, widths = c(1,1,0)), 
      nrow = 2)
ggsave(dpi = 600, "3-plot_speed.jpeg")

# 3 plot setup - Quad, Ham and Ankle

ggarrange(
      ggarrange(blank, quad, blank, nrow = 1, widths = c(0.5,1,0.5), legend = "none"),
      ggarrange(ham, ankle, blank, nrow = 1, widths = c(1,1,0)), 
      nrow = 2)
ggsave(dpi = 600, "3-plot_bodypart.jpeg")

# Multiple linear regressions ---------------------------------------------

# Let's try with Cost of Walking as dependent outcome with different setups for 
# quantifying muscular strength and it's link with CoW. First.
# Trying with only measures pooled into one

# To do that, I create a combined df with CoW and all the necessary measures.

multiple_regressions <- contrex_max_norm %>%
      full_join(vo2, by = "id") 
multiple_regressions[is.na(multiple_regressions) | multiple_regressions=="-Inf"] = NA
multiple_regressions <- multiple_regressions %>%
      full_join(general, by = "id")

multiple_regressions <- multiple_regressions %>%
      mutate(strength = (iso_combined + c_60_combined + c_120_combined) / 3)

# Models with Cost of Walking as dependent variable
      
model_all_factors_cow <- lm(formula = energy_CoW ~ q_iso_max_norm + h_iso_max_norm + a_iso_max_norm +
                    q_60_max_norm + h_60_max_norm + a_60_max_norm + 
                    q_120_max_norm + h_120_max_norm + a_120_max_norm +
                    bmi + sex + group,
              data = multiple_regressions)
summary(model_all_factors_cow)

model_strengh_cow <- lm(formula = energy_CoW ~ strength +
                  bmi + sex + group,
                  data = multiple_regressions)
summary(model_strengh_cow)

model_general_cow <- lm(formula = energy_CoW ~ bmi + sex + group + 
                  vo2_peak_norm + six_mwt + tug + walk_10m,
                  data = multiple_regressions)
summary(model_general_cow)

# Models with six minute walking test as dependent variable

model_all_sixmwt <- lm(formula = six_mwt ~ q_iso_max_norm + h_iso_max_norm + a_iso_max_norm +
                  q_60_max_norm + h_60_max_norm + a_60_max_norm + 
                  q_120_max_norm + h_120_max_norm + a_120_max_norm +
                  bmi + sex + group,
          data = multiple_regressions)
summary(model_all_sixmwt)

model_strength_sixmwt <- lm(formula = six_mwt ~ strength +
                    bmi + sex + group,
              data = multiple_regressions)
summary(model_strength_sixmwt)

model_general_sixmwt <- lm(formula = six_mwt ~ energy_CoW + bmi + sex + group + 
                              vo2_peak_norm + tug + walk_10m,
                        data = multiple_regressions)
summary(model_general_sixmwt)

# models not considering group or gender

model_strength_sixmwt_nogroup <- lm(formula = six_mwt ~ strength +
                                  vo2_peak_norm,
                            data = multiple_regressions)
summary(model_strength_sixmwt_nogroup)

model_general_sixmwt_nogroup <- lm(formula = six_mwt ~ strength + energy_CoW +
                                 vo2_peak_norm + tug + walk_10m,
                           data = multiple_regressions)
summary(model_general_sixmwt_nogroup)

