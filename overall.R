setwd("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Data")

library(readxl)
library(dplyr)
library(tidyverse)
library(purrrlyr)
library(ggpubr)
library(ggplot2)

my_comparisons <- list( c("CP_O", "CP_Y"), c("CP_O", "TD_O"), c("CP_O", "TD_Y") )

overall <- read_excel("Anthropometrics.xlsx", 
                  sheet = "overall")

overall_mean <- subset(overall, height >0) %>%
      group_by(group) %>%
      summarise_at(vars(six_mwt_oxy, six_mwt, tug, walk_10m_speed, berg), list(name = mean, sd = sd))

berg_test <- subset(overall, berg !=0)
kruskal.test(berg ~ group, data = berg_test)
# Large difference between young and old. 

# Figures -----------------------------------------------------------------

#standard <- theme_pubr(legend = "none") +
#      theme(text = element_text(size = 25)) + 
#      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
#      stat_summary(
#            geom = "point", fun.y = "mean",
#            size = 15, shape = '-', position=position_dodge(width=.5))
#statistics <- stat_compare_means(method = "kruskal.test", label = "p.format") +
#      stat_compare_means(method = "wilcox", label = "p.signif",
#                         comparisons = my_comparisons,
#                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni"))

# Anthropometrics

tug <- ggplot(data = subset(overall, group !="0"), 
              aes(x = group,
                  y = tug,
                  col = group)) +
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
      scale_y_continuous(limits = c(4,26)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "", y = ("Time [s]"),
           subtitle = "Timed up and go")
tug
ggsave(dpi = 600, scale = 1.5,
       "Timed up and go.tiff")

walkway <- ggplot(data = subset(overall, group !="0"), 
            aes(x = group, y = walk_10m_speed,
                col = group)) +
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
      scale_y_continuous(limits = c(0.5,3.1)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "", y = ("Walking speed [m/s]"),
           subtitle = "10 meter walking test")
walkway
ggsave(dpi = 600, scale = 1.5,
       "10 meter walking test speed.tiff")

walkingtest <- ggplot(data = subset(overall, group !="0"), 
                  aes(x = group, y = six_mwt,
                      col = group)) +
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
      scale_y_continuous(limits = c(250,900)) + 
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y]))) +
      labs(x = "", y = ("Walking distance [m]"),
           subtitle = "Six minute walking test")
walkingtest
ggsave(dpi = 600, scale = 1.5,
       "6 minute walking test distance.tiff")

walkingtestspeed <- ggplot(data = subset(overall, group !="0"), 
                           aes(x = group, y = six_mwt/360,
                               col = group)) +
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
      labs(x = "", y = ("Average walking speed [m/s]"),
           subtitle = "Six minute walking test")
walkingtestspeed
ggsave(dpi = 600, scale = 1.5,
       "Six minute walking test speed.tiff")

walkingmax_6mwt <- ggplot(data = subset(overall, group !="0"), 
            aes(x = walk_10m_speed, y = six_mwt/360, col = group)) +
      geom_point(aes(shape = as.factor(pairs), size = 5, alpha = 0.6)) + 
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      #geom_smooth(method = "lm") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) + 
      labs(x = "10MWT [m/s]", y = "6MWT [m/s]",
           subtitle = "10 meter walking test and 6-minute walking test performance") +
      scale_x_continuous(breaks = seq(0,3,.5),limits = c(0,3)) +
      scale_y_continuous(breaks = seq(0,3,.5),limits = c(0,3))
walkingmax_6mwt
ggsave(dpi = 600, scale = 1.5,
       "Speed difference 10 meter vs 6 minute.tiff")

library(blandr)
library(BlandAltmanLeh)
blandr.statistics(overall$six_mwt_oxy, overall$six_mwt)
sixmwt_bland_altman <- blandr::blandr.plot.ggplot(
      statistics.results = blandr.statistics(overall$six_mwt_oxy, overall$six_mwt),
      method1name = "With mask",
      method2name = "Without mask"
)
sixmwt_bland_altman

bland.altman.plot(overall$six_mwt_oxy, overall$six_mwt,
            graph.sys="ggplot2",
            xlab = "Walking distance in 6MWT with a mask on",
            ylab = "Difference without a mask on",) +
      theme_pubr(legend = "none") 

ggarrange(tug, walkway, walkingtest, walkingmax_6mwt,
          labels = c("A","B","C", "D"))
ggsave(dpi = 600, scale = 1.5,
       "General walking plots (4-aside).tiff")

# Within person statistics ------------------------------------------------

# Test to see if six minute with a mask gives different results
walktest <- with(overall, t.test(six_mwt_oxy, six_mwt,
                                 paired = TRUE))
walktest

walktest_cp <- with(berg_test, t.test(six_mwt_oxy, six_mwt,
                                 paired = TRUE))
walktest_cp

# Correlation test to see if the overall concept is the same
walktest_cor <- with(overall, cor.test(x = six_mwt_oxy, y = six_mwt))
walktest_cor

sixmwtplot <- ggplot(data = subset(overall, group !="0"), 
                     aes(x = six_mwt, y = six_mwt_oxy, position = id, col=group)) +
      geom_point(aes(shape = as.factor(pairs), size = 5, alpha = 0.6)) + 
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      #geom_smooth(method = "lm") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      theme_pubr(legend = "none") +
      theme(text = element_text(size = 15)) + 
      labs(x = "Walking distance with mask [m]", y = ("Walking distance without mask [m]"),
           subtitle = "Six minute walking test with and without mask") +
      scale_x_continuous(breaks = seq(200,750,50),limits = c(200,750)) +
      scale_y_continuous(breaks = seq(200,750,50),limits = c(200,750))
sixmwtplot
ggsave(dpi = 600, scale = 1.5,
       "with_and_without_mask.tiff")

# We see a difference in distance walked with and without the mask on a within
# person basis, but not on a group level where the correlation is strong. 

# Group statistics --------------------------------------------------------

anthro <- names(overall)[c(7,8,9,10)]
ttests <- lapply(anthro, function(x) t.test(reformulate("cp",x), data = overall))
ttests# no differences in any anthropometric value (age, height, weight, BMI)


# Questionnaires ----------------------------------------------------------

# Importing the questionnaire responses
setwd("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Questionnaires")
questions <- read_excel("Collected output questionnaires.xlsx")

Godin <- ggplot(data = questions, 
                aes(x = group, y = Godin, col=group)) +
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
      labs(x = "", y = "GODIN-score (Higher = more active)",
           subtitle = "")
Godin

question_means <- questions %>%
      group_by(group) %>%
      summarise_at(vars(Godin, FS, `Physical Functioning`, `Role Physical`,
                        `Bodily Pain`, `General Health`, Vitality,
                        `Social Functioning`, `Role Emotional`, `Mental Health`), 
                   list(name = mean, sd = sd), na.rm = TRUE)
question_comparison <- names(questions)[c(3:19)]
question_ttests <- lapply(question_comparison, function(x) kruskal.test(reformulate("group",x), data = questions))
question_ttests

pairwise.wilcox.test(questions$FS, questions$group, p.adjust.method = "bonf")
pairwise.wilcox.test(questions$`Role Physical`, questions$group, p.adjust.method = "bonf")
pairwise.wilcox.test(questions$`Mental Health`, questions$group, p.adjust.method = "bonf")

