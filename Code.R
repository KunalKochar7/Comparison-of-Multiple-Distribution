# Comparison of Multiple Distributions (Report-2)


library(ggplot2)
library(gridExtra)

R.version.string  #Version of R

df <- read.csv("./ImmoDataRuhr.csv", header = T) #read data

#Descriptive Statistics
table(df$regio2)
summary(df$sqmPrice)
str(df)

#Data summary
df_s <- data.frame()
for (i in unique(df$regio2)){
  
  df_s <- rbind(df_s,summary(df$sqmPrice[df$regio2 == i]))
  
}
colnames(df_s) <- c("Min","Q1","Median","Mean","Q3","Max")
df_s

#Boxplot for homogeneity of variance assessment
ggplot(data = df, mapping = aes(x = regio2, y = sqmPrice)) +
  stat_boxplot(geom = "errorbar", size = 1) +
  geom_boxplot(fill="darkcyan") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))

#QQPlot for normality assessment
for (i in df$regio2){
  
  new_df <- df[df$regio2 == i,]
  
  p <- ggplot(data = new_df, mapping = aes(sample = sqmPrice)) + 
    geom_qq(colour = "darkcyan") + 
    geom_qq_line(size = 1) +
    labs(x = "", y = i)
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))
  
  assign(paste0("qq_",i,"_plot"),p)
}
grid.arrange(qq_Dortmund_plot, qq_Bochum_plot, qq_Essen_plot, qq_Duisburg_plot, nrow = 2, ncol = 3)

#Anova Test
anova.res <- aov(formula = sqmPrice ~ regio2, data = df)
summary(anova.res)

#Without Adjustment pairwise t-test
pairwise.t.test(x = df$sqmPrice, g = df$regio2, p.adjust.method = "none", pool.sd = TRUE)

#Bonferroni Adjustment pairwise t-test
pairwise.t.test(x = df$sqmPrice, g = df$regio2, p.adjust.method = "bonferroni", pool.sd = TRUE)
