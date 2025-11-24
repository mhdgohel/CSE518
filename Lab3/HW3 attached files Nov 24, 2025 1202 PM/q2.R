data2 <- read.csv("dataset2.csv")

data2$menu <- as.factor(data2$menu)
data2$user <- as.factor(data2$user)

anova_rm_model <- aov(time ~ menu + Error(user/menu), data = data2)

print("--- Repeated Measures ANOVA Results ---")
print(summary(anova_rm_model))

print("--- Pairwise t-test Results (Paired, No Adjustment) ---")
pairwise_rm_results <- pairwise.t.test(data2$time, data2$menu, 
                                       p.adjust.method = "none", 
                                       paired = TRUE)
print(pairwise_rm_results)

jpeg("dataset2_boxplot.jpg")
boxplot(time ~ menu, data = data2,
        main = "Dataset 2: Time by Menu Type",
        xlab = "Menu Type",
        ylab = "Task Completion Time",
        col = c("lightblue", "lightgreen", "pink", "wheat"),
        las = 1)

dev.off()

library(ggplot2)
p2<-ggplot(data2, aes(x=menu, y=time, fill=menu)) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) +
  geom_jitter(width=0.2, alpha=0.5) +
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="white") +
  labs(title="Scenario 2: Menu Performance (Within-Subjects)",
       y="Completion Time (s)", x="Menu Type") +
  theme_minimal()

ggsave("dataset2_ggplot.png", plot = p2, width = 6, height = 4)

print(p2)