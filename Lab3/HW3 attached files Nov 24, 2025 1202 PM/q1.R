data1 <- read.csv("dataset1.csv")

data1$menu <- as.factor(data1$menu)

anova_model <- aov(time ~ menu, data = data1)

print("--- One-way ANOVA Results ---")
print(summary(anova_model))

print("--- Pairwise t-test Results (Independent, No Adjustment) ---")
pairwise_results <- pairwise.t.test(data1$time, data1$menu, 
                                    p.adjust.method = "none", 
                                    pool.sd = FALSE)
print(pairwise_results)

jpeg("dataset1_boxplot.jpg")
boxplot(time ~ menu, data = data1,
        main = "Dataset 1: Time by Menu Type",
        xlab = "Menu Type",
        ylab = "Task Completion Time",
        col = c("lightblue", "lightgreen", "pink", "wheat"),
        las = 1)

dev.off()

library(ggplot2)

p <- ggplot(data1, aes(x=menu, y=time, fill=menu)) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) +
  geom_jitter(width=0.2, alpha=0.5) +
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="white") +
  labs(title="Scenario 1: Menu Performance (Between-Subjects)",
       y="Completion Time (s)", x="Menu Type") +
  theme_minimal()

ggsave("dataset1_ggplot.png", plot = p, width = 6, height = 4)

# OR, if you want to see it in the RStudio plot window, use:
print(p)