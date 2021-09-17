# create figure for commentary

library(cowplot)
library(tidyverse)

set.seed(2113)

# plotting function
plotFun <- function(n = 10, ylab, xlab = NULL) {
  tibble(
    id = 1:n, 
    effect = runif(n, -0.1, 1),
    se = runif(n, 0.2, 0.35),
    lower = effect + 1.96*se,
    upper = effect - 1.96*se
  ) %>%
    ggplot(aes(x = effect, xmin = lower, xmax = upper, y = id)) +
    geom_pointrange(size = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(axis.ticks.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank()) +
    ylab(ylab) +
    xlab(xlab) +
    xlim(-1.2, 1.8)
}

# subplots
pA <- plotFun(ylab = "Analytic\nstrategies")
pB <- plotFun(ylab = "Nation\nstates")
pC <- plotFun(ylab = "Religious\ndenominations")
pD <- plotFun(ylab = "Religiosity\nself-report items", xlab = " ")
pE <- plotFun(ylab = "Wellbeing\nself-report items", xlab = "Effect size")
pF <- plotFun(ylab = "Out-of-sample\ndatasets", xlab = " ")

label <- 
  ggplot() + 
  annotate("text", x = 0, y = 0, size = 8, label = "MARP") +
  scale_x_continuous(breaks = c(-0.05, 0.05)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(3, "mm"))
top <- plot_grid(pA, pB, pC, nrow = 1)
bot <- plot_grid(pD, pE, pF, nrow = 1)
out <- plot_grid(label, top, bot, nrow = 3, rel_heights = c(0.5, 1, 1.05))
ggsave(out, filename = "figure.pdf", width = 6, height = 4)
