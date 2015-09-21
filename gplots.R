library(ggplot2)
library(grid)
# Global theme #1
gtheme <- theme(
  plot.title = element_text(size = 20, vjust = 1.5),
  axis.title.x = element_blank(),
  axis.text.x = element_text(color = "black"),
  axis.title.y = element_text(vjust = 1.5, size = 16),
  axis.text.y = element_text(color = "black"),
  legend.position = "none",
  plot.margin = unit(c(1.5, 1, 1.5, 1), "lines")
)
# Global theme #2
gtheme2 <- theme(
  plot.title = element_text(size = 20, vjust = 1.5),
  axis.title.x = element_blank(),
  axis.text.x = element_text(color = "black"),
  axis.title.y = element_text(vjust = 1.5, size = 16),
  axis.text.y = element_text(color = "black"),
  legend.position = "bottom"
)
# Global theme #3
gtheme3 <- theme(
  plot.title = element_text(size = 20, vjust = 1.5),
  axis.title.x = element_text(vjust = 0, size = 16),
  axis.text.x = element_text(color = "black"),
  axis.title.y = element_text(vjust = 1.5, size = 16),
  axis.text.y = element_text(color = "black"),
  legend.position = c(.9, .9),
  legend.title = element_blank(),
  plot.margin = unit(c(1.5, 1, 1.5, 1), "lines")
)
# ggplot histogram
ghist <- function(var, bin = 5,  y, x){
  ggplot(data, aes(var, fill = factor(0))) +
    geom_histogram(color = "black", binwidth = bin) +
    ylab("No. Patients") +
    annotate("text", y = y, x = x, size = 4,
      label = paste(
        "Mean (SD): ", round(mean(var), 1), " (", round(sd(var), 2), "); ",
        "Median (IQR): ", round(median(var), 1), " (", round(IQR(var), 2), "); ",
        "Min, Max: ", round(min(var), 1), ", ", round(max(var), 2), sep = "")) +
    expand_limits(y = c(0, y)) +
    gtheme
}
# ggplot barplot
gbar <- function(var, origin = data){
  ggplot(data = origin, aes(var, fill = var)) +
    geom_bar(color = "black") +
    ylab("No. Patients") +
    geom_text(stat = "bin", size = 4,
      aes(label = paste(..count.., " (", round((..count..)/sum(..count..)*100), "%)", sep = "")), vjust = -1) +
    expand_limits(y = c(0, 1.1*max(table(var)))) +
    gtheme
}
# ggplot boxplot
gbox <- function(var, group, origin = data, pair = FALSE){
  w.p <- format(wilcox.test(var ~ group, paired = pair)$p.value, digits = 2)
  ymax <- max(tapply(var, group, FUN = max))
  var_q <- tapply(var, group, quantile)
  var_q <- round(var_q, 1)
  ggplot(data = origin, aes(x = group, y = var, fill = group)) +
    geom_boxplot() +
    annotate("text", label = paste("P =", w.p), x = 1.5, y = ymax) +
    # Medians
    annotate("text", label = var_q[[1]][3], x = .55, y = var_q[[1]][3], size = 4) +
    annotate("text", label = var_q[[2]][3], x = 2.45, y = var_q[[2]][3], size = 4) +
    # Q1s
    annotate("text", label = var_q[[1]][2], x = 1.45, y = var_q[[1]][2], size = 4) +
    annotate("text", label = var_q[[2]][2], x = 1.55, y = var_q[[2]][2], size = 4) +
    # Q3s
    annotate("text", label = var_q[[1]][4], x = 1.45, y = var_q[[1]][4], size = 4) +
    annotate("text", label = var_q[[2]][4], x = 1.55, y = var_q[[2]][4], size = 4) +
    # Theme
    gtheme
}
# ggplot2 barplots for 2 comparison of categorical variables
gbar2 <- function(var1, var2){
  t <- table(var1, var2)
  tp <- round(as.numeric(prop.table(t, 1))*100)
  P <- fisher.test(t)$p.value
  P <- format(P, digits = 2)
  g <- ggplot(data.frame(t), aes(x = var1, y = Freq, fill = var2)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black") +
    ylab("No. patients") +
    geom_text(aes(x = var1, y = Freq, label = Freq), vjust = -1,
      position = position_dodge(width = 1)) +
    scale_fill_discrete(name = "5hmC level") +
    expand_limits(y = c(0, 1.2*max(t))) +
    annotate("text", label = paste("P =", P), x = nlevels(var1), y = 1.2*max(t)) +
    gtheme2
}
# ggplot2 survival curves using ggfortify
library("ggfortify")
library("survival")
gsurv <- function(FU, outcome, status){
  ht <- survdiff(Surv(FU, as.numeric(outcome)) ~ status)
  p <- format(pchisq(ht$chisq, df = 1, lower.tail = FALSE), digits = 2)
  fit <- survfit(Surv(FU, as.numeric(outcome)) ~ status)
  autoplot(fit, conf.int = FALSE, censor = FALSE, surv.size = 1) +
    ylab("Survival") +
    xlab("Follow-up") +
    annotate("text", label = paste("P =", p), x = 15, y = .05) +
    scale_colour_discrete(labels = levels(status)) +
    gtheme3
}
