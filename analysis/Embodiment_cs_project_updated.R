# ------------------------------
# 0. Clean session
# ------------------------------
# Restart R/RStudio before running this to avoid package conflicts
rm(list = ls())
gc()

# ------------------------------
# 1. Install and load psych only first
# ------------------------------
if(!require(psych)) install.packages("psych")
library(psych)

# ------------------------------
# 2. Load data
# ------------------------------
data <- read.csv("Speaker_Spot.csv")

# Keep first 18 columns
data <- data[, 1:18]

# Rename columns for clarity
names(data) <- c(
  "Media", "ParticipantID", "Age", "Gender", "Familiarity",
  "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11",
  "Q12", "Q13", "Q14", "Q15", "Q16", "Q17"
)

# Convert Media to factor
data$Media <- factor(data$Media)  # Spot vs Speaker

# ------------------------------
# 1. Manual Cronbach's alpha function
# ------------------------------
cronbach_alpha <- function(df) {
  k <- ncol(df)
  item_var <- apply(df, 2, var, na.rm = TRUE)
  total_var <- var(rowSums(df, na.rm = TRUE), na.rm = TRUE)
  alpha <- (k / (k - 1)) * (1 - sum(item_var)/total_var)
  return(alpha)
}

# ------------------------------
# 2. Ensure numeric
# ------------------------------
likert_cols <- c("Q5","Q6","Q7","Q8","Q9","Q10","Q12","Q13","Q14","Q15","Q16","Q17")
for(col in likert_cols){
  data[[col]] <- as.numeric(as.character(data[[col]]))
}

# Reverse-code items
data$Q10R <- 6 - data$Q10
data$Q12R <- 6 - data$Q12
data$Q13R <- 6 - data$Q13
data$Q14R <- 10 - data$Q14

# ------------------------------
# 3. Compute constructs
# ------------------------------
Credibility <- data[, c("Q5", "Q6", "Q15", "Q14R")] 
Accuracy    <- data[, c("Q6", "Q14R")]     
Embodiment  <- data[, c("Q8", "Q9", "Q12R", "Q13R", "Q17")]
Comfort     <- data[, c("Q7", "Q12R", "Q17")]
Enjoyment   <- data[, c("Q10R", "Q16")]

# ------------------------------
# 4. Compute Cronbach's alpha manually
# ------------------------------
cat("Cronbach's alpha for Credibility:", cronbach_alpha(Credibility), "\n")
cat("Cronbach's alpha for Embodiment:", cronbach_alpha(Embodiment), "\n")
cat("Cronbach's alpha for Comfort:", cronbach_alpha(Comfort), "\n")
cat("Cronbach's alpha for Enjoyment:", cronbach_alpha(Enjoyment), "\n")
cat("Cronbach's alpha for Accuracy:", cronbach_alpha(Accuracy), "\n")

# ------------------------------
# 5. ANOVA example
# ------------------------------
data$Credibility <- rowMeans(Credibility, na.rm = TRUE)
data$Accuracy <- rowMeans(Accuracy, na.rm = TRUE)
data$Embodiment <- rowMeans(Embodiment, na.rm = TRUE)
data$Comfort <- rowMeans(Comfort, na.rm = TRUE)
data$Enjoyment <- rowMeans(Enjoyment, na.rm = TRUE)


constructs <- c("Credibility","Accuracy","Embodiment","Comfort","Enjoyment")

for(c in constructs){
  cat("\n==========", c, "==========\n")
  print(summary(aov(data[[c]] ~ data$Media)))
}

library(ggplot2)
library(reshape2)

# Select the constructs for plotting
plot_data <- data[, c("Media", "Credibility", "Accuracy", "Embodiment", "Comfort", "Enjoyment")]

# Convert data to long format for ggplot
plot_long <- melt(plot_data, id.vars = "Media",
                  variable.name = "Construct",
                  value.name = "Score")


# Boxplots with jittered points
ggplot(plot_long, aes(x = Media, y = Score, fill = Media)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  facet_wrap(~Construct, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(title = "Construct Scores by Media Type",
       x = "Media",
       y = "Score") +
  theme(legend.position = "none")


# ------------------------------
# 6. Compute Pearson correlations between constructs
# ------------------------------
construct_scores <- data[, c("Credibility","Accuracy","Embodiment","Comfort","Enjoyment")]

# Pearson correlation matrix
cor_matrix <- cor(construct_scores, use = "pairwise.complete.obs", method = "pearson")
print(cor_matrix)


library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "orange", tl.cex = 1, number.cex = 0.9)


# # ------------------------------
# # 3. Compute constructs
# # ------------------------------
# Credibility <- data[, c("Q5","Q6","Q7")]
# Accuracy    <- data$Q8          # single item
# Embodiment  <- data[, c("Q9","Q10R")]
# Comfort     <- data[, c("Q13R","Q14R")]
# Enjoyment   <- data[, c("Q16","Q17")]
# 
# # ------------------------------
# # 4. Compute Cronbach's alpha manually
# # ------------------------------
# cat("Cronbach's alpha for Credibility:", cronbach_alpha(Credibility), "\n")
# cat("Cronbach's alpha for Embodiment:", cronbach_alpha(Embodiment), "\n")
# cat("Cronbach's alpha for Comfort:", cronbach_alpha(Comfort), "\n")
# cat("Cronbach's alpha for Enjoyment:", cronbach_alpha(Enjoyment), "\n")
# cat("Accuracy is a single item, alpha not applicable.\n")
# 
# # ------------------------------
# # 5. ANOVA example
# # ------------------------------
# data$Credibility <- rowMeans(Credibility, na.rm = TRUE)
# data$Accuracy <- data$Q8
# data$Embodiment <- rowMeans(Embodiment, na.rm = TRUE)
# data$Comfort <- rowMeans(Comfort, na.rm = TRUE)
# data$Enjoyment <- rowMeans(Enjoyment, na.rm = TRUE)
# 
# 
# constructs <- c("Credibility","Accuracy","Embodiment","Comfort","Enjoyment")
# 
# for(c in constructs){
#   cat("\n==========", c, "==========\n")
#   print(summary(aov(data[[c]] ~ data$Media)))
# }
# 
# library(ggplot2)
# library(reshape2)
# 
# # Select the constructs for plotting
# plot_data <- data[, c("Media", "Credibility", "Accuracy", "Embodiment", "Comfort", "Enjoyment")]
# 
# # Convert data to long format for ggplot
# plot_long <- melt(plot_data, id.vars = "Media",
#                   variable.name = "Construct",
#                   value.name = "Score")
# 
# 
# # Boxplots with jittered points
# ggplot(plot_long, aes(x = Media, y = Score, fill = Media)) +
#   geom_boxplot(alpha = 0.5, outlier.shape = NA) +
#   geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
#   facet_wrap(~Construct, scales = "free_y") +
#   theme_minimal(base_size = 14) +
#   labs(title = "Construct Scores by Media Type",
#        x = "Media",
#        y = "Score") +
#   theme(legend.position = "none")
