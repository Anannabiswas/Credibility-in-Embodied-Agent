# Install packages if you don't have them

#install.packages(c("psych", "nFactors"))

# ------------------------------
# Load packages
# ------------------------------
library(psych)
library(nFactors)

# ------------------------------
# 1. Load data
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
data$Media <- factor(data$Media)   # Spot vs Speaker

# ------------------------------
# 2. Convert Likert items to numeric
# ------------------------------
likert_cols <- c("Q5","Q6","Q7","Q8","Q9","Q10","Q11",
                 "Q12","Q13","Q14","Q15","Q16","Q17")

for(col in likert_cols){
  data[[col]] <- as.numeric(as.character(data[[col]]))
}

# ------------------------------
# 3. Reverse-code items
# ------------------------------
data$Q10R <- 6 - data$Q10   # if original scale 1–5
data$Q12R <- 6 - data$Q12
data$Q13R <- 6 - data$Q13
data$Q14R <- 10 - data$Q14  # original scale 1–9

# ------------------------------
# 4. Create combined item matrix for Factor Analysis
# ------------------------------
items <- data[, c(
  "Q5","Q6","Q7","Q8","Q9",
  "Q10R","Q11","Q12R","Q13R","Q14R",
  "Q15","Q16","Q17"
)]

#apply(items, 2, sd, na.rm = TRUE)
items <- items[, !(names(items) == "Q11")]
apply(items, 2, sd, na.rm = TRUE)
# ------------------------------
# 5. Correlation matrix
# ------------------------------

cor_mat <- cor(items, use = "pairwise.complete.obs")
which(is.na(cor_mat), arr.ind = TRUE)

which(abs(cor_mat) > 0.95 & abs(cor_mat) != 1, arr.ind = TRUE)
eigen(cor_mat)$values




# ------------------------------
# 6. Scree Plot
# ------------------------------
ev <- eigen(cor_mat)
plot(ev$values, type = "b",
     main = "Scree Plot",
     xlab = "Factor Number",
     ylab = "Eigenvalue")

fa.parallel(items, fa = "fa", fm = "pa")
fa(items, nfactors = 3, fm = "pa", rotate = "oblimin")

# ------------------------------
# 8. Kaiser Criterion (Eigenvalue > 1)
# ------------------------------
ev$values
sum(ev$values > 1)



library(psych)

# Run factor analysis
fa_result <- fa(items, nfactors = 3, fm = "pa", rotate = "oblimin")

# Extract pattern matrix
loadings_matrix <- fa_result$loadings
loadings_df <- as.data.frame(loadings_matrix[1:nrow(loadings_matrix), 1:ncol(loadings_matrix)])
loadings_df$Item <- rownames(loadings_df)

# Threshold for strong loadings
threshold <- 0.7

# Initialize empty data frame to store results
summary_table <- data.frame(Factor = character(),
                            Item = character(),
                            Loading = numeric(),
                            stringsAsFactors = FALSE)

# Loop over each factor
for (factor in colnames(loadings_df)[1:(ncol(loadings_df)-1)]) {
  # Select items with loading above threshold
  temp <- loadings_df[abs(loadings_df[[factor]]) >= threshold, c("Item", factor)]
  if(nrow(temp) > 0){
    # Add factor name and rename loading column
    temp$Factor <- factor
    colnames(temp)[2] <- "Loading"
    # Reorder columns
    temp <- temp[, c("Factor", "Item", "Loading")]
    # Add to summary table
    summary_table <- rbind(summary_table, temp)
  }
}

# Reset row names
rownames(summary_table) <- NULL

# Print summary table
print(summary_table)



library(ggplot2)

# Assuming 'summary_table' is the data frame from previous code:
# Columns: Factor, Item, Loading

# Optional: reorder items by factor for better plotting
summary_table$Item <- factor(summary_table$Item, levels = unique(summary_table$Item))

# Plot
ggplot(summary_table, aes(x = Loading, y = Item, fill = Factor)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Factor Loadings of Items",
       x = "Loading",
       y = "Survey Item") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.y = element_text(size = 10))





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
# 3. Compute constructs
# ------------------------------
Credibility <- data[, c("Q5", "Q6")] 
Affability    <- data[, c("Q7", "Q16", "Q17", "Q10R")]     
Social_Competence  <- data[, c("Q8", "Q15")]

# ------------------------------
# 4. Compute Cronbach's alpha manually
# ------------------------------
cat("Cronbach's alpha for Credibility:", cronbach_alpha(Credibility), "\n")
cat("Cronbach's alpha for Affability:", cronbach_alpha(Affability), "\n")
cat("Cronbach's alpha for Social_Competence:", cronbach_alpha(Social_Competence), "\n")


# ------------------------------
# 5. ANOVA example
# ------------------------------
data$Credibility <- rowMeans(Credibility, na.rm = TRUE)
data$Affability <- rowMeans(Affability, na.rm = TRUE)
data$Social_Competence<- rowMeans(Social_Competence, na.rm = TRUE)


constructs <- c("Credibility","Affability","Social_Competence")

for(c in constructs){
  cat("\n==========", c, "==========\n")
  print(summary(aov(data[[c]] ~ data$Media)))
}

library(ggplot2)
library(reshape2)

# Select the constructs for plotting
plot_data <- data[, c("Media", "Credibility", "Affability", "Social_Competence")]

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



library(ggplot2)
library(dplyr)

# Example: summary_table from previous step
# Columns: Factor, Item, Loading
colnames(summary_table)
# Add a Construct column manually based on your EFA mapping
# Example mapping based on your previous factors
summary_table <- summary_table %>%
  mutate(Construct = case_when(
    Factor == "PA1" ~ "Affability",
    Factor == "PA2" ~ "Credibility",
    Factor == "PA3" ~ "Social_Competence"
  ))


ggplot(summary_table, aes(x = Loading, y = reorder(Item, Loading), color = Construct)) +
  geom_point(size = 4) +
  geom_text(aes(label = round(Loading, 2)), hjust = -0.3, size = 3) +
  facet_wrap(~Construct, scales = "free_y", ncol = 1) +
  labs(title = "Factor Loadings per Construct",
       x = "Loading",
       y = "Item") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))



# 
# 
# 
# # ------------------------------
# # 6. Compute Pearson correlations between constructs
# # ------------------------------
# colnames(data)
# construct_scores <- data[, c("Credibility","Affebility","Social_Competence")]
# 
# # Pearson correlation matrix
# cor_matrix <- cor(construct_scores, use = "pairwise.complete.obs", method = "pearson")
# print(cor_matrix)
# 
# 
# # library(corrplot)
# # 
# corrplot(cor_matrix, method = "color", type = "upper", 
#          addCoef.col = "orange", tl.cex = 1, number.cex = 0.9)