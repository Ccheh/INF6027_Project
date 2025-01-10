# Load necessary R packages
library(tidyverse)
library(skimr)
library(readr)
library(dplyr)
library(scales)
library(factoextra)
library(FactoMineR)
library(factoextra)

# 1. Data Loading and Cleaning
# =====================
# Load data
df <- read_csv("dataset.csv", 
               locale = locale(encoding = "iso-8859-1"),
               show_col_types = FALSE)

# Check data structure and basic statistics
str(df)       # Data structure
summary(df)   # Basic statistics
skim(df)      # Detailed descriptive statistics

# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)
print(missing_values[missing_values > 0])  # Print columns with missing values

# Display rows with missing values
if(sum(missing_values) > 0) {
  print("\nDetailed information about rows with missing values:")
  missing_rows <- df[!complete.cases(df), ]  # Use base R syntax
  missing_rows$row_num <- seq_len(nrow(missing_rows))  # Add row numbers
  print(missing_rows)
}

# Data cleaning: Remove rows with missing values in key columns (e.g., artists, album_name, track_name)
df_cleaned <- df %>%
  filter(!is.na(artists) & !is.na(album_name) & !is.na(track_name))
# Check missing values after cleaning
missing_values_cleaned <- colSums(is.na(df_cleaned))
print(missing_values_cleaned)
print(missing_values_cleaned[missing_values_cleaned > 0])
df <- df_cleaned

# Summary statistics for numeric variables
numeric_vars <- c("popularity", "duration_ms", "danceability", 
                  "energy", "acousticness", "valence",
                  "tempo", "loudness", "instrumentalness", 
                  "speechiness", "liveness")
summary(df[numeric_vars])

# Convert duration from milliseconds to minutes
df <- df %>%
  mutate(duration_min = round(duration_ms / (1000 * 60), 2))
duration_summary <- summary(df$duration_min)
print(duration_summary)

# ====== 2. Outlier Detection ======
# Check abnormal durations
duration_check <- df %>%
  summarise(
    extremely_short = sum(duration_ms < 30000),  # Less than 30 seconds
    extremely_long = sum(duration_ms > 600000)   # Greater than 10 minutes
  )
print(duration_check)
# Check popularity distribution
popularity_check <- df %>%
  summarise(
    zero_popularity = sum(popularity == 0),
    very_low_popularity = sum(popularity < 10)
  )
print(popularity_check)
# Use boxplots to check outliers
variables_to_check <- c("tempo", "loudness", "duration_min", "popularity")
df_long <- df %>%
  pivot_longer(
    cols = all_of(variables_to_check), 
    names_to = "variable", 
    values_to = "value"
  )
ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, alpha = 0.5) +
  labs(
    title = "Boxplots of Non-Standardized Variables",
    subtitle = "Distribution and Outliers of Duration, Loudness, Popularity, and Tempo",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Check extreme values for audio features
feature_check <- df %>%
  summarise(
    extreme_danceability = sum(danceability < 0.001 | danceability > 0.99),
    extreme_energy = sum(energy < 0.001 | energy > 0.99),
    extreme_acousticness = sum(acousticness < 0.001 | acousticness > 0.99),
    extreme_valence = sum(valence < 0.001 | valence > 0.99),
    extremely_tempo = sum(tempo < 40| tempo > 250),  
    extremely_loudness = sum(loudness > 5 | loudness < -50), 
    extreme_instrumentalness = sum(instrumentalness < 0.001 | instrumentalness > 0.99),
    extreme_speechiness = sum(speechiness > 0.66),
    extreme_liveness = sum(liveness > 0.8)
  )
print(feature_check)

# Check distribution of explicit content
explicit_counts <- df %>%
  count(explicit) %>%
  mutate(percentage = round(n/sum(n)*100, 2)) %>%
  arrange(desc(n))
print(explicit_counts)

# Remove rows where tempo = 0 as these values make no logical sense
df <- df %>% filter(tempo != 0)

# Feature standardization
df <- df %>%
  mutate(
    tempo_scaled = rescale(tempo, to = c(0, 1)),
    loudness_scaled = rescale(loudness, to = c(0, 1)),
    duration_min_scaled = rescale(duration_min, to = c(0, 1))
  )
summary(df[, c("tempo_scaled", "loudness_scaled", "duration_min_scaled")])

# ====== 3. Exploratory Data Analysis ======
# Plot distributions of numeric variables
all_numeric_vars <- c("popularity", "duration_min", "danceability", "energy", 
                      "acousticness", "valence", "tempo", "loudness", 
                      "instrumentalness", "speechiness", "liveness")

df_long_all <- df %>%
  pivot_longer(cols = all_of(all_numeric_vars), 
               names_to = "Variable", 
               values_to = "Value")

# Plot distribution of all variables
ggplot(df_long_all, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free", ncol = 4) +
  labs(
    title = "Distribution of All Audio Features ",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),  # Adjust facet label size
    axis.text = element_text(size = 8),                 # Adjust axis label size
    legend.position = "none"                            # Remove legend
  )

# Plot duration distribution
ggplot(df, aes(x = duration_min)) +
  geom_density(fill = "blue", alpha = 0.5) +
  xlim(0, 20) + 
  labs(
    title = "Density Plot of Duration (Minutes)",
    x = "Duration (Minutes)",
    y = "Density"
  ) +
  theme_minimal()

# Plot instrumentalness distribution
ggplot(df, aes(x = instrumentalness)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(
    title = "Density Plot of Instrumentalness",
    x = "Instrumentalness",
    y = "Density"
  ) +
  theme_minimal()
skim(df$instrumentalness)

# ====== 4. Feature Relationship Analysis ======
# Correlation analysis
library(corrplot)
library(reshape2)
cor_matrix <- cor(df[, c("popularity", "danceability", "energy", "acousticness", 
                         "valence", "tempo", "loudness", "instrumentalness", 
                         "speechiness", "liveness", "duration_min")], use = "complete.obs")

cor_melt <- melt(cor_matrix)

# Plot correlation matrix
ggplot(data = cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Add grid lines between tiles
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",  # Color gradient
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"  # Legend title
  ) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +  # Display correlation coefficients
  theme_minimal() +  # Use minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),  # X-axis labels
    axis.text.y = element_text(size = 10, face = "bold"),  # Y-axis labels
    plot.title = element_text(size = 14, face = "bold"),  # Title style
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "right"  # Legend placement on the right
  ) +
  labs(
    title = "Correlation Matrix",  # Chart title
    x = NULL,  # Remove x-axis title
    y = NULL   # Remove y-axis title
  )

# ====== 5. Popularity Influence Analysis ======
# Linear regression analysis
model_rq1 <- lm(popularity ~ danceability + energy + acousticness + valence +
                  tempo + loudness + instrumentalness + speechiness + liveness + duration_min, data = df)

summary(model_rq1)

# Stepwise regression
library(MASS)
step_model <- stepAIC(model_rq1, direction = "both")
summary(step_model)

# Check multicollinearity
library(car)
vif(model_rq1)

# Visualize regression coefficients
library(broom)
coef_df <- tidy(model_rq1) %>%
  filter(term != "(Intercept)") %>%  # Remove intercept
  arrange(desc(abs(estimate)))      # Sort by absolute value

ggplot(coef_df, aes(x = reorder(term, abs(estimate)), y = estimate, fill = estimate > 0)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = round(estimate, 2)), vjust = -0.5, size = 3) +  # Display coefficient values
  scale_fill_manual(values = c("red", "blue"), guide = "none") +       # Color differentiation for positive/negative
  labs(
    title = "Regression Coefficients",
    x = "Features",
    y = "Coefficient"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate labels
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

# Plot histogram of popularity
ggplot(df, aes(x = popularity)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Popularity (Optimized)",
    x = "Popularity",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Plot density of popularity
ggplot(df, aes(x = popularity)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  labs(
    title = "Density Plot of Popularity (Optimized)",
    x = "Popularity",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# ====== 6. Popularity Group Analysis ======
# Group statistics by popularity levels
group_summary <- df %>%
  mutate(popularity_group = case_when(
    popularity >= 80 ~ "High",
    popularity >= 20 ~ "Medium",
    TRUE ~ "Low"
  )) %>%
  group_by(popularity_group) %>%
  summarise(across(c(danceability, energy, valence, tempo, loudness, 
                     acousticness, speechiness, instrumentalness, liveness, duration_min), 
                   mean, na.rm = TRUE))
print(group_summary)

# ANOVA and post-hoc analysis
df_grouped <- df %>%
  mutate(popularity_group = case_when(
    popularity >= 80 ~ "High",
    popularity >= 20 ~ "Medium",
    TRUE ~ "Low"
  ))

audio_features <- c("danceability", "energy", "valence", "tempo", 
                    "loudness", "acousticness", "speechiness", 
                    "instrumentalness", "liveness", "duration_min")

anova_results <- lapply(audio_features, function(feat) {
  formula <- as.formula(paste(feat, "~ popularity_group"))
  aov_result <- aov(formula, data = df_grouped)
  tukey <- TukeyHSD(aov_result)
  return(list(anova = summary(aov_result), tukey = tukey))
})

for(i in seq_along(anova_results)) {
  cat("\n=== Feature:", audio_features[i], "===\n")
  print(anova_results[[i]]$anova)
  cat("\n--- Post-hoc Analysis ---\n")
  print(anova_results[[i]]$tukey)
}

features_to_plot <- c("instrumentalness", "loudness", "danceability")
df_long <- df_grouped %>%
  pivot_longer(cols = all_of(features_to_plot),
               names_to = "feature", 
               values_to = "value")

ggplot(df_long, aes(x = popularity_group, y = value, fill = popularity_group)) +
  geom_boxplot() +
  facet_wrap(~feature, scales = "free_y") +
  theme_minimal() +
  labs(title = "Key Features Across Popularity Groups")

anova_summary <- data.frame(
  feature = audio_features,
  f_value = sapply(anova_results, function(x) x$anova[[1]]$`F value`[1]),
  p_value = sapply(anova_results, function(x) x$anova[[1]]$`Pr(>F)`[1])
) %>%
  arrange(desc(f_value))


ggplot(anova_summary, aes(x = reorder(feature, f_value), y = f_value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Features' Impact on Popularity (F-values)",
       x = "Audio Features",
       y = "F-value")

# ====== 7. High Popularity Songs Analysis ======
# Cluster analysis for high popularity songs
high_popularity_songs <- df %>% filter(popularity >= 80)
print(dim(high_popularity_songs))  # Check dimensions of high popularity group
summary(high_popularity_songs)    # Summary statistics

features_to_cluster <- high_popularity_songs[, c("danceability", "energy", "valence", "tempo_scaled", 
                                                 "loudness_scaled", "acousticness", "speechiness", 
                                                 "instrumentalness", "liveness")]

# Determine optimal number of clusters using elbow method
fviz_nbclust(features_to_cluster, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters")
print(fviz_nbclust)

set.seed(123)  # Set random seed

# Perform k-means clustering
kmeans_result <- kmeans(features_to_cluster, centers = 3)
high_popularity_songs$cluster <- as.factor(kmeans_result$cluster)
# View number of samples per cluster
table(high_popularity_songs$cluster)

# PCA for dimensionality reduction
pca_result <- PCA(features_to_cluster, graph = FALSE)
fviz_pca_ind(
  pca_result,
  repel = TRUE,                # Avoid overlapping variable labels
  habillage = high_popularity_songs$cluster,  # Color by cluster
  addEllipses = TRUE,          # Add ellipses for each cluster
  geom = "point",              # Show points only
  pointsize = 2                # Adjust point size
) +
  labs(title = "PCA Clusters without Sample Labels", 
       x = "Dim1 (28.2%)", 
       y = "Dim2 (15.9%)") +
  theme_minimal()


# Variable contribution analysis
fviz_pca_var(
  pca_result, 
  col.var = "contrib",               # Use color to indicate contribution
  gradient.cols = c("blue", "yellow", "red"), # Gradient colors
  repel = TRUE                       # Avoid overlapping labels
) +
  labs(title = "Contribution of Variables to PCA Components")

# ====== 8. Emotional Feature Analysis ======
# Emotional feature statistics
df <- df %>%
  mutate(popularity_group = case_when(
    popularity >= 80 ~ "High",
    popularity >= 20 ~ "Medium",
    TRUE ~ "Low"
  ))

emotion_summary <- df %>%
  group_by(popularity_group) %>%
  summarise(
    mean_valence = mean(valence, na.rm = TRUE),
    mode_ratio = mean(mode, na.rm = TRUE)  # Average value of mode indicates major key ratio
  )
print(emotion_summary)

# Distribution of Valence
ggplot(df, aes(x = popularity_group, y = valence, fill = popularity_group)) +
  geom_boxplot() +
  labs(title = "Valence by Popularity Group", x = "Popularity Group", y = "Valence") +
  theme_minimal()

# ANOVA for emotional features
aov_valence <- aov(valence ~ popularity_group, data = df)
summary(aov_valence)
TukeyHSD(aov_valence)

# Distribution of Mode
chisq.test(table(df$popularity_group, df$mode))
df_mode_table <- df %>%
  group_by(popularity_group, mode) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / sum(count))

ggplot(df_mode_table, aes(x = popularity_group, y = ratio, fill = as.factor(mode))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  labs(title = "Mode Distribution by Popularity Group", x = "Popularity Group", y = "Proportion", fill = "Mode") +
  theme_minimal()




