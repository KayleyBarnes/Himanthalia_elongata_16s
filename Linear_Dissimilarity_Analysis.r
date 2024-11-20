# Linear Dissimilarity Analysis code. The below was used to complete all of the LDA pairs. The example provied is for CON vs XHE treatments at genus level. 

# Combind dataframes
CONXHE_data <- rbind(con_data,xhe_data)

# Select taxonomic data (treatment name = column 1 and features from column 2)
X3 <- CONXHE_data[, 2:ncol(CONXHE_data)]
# Ensure that treatment column contains all relevant levels for pairwise comparisons
y3 <- as.factor(CONXHE_data[, 1]) 

# Remove columns with zero variance
non_zero_var_columns3 <- apply(X3, 2, sd) != 0
X3 <- X3[, non_zero_var_columns3]

# Check for collinearity
cor_matrix3 <- cor(X3)
high_corr_features3 <- findCorrelation(cor_matrix3, cutoff = 0.9)  # Adjust cutoff as needed
X_filtered3 <- X3[, -high_corr_features3]  # Remove highly correlated features

# Create a data frame with the taxonomic features and treatment labels for ANOVA
anova_data3 <- data.frame(Treatment = y3, X_filtered3)

# Perform pairwise comparisons using ANOVA and Tukey's HSD for each feature
anova_results3 <- lapply(names(anova_data3[, -1]), function(feature) {
  formula3 <- as.formula(paste(feature, "~ Treatment"))
  anova_model3 <- aov(formula3, data = anova_data3)
  tukey_result3 <- TukeyHSD(anova_model3)
  
  # Convert the Tukey's HSD result into a data frame for easier tabulation
  tukey_df3 <- as.data.frame(tukey_result3$Treatment)
  tukey_df3$Feature <- feature  # Associate the taxonomic feature name
  return(tukey_df3)
})

# Combine individual feature results into a single table
pairwise_comparisons_table3 <- do.call(rbind, anova_results3)

# Perform LDA
lda_model3 <- lda(X_filtered3, grouping = y3)

# Display the LDA model summary
print(summary(lda_model3))

# Extract linear discriminant coefficients from the LDA model
scaling_dfBaseCON <- as.data.frame(lda_model3$scaling)
scaling_dfBaseCON$Feature <- rownames(scaling_dfBaseCON)

# Add a column to determine association based on the sign of the coefficient
scaling_dfBaseCON$Association <- ifelse(scaling_dfBaseCON[,1] > 0, "CON", "XHE")

# Filter scaling data to keep only significant features
scaling_melt_filtered3 <- melt(scaling_dfBaseCON, id.vars = c("Feature", "Association"))

# Merge p-values into the melted scaling data frame using the correct feature name
scaling_melt_filtered3 <- merge(scaling_melt_filtered3, 
                                pairwise_comparisons_table3[, c("Feature", "p adj")], 
                                by = "Feature", 
                                all.x = TRUE)

# Rename 'p adj' to 'p_adj'
colnames(scaling_melt_filtered3)[colnames(scaling_melt_filtered3) == "p adj"] <- "p_adj"

# Ensure p_adj is numeric and round it for better readability
scaling_melt_filtered3$p_adj <- round(as.numeric(as.character(scaling_melt_filtered3$p_adj)), 3)

# Filter out features with p_adj >= 0.1 (keep only significant features)
scaling_melt_filtered3 <- scaling_melt_filtered3[scaling_melt_filtered3$p_adj < 0.05, ]

# Plot with p-values displayed 
CONXHE_genus_lda <- ggplot(scaling_melt_filtered3, aes(y = reorder(Feature, value), x = value, fill = Association)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = ifelse(!is.na(p_adj), p_adj, "")),  # Show p-values where available
            hjust = -0.2,  # Adjust to position the text outside the bars
            size = 3,      # Adjust text size for readability
            color = "black") +
  labs(title = "b) CON ~ XHE",
       y = "Genus",
       x = "LDA Score") +
  theme(axis.text.y = element_text(hjust = 1)) +
  scale_fill_manual(values = c("CON" = "#333FFC", "XHE" = "#00CCFF")) +
  scale_x_continuous(limits = c(-0.75, 0.75)) +  # Set x-axis limits
  guides(fill = guide_legend(title = "Treatment", position = "bottom")) +
  theme_light()
print(CONXHE_genus_lda)


