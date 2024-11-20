# R code to create the 3 stacked bar charts for the 16S data at genus level. First plot is split by treatment, secong by treatment and feeding period and final by Treatment, cow_ID and feeding period.

#20% of data grouped as g_other for visualisation purposes
# Reshape the data from wide to long format
data_long_80per <- Genus_80per %>%
  pivot_longer(cols = starts_with("g_"),  
               names_to = "Taxa",       
               values_to = "Abundance")   

# Calculate the total abundance for each treatment
data_long_80per <- data_long_80per %>%
  group_by(Treatment) %>%
  mutate(Total_Abundance_Per_Treatment = sum(Abundance)) %>%
  ungroup()

# Calculate percentage abundance for each Taxa within each Treatment
data_long_80per <- data_long_80per %>%
  mutate(Percentage_Abundance = (Abundance / Total_Abundance_Per_Treatment) * 100)

# Calculate the total abundance for each Taxa (for ordering purposes)
total_abundance_genus <- data_long_80per %>%
  group_by(Taxa) %>%
  summarise(Total_Abundance = sum(Abundance), .groups = 'drop') %>%
  arrange(Total_Abundance)  # Order from low to high abundance

# Create an ordered factor for Taxa based on total abundance
data_long_80per$Taxa <- factor(data_long_80per$Taxa,
                                   levels = total_abundance_genus$Taxa,
                                   ordered = TRUE)

# Create custom colour palette
custom_colors <- c(
  "#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", "#911EB4", "#42D4F4", "#F032E6", 
  "#BFEF45", "#FABED4", "#469990", "#D2B48C", 
  "#9A6324", "#FFFAC8", "#008080", 
  "#A9A9A9", "#000075", "#AAFFC3", "#808000", "#FFD8B1", "#E6BEFF", "#87CEEB",  
  "#D2691E", "#20B2AA", "#FF4500", "#1E90FF"
)


# Plot with percentage abundance
Genus_plot <- ggplot(data_long_80per, aes(x = Treatment, y = Percentage_Abundance, fill = Taxa)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "a)",
       x = "Treatment",
       y = "Genus Abundance (%)", 
       fill = "Taxa") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + 
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE, reverse = TRUE, position = "bottom")) 

# Print the plot
print(Genus_plot)


# Calculate the total abundance for each Taxa by Treatment and Feeding Period
total_abundance_FP <- data_long_80per100 %>%
  group_by(Taxa) %>%
  summarise(Total_Abundance = sum(Abundance), .groups = 'drop') %>%
  arrange(Total_Abundance)  

# Create an ordered factor for Taxa based on total abundance
data_long_80per100$Taxa <- factor(data_long_80per100$Taxa,
                                      levels = total_abundance_FP$Taxa,
                                      ordered = TRUE)

## order treatments and feeding perids
data_long_80per100$Treatment_Feeding <- factor(interaction(data_long_80per100$Treatment, 
                                                               data_long_80per100$Feeding_period),
                                                   levels = c("BASE.BASE", "CON.P1", "CON.P2", "CON.P3", 
                                                              "HE.P1", "HE.P2", "HE.P3", 
                                                              "XHE.P1", "XHE.P2", "XHE.P3" 
                                                   )) 
# Rename 'BASE.BASE' to 'BASE'
levels(data_long_80per100$Treatment_Feeding) <- gsub("BASE.BASE", "BASE", levels(data_long_80per100$Treatment_Feeding))


# Plot with reordered taxa based on abundance
Genus_T_FP <- ggplot(data_long_80per100, aes(x = Treatment_Feeding, y = Percentage, fill = Taxa)) +
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "b)",
       x = "Treatment and Feeding Period",
       y = "Genus Abundance (%)",
       fill = "Taxa") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  
  scale_fill_manual(values = custom_colors) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE, position = "bottom", reverse = TRUE ))  

# Print the plot
print(Genus_T_FP)


## Split by individual cow ID


# Create a new column combining Treatment, Feeding_period, and Cow_ID
data_long_80per100$Treatment_Feeding_Cow <- factor(interaction(data_long_80per100$Treatment,
                                                                   data_long_80per100$Feeding_period,
                                                                   data_long_80per100$Cow_ID),
                                                       levels = c("BASE.BASE.429", "BASE.BASE.967", "BASE.BASE.238", "BASE.BASE.196", 
                                                                  "BASE.BASE.887", "BASE.BASE.310", "BASE.BASE.421", "BASE.BASE.27", 
                                                                  "BASE.BASE.108", "BASE.BASE.31", "BASE.BASE.434", "BASE.BASE.290", 
                                                                  "BASE.BASE.437", "BASE.BASE.450",  
                                                                  "CON.P1.429", "CON.P1.967", "CON.P1.238", "CON.P1.196", 
                                                                  "CON.P1.887", "CON.P1.310", "CON.P1.421", "CON.P1.27", 
                                                                  "CON.P1.108", "CON.P1.31", "CON.P1.434", "CON.P1.290", 
                                                                  "CON.P1.437", "CON.P1.450", 
                                                                  "CON.P2.429", "CON.P2.967", "CON.P2.238", "CON.P2.196", 
                                                                  "CON.P2.887", "CON.P2.310", "CON.P2.421", "CON.P2.27", 
                                                                  "CON.P2.108", "CON.P2.31", "CON.P2.434", "CON.P2.290", 
                                                                  "CON.P2.437", "CON.P2.450", 
                                                                  "CON.P3.429", "CON.P3.967", "CON.P3.238", "CON.P3.196", 
                                                                  "CON.P3.887", "CON.P3.310", "CON.P3.421", "CON.P3.27", 
                                                                  "CON.P3.108", "CON.P3.31", "CON.P3.434", "CON.P3.290", 
                                                                  "CON.P3.437", "CON.P3.450", 
                                                                  "HE.P1.429", "HE.P1.967", "HE.P1.238", "HE.P1.196", 
                                                                  "HE.P1.887", "HE.P1.310", "HE.P1.421", "HE.P1.27", 
                                                                  "HE.P1.108", "HE.P1.31", "HE.P1.434", "HE.P1.290", 
                                                                  "HE.P1.437", "HE.P1.450", 
                                                                  "HE.P2.429", "HE.P2.967", "HE.P2.238", "HE.P2.196", 
                                                                  "HE.P2.887", "HE.P2.310", "HE.P2.421", "HE.P2.27", 
                                                                  "HE.P2.108", "HE.P2.31", "HE.P2.434", "HE.P2.290", 
                                                                  "HE.P2.437", "HE.P2.450", 
                                                                  "HE.P3.429", "HE.P3.967", "HE.P3.238", "HE.P3.196", 
                                                                  "HE.P3.887", "HE.P3.310", "HE.P3.421", "HE.P3.27", 
                                                                  "HE.P3.108", "HE.P3.31", "HE.P3.434", "HE.P3.290", 
                                                                  "HE.P3.437", "HE.P3.450", 
                                                                  "XHE.P1.429", "XHE.P1.967", "XHE.P1.238", "XHE.P1.196", 
                                                                  "XHE.P1.887", "XHE.P1.310", "XHE.P1.421", "XHE.P1.27", 
                                                                  "XHE.P1.108", "XHE.P1.31", "XHE.P1.434", "XHE.P1.290", 
                                                                  "XHE.P1.437", "XHE.P1.450", 
                                                                  "XHE.P2.429", "XHE.P2.967", "XHE.P2.238", "XHE.P2.196", 
                                                                  "XHE.P2.887", "XHE.P2.310", "XHE.P2.421", "XHE.P2.27", 
                                                                  "XHE.P2.108", "XHE.P2.31", "XHE.P2.434", "XHE.P2.290", 
                                                                  "XHE.P2.437", "XHE.P2.450", 
                                                                  "XHE.P3.429", "XHE.P3.967", "XHE.P3.238", "XHE.P3.196", 
                                                                  "XHE.P3.887", "XHE.P3.310", "XHE.P3.421", "XHE.P3.27", 
                                                                  "XHE.P3.108", "XHE.P3.31", "XHE.P3.434", "XHE.P3.290", 
                                                                  "XHE.P3.437", "XHE.P3.450"))

# Rename 'BASE.BASE.429' to 'BASE 429' (and similarly for other combinations) for more readable labels
levels(data_long_80per100$Treatment_Feeding_Cow) <- gsub("BASE.BASE", "BASE", levels(data_long_80per100$Treatment_Feeding_Cow))

# Plot with reordered taxa based on abundance and grouping by Cow_ID
Genus_T_FP_Cow_ID <- ggplot(data_long_80per100, aes(x = Treatment_Feeding_Cow, y = Percentage, fill = Taxa)) +
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "c)",
       x = "Treatment, Feeding Period and Cow ID",
       y = "Genus Abundance (%)",
       fill = "Taxa") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(fill = guide_legend(nrow = 5, byrow = TRUE, position = 'bottom', reverse = TRUE))  

# Print the plot
print(Genus_T_FP_Cow_ID)
