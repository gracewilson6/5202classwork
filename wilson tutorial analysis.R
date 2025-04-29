
#BAR PLOT FOR VISUALIZING MEAN VITAMIN D WITH INCIDENCE OF COLORECTAL CANCER

# Load libraries
library(ggplot2)
library(dplyr)

# create object for data from CSV
VitD <- read.csv("https://raw.githubusercontent.com/gracewilson6/5202classwork/main/tutorialanalysis.csv")

# Summarize mean and standard error
summary_VitD <- VitD %>%
  group_by(CRC) %>%
  summarise(
    mean_vitamin_d = mean(VD, na.rm = TRUE),
    se = sd(VD, na.rm = TRUE) / sqrt(n())
  )

# Create bar plot
ggplot(summary_VitD, aes(x = factor(CRC), y = mean_vitamin_d, fill = factor(CRC))) +
  geom_bar(stat = "identity", width = 0.2, color = "black") +
  geom_errorbar(aes(ymin = mean_vitamin_d - se, ymax = mean_vitamin_d + se),
                width = 0.1, color = "black") +
  geom_text(aes(label = round(mean_vitamin_d, 1)),
            vjust = -3, size = 5) +
  labs(
    title = "Incidence of Colorectal Cancer vs Serum Vitamin D",
    x = "Colorectal Cancer Incidence",
    y = "Mean Serum Vitamin D Level",
    fill = "Cancer Incidence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )
  



# PLOTS TO DISPLAY VITAMIN D LEVELS GROUPED BY CANCER TYPE

#create object from .csv file
cancer_types <- read.csv("https://raw.githubusercontent.com/gracewilson6/5202classwork/main/typesofcancer.csv")


# Summarize by 'cancer'
cancer_summary <- cancer_types %>%
  group_by(cancer) %>%
  summarize(mean_VD = mean(VD, na.rm = TRUE)) %>%
  arrange(mean_VD)

# Create an ordered factor for 'cancer'
cancer_types$cancer_ordered <- factor(cancer_types$cancer, levels = cancer_summary$cancer)

#plot

# Background shading regions with ordered factor
bg_regions <- data.frame(
  xmin = c(0, 12, 20, 50),
  xmax = c(12, 20, 50, 80),
  fill = factor(c("red", "yellow", "lightgreen", "darkgreen"),
                levels = c("red", "yellow", "lightgreen", "darkgreen")),  # Order for legend
  label = c("Deficient (0–12)", "Insufficient (12–20)", "Sufficient (20–50)", "High (50–80)")
)
# Named vector for custom labels
fill_labels <- setNames(bg_regions$label, bg_regions$fill)
# Plot with ordered legend
ggplot(cancer_types) +
  # Background shading with legend
  geom_rect(data = bg_regions,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE,
            alpha = 0.2,
            show.legend = TRUE) +
  # Custom fill scale with ordered labels
  scale_fill_manual(
    name = "Vitamin D Category",
    values = setNames(as.character(bg_regions$fill), bg_regions$fill),
    labels = fill_labels
  ) +
  # Pointrange for vitamin D stats
  stat_summary(
    aes(x = VD, y = cancer_ordered),
    fun.min = min,
    fun.max = max,
    fun = mean,
    geom = "pointrange"
  ) +
  # Labels and theme
  labs(
    title = "Vitamin D Levels by Cancer Type",
    x = "Serum Vitamin D (ng/mL)",
    y = "Cancer Type"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.position = "right"
  )



# ridge plot to analyze distribution of vitamin D levels across patients/cancer types

#install packages for appropriate plot
install.packages("ggridges")
library(ggridges)

#create graph from object created from .csv data
ggplot(cancer_types, aes(x = VD, y = cancer)) +
  geom_density_ridges(fill = "lightblue") +
  labs(
    title = "Vitamin D Levels by Cancer Type",
    y = "Cancer Type",
    x = "Vitamin D Level (ng/mL)"
  ) +
  theme_minimal() + 
theme(
  title = element_text(face = "bold"),
  axis.title.x = element_text(face = "bold"),
  axis.title.y = element_text(face = "bold"),
  axis.text.x = element_text(angle = 45, hjust = 1)
)



