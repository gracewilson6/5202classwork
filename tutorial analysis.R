#box plot for visualizing incidence of CRC
# Load libraries
library(ggplot2)
library(dplyr)

# create object for data for bar graph
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
    x = "Colorectal Cancer Incidence (0 = No, 1 = Yes)",
    y = "Mean Serum Vitamin D Level",
    fill = "Cancer Incidence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )
  


# plot to display vitamin D levels compared to types of CRC

#create object from 
cancer_types <- read.csv("https://raw.githubusercontent.com/gracewilson6/5202classwork/main/typesofcancer.csv")


# Summarize by 'cancer', not 'model'
cancer_summary <- cancer_types %>%
  group_by(cancer) %>%
  summarize(mean_VD = mean(VD, na.rm = TRUE)) %>%
  arrange(mean_VD)

# Create an ordered factor for 'cancer', not 'model'
cancer_types$cancer_ordered <- factor(cancer_types$cancer, levels = cancer_summary$cancer)

# Now plot
ggplot(cancer_types) +
  stat_summary(
    aes(x = VD, y = cancer_ordered), 
    fun.min = min,
    fun.max = max,
    fun = mean,
    geom = "pointrange"
)

#box plot for same data 
library(ggplot2)

ggplot(cancer_types, aes(x = cancer, y = VD)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Vitamin D Levels by Cancer Type",
    x = "Cancer Type",
    y = "Vitamin D Level (ng/mL)"
  ) +
  theme_minimal() +
  theme(
    title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ridge plot 

install.packages("ggridges")
library(ggridges)

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



# world map


# Install these packages if you don't have them yet:
# install.packages(c("ggplot2", "dplyr", "sf", "rnaturalearth", "rnaturalearthdata"))

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. Load your data
cancer_regions <- read.csv("https://raw.githubusercontent.com/gracewilson6/5202classwork/main/typesofcancer.csv")


# Example: your_data should have columns like 'Country' and 'VD'

# 2. Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Prepare your data
# (make sure country names match -- sometimes you may need to fix spelling!)
# Optionally, aggregate by country
your_data_summary <- your_data %>%
  group_by(Country) %>%
  summarize(mean_VD = mean(VD, na.rm = TRUE))

# 4. Join your data to the world map
world_data <- left_join(world, your_data_summary, by = c("name" = "Country"))

# 5. Plot
ggplot(world_data) +
  geom_sf(aes(fill = mean_VD)) +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") +
  labs(
    title = "Average Vitamin D Levels by Country",
    fill = "Vitamin D"
  ) +
  theme_minimal()
