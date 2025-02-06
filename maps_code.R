library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(magrittr)
library (patchwork)
library(ggpubr) 
library(stringr)
library(readxl)

balch2023 <- read_excel("Balch2023.xlsx", sheet="Sheet2") #data from the 2018 COCCOMIX dataset (https://www.science.org/doi/10.1126/sciadv.adf6973)
data_new <- read.csv("literature_abundances0.1.3.csv")  %>%   #data from the CASCADE database, downloaded from Zenodo, from the concatenated literature datasets, CASACDE verrsion v0.1.3 (https://zenodo.org/records/13919889)
  filter(!(Latitude == 51.56 & Longitude == 129.40 & Reference == "Okada1973"))  #removing the point from the Okada 1973 dataset that is located on land (likely a misannotation in the original paper)
data_new <-rbind(data_new, balch2023)   #combining data from CASCADE and COCCOMIX

world_map <- map_data("world")

##plotting all the distinct points from CASCADE and COCCOMIX on a map
data_new$Latitude <- as.numeric(data_new$Latitude)
data_new$Longitude <- as.numeric(data_new$Longitude)
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = data_new, aes(x = Longitude, y = Latitude), color = "seagreen4", size = 1.5, alpha = 0.8) +
  theme_minimal() +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  theme(
    #plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 25),                 # X-axis text size
    axis.text.y = element_text(size = 25),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  ) +
  labs(x = "Longitude",
       y = "Latitude")

##plots of abundances of the 4 main Syracosphaeraceae genera combined

#extracting only data for Syracosphaera genus
syraco_data <- data_new %>%
  filter(grepl("Syracosphaera", Species, ignore.case = TRUE)) %>% 
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>% 
  rename_with(~ gsub("cells.L.1", "abundance", .))

#extracting only data for Calciopappus genus
calcio_data <- data_new %>%
  filter(grepl("Calciopappus", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

#extracting only data for Ophiaster genus
ophiaster_data <- data_new %>%
  filter(grepl("Ophiaster", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

#extracting only data for Michaelsarsia genus
micha_data <- data_new %>%
  filter(grepl("Michaelsarsia", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

#defining breaks of abundance values to be shown on the plots
global_breaks = c(0.5e04, 1e04, 1e05, 1e06, 1.5e06)
global_labels <- sapply(global_breaks, function(x) {
  scientific_parts <- strsplit(format(x, scientific = TRUE), "e")[[1]]
  coefficient <- scientific_parts[1]
  exponent <- gsub("\\+?", "", scientific_parts[2])  # Remove the "+" sign if present
  parse(text = paste0(coefficient, " %*% 10^", exponent))
})

#checking ranges of abundance to set the limits accordingly
range(syraco_data$abundance, na.rm = TRUE)
range(calcio_data$abundance, na.rm = TRUE)
range(ophiaster_data$abundance, na.rm = TRUE)
range(micha_data$abundance, na.rm = TRUE)   

syraco_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = syraco_data, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.5, na.rm=TRUE) +
  scale_color_manual(values = c("HET" = "blue", "HOL" = "red")) +
  scale_size_continuous(range = c(1,5),limits = c(min(0.5e04, 0), max(1.5e06, 87754)), labels = global_labels, breaks=global_breaks)+  
    theme_minimal() +
    coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cell L" ^ {-1} * ")")), color = "Type") +
  theme(
    #plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 20),                # X-axis title size
    axis.title.y = element_text(size = 20),                # Y-axis title size
    axis.text.x = element_text(size = 15),                 # X-axis text size
    axis.text.y = element_text(size = 15),                 # Y-axis text size
    legend.title = element_text(size = 20),                # Legend title size
    legend.text = element_text(size = 20)                 # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5), order=2),
         size=guide_legend(order=1))

calcio_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = calcio_data, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1,5),limits = c(min(0.5e04, 3), max(1.5e06, 359200)), labels = global_labels, breaks=global_breaks) + 
    theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cell L" ^ {-1} * ")")), color = "Type") +
  theme(
    #plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 20),                # X-axis title size
    axis.title.y = element_text(size = 20),                # Y-axis title size
    axis.text.x = element_text(size = 15),                 # X-axis text size
    axis.text.y = element_text(size = 15),                 # Y-axis text size
    legend.title = element_text(size = 20),                # Legend title size
    legend.text = element_text(size = 20)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))


ophiaster_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = ophiaster_data, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1,5), limits = c(min(0.5e04, 5), max(1.5e06)), labels = global_labels, breaks=global_breaks)+  
    theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cell L" ^ {-1} * ")")), color = "Type") +
  theme(
    #plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 20),                # X-axis title size
    axis.title.y = element_text(size = 20),                # Y-axis title size
    axis.text.x = element_text(size = 15),                 # X-axis text size
    axis.text.y = element_text(size = 15),                 # Y-axis text size
    legend.title = element_text(size = 20),                # Legend title size
    legend.text = element_text(size = 20)                  # Legend text size
  ) +
  guides(color = guide_legend(override.aes = list(size = 4.5)))

micha_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = micha_data, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1,5), limits = c(min(0.5e04, 2), max(1.5e06,18000)), labels = global_labels, breaks=global_breaks)+  
    theme_minimal() +
    coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cell L" ^ {-1} * ")")), color = "Type") +
  theme(
    #plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 20),                # X-axis title size
    axis.title.y = element_text(size = 20),                # Y-axis title size
    axis.text.x = element_text(size = 15),                 # X-axis text size
    axis.text.y = element_text(size = 15),                 # Y-axis text size
    legend.title = element_text(size = 20),                # Legend title size
    legend.text = element_text(size = 20)                  # Legend text size
  ) +
  guides(color = guide_legend(override.aes = list(size = 4.5)))  # Adjust legend circle size for Type

genera_plots <- ggarrange(syraco_plot, micha_plot, calcio_plot, ophiaster_plot,
                          labels = c("a)", "b)", "c)", "d)"),
                          font.label = list(size = 20),
                          common.legend = TRUE,
                          legend = "right",
                          ncol = 2, # Arrange plots in 2 columns
                          nrow = 2, # Arrange plots in 2 rows
                          align = "hv")# Align both horizontally and vertically


#checking which Syracosphaera species appear in the highest abundance in the highest number of studies
syraco_data_combined <- syraco_data %>%
  mutate(Species = str_replace(Species, " HET.*| HOL.*| COMB.*", ""))  #removing the HET,HOL,COMB specifiers so that all types of the same species can be clustered together

syraco_abundance_data <- syraco_data_combined %>%
  group_by(Species) %>%
  summarise(total_abundance = sum(`abundance`, na.rm = TRUE)) 

# Get the order of species based on total abundance
ordered_species <- syraco_abundance_data %>%
  arrange(desc(total_abundance)) %>%
  pull(Species)

scientific_10 <- function(x) {
  parse(text = gsub("e\\+?", " %*% 10^", sprintf("%.1e", x)))
}

p1 <- ggplot(syraco_abundance_data, aes(x = reorder(Species, -total_abundance), y = total_abundance)) +
  geom_bar(stat = "identity", fill = "grey40") +
  #geom_text(aes(label = total_abundance), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(x = NULL, y = expression("Total abundance (cell L" ^ {-1} * ")")) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),    # Larger font for y-axis title
        axis.text.y = element_text(size = 12)) +   # Larger font for y-axis labels
  scale_y_continuous(labels = scientific_10)

# Set the levels of Species based on the order from p1
syraco_data_combined$Species <- factor(syraco_data_combined$Species, levels = ordered_species)   

# Create the dot plot for references below the bar plot
p2 <- ggplot(syraco_data_combined, aes(x = Species, y = Reference)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "italic", size = 12),  # Larger font for species labels
        axis.text.y = element_text(size = 10, lineheight = 1500),  # Larger font for reference names + extra spacing
        axis.title.x = element_text(size = 14),    # Larger font for x-axis title
        axis.title.y = element_text(size = 14))    # Larger font for y-axis title

# Combine the two plots vertically using patchwork or gridExtra
p1 / p2

##plotting the distribution of  Syracoshpaera species with cumulative abundance higher than 2.5*10^5, in decreasing order of cumulative abundance

#Syracosphaera_molischii
smolischii_data <- data_new %>%
  filter(grepl("Syracosphaera molischii", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

smolischii_data_het <- smolischii_data %>%
  filter(Type == "HET")
smolischii_data_hol <- smolischii_data %>%
  filter(Type == "HOL") 

smolischii_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = smolischii_data_het, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 5), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(1e04, 2e04, 3e04, 4e04))+  
  theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera molischii") * " abundance"),
       x = "Longitude", y = "Latitude", size =  expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
       subtitle = "a)" ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                 # Legend text size
  ) +
  guides(color = guide_legend(override.aes = list(size = 4.5)))

smolischii_hol_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = smolischii_data_hol, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HOL" = "red")) +
  scale_size_continuous(range = c(2, 5), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(1e04, 2e04, 3e04, 4e04)) +  
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(#title = expression(italic("Syracosphaera pulchra") * " abundance"),
    x = "Longitude", y = "Latitude", size =  expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
    subtitle = "b)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  ) +
  guides(color = guide_legend(override.aes = list(size = 4.5), order=2),
         size=guide_legend(order=1))

smolischii_het_plot/smolischii_hol_plot

#Syracosphaera_pulchra
spulch_data <- data_new %>%
  filter(grepl("Syracosphaera pulchra", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

spulch_data_het <- spulch_data %>%
  filter(Type == "HET")
spulch_data_hol <- spulch_data %>%
  filter(Type == "HOL") 

spulch_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = spulch_data_het, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 5), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(1e04, 2e04, 3e04, 4e04))+  
   theme_minimal() +
    coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera pulchra") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
       subtitle="a)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                 # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

spulch_hol_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = spulch_data_hol, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HOL" = "red")) +
  scale_size_continuous(range = c(1, 3), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
  }, breaks = c(2e03, 4e03, 6e03)) +  
  theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(#title = expression(italic("Syracosphaera pulchra") * " abundance"),
    x = "Longitude", y = "Latitude", size =expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
    subtitle = "b)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                 # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

spulch_het_plot/spulch_hol_plot

#Syracosphaera_rotula
srotula_data <- data_new %>%
  filter(grepl("Syracosphaera rotula", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

srotula_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = srotula_data, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(2, 6), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(2e04, 4e04, 6e04, 8e04))+  
  theme_minimal() +
    coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera rotula") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

#Syracosphaera_histrica
shistrica_data <- data_new %>%
  filter(grepl("Syracosphaera histrica", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

shistrica_data_het <- shistrica_data %>%
  filter(Type == "HET")
shistrica_data_hol <- shistrica_data %>%
  filter(Type == "HOL") 

shistrica_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = shistrica_data_het, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 3), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
  }, breaks = c(1e03, 2e03, 3e03, 4e03, 5e03)) +  
    theme_minimal() +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera histrica") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
       subtitle = "a)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

shistrica_hol_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = shistrica_data_hol, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HOL" = "red")) +
  scale_size_continuous(range = c(2, 5), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(1e04, 2e04, 3e04, 4e04)) +  
    theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(#title = expression(italic("Syracosphaera pulchra") * " abundance"),
    x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
    subtitle = "b)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5), order=2),
         size=guide_legend(order=1))

shistrica_het_plot/shistrica_hol_plot

#Syracosphaera_ossa
sossa_data <- data_new %>%
  filter(grepl("Syracosphaera ossa", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

sossa_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = sossa_data, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(0.5, 3.5), 
                        labels = function(x) {
                          sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
                        }, breaks = c(2e03, 4e03, 6e03, 8e03)) +    
    theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera ossa") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

#Syracosphaera_arethusae
saret_data <- data_new %>%
  filter(grepl("Syracosphaera arethusae", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

saret_data_het <- saret_data %>%
  filter(Type == "HET")
saret_data_hol <- saret_data %>%
  filter(Type == "HOL")

saret_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = saret_data_het, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 3.5), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(0.5e04, 1e04, 1.5e04))+  
    theme_minimal() +
    coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera arethusae") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
       subtitle="a)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))


saret_hol_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = saret_data_hol, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HOL" = "red")) +
  scale_size_continuous(range = c(2, 4), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(1e04, 2e04, 3e04))+  
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(#title = expression(italic("Syracosphaera arethusae") * " abundance"),
    x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
    subtitle = "b)")+
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5), order=2),
         size=guide_legend(order=1))

saret_het_plot/saret_hol_plot

#Syracosphaera_mediterranea
smedit_data <- data_new %>%
  filter(grepl("Syracosphaera mediterranea", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

smedit_data_het <- smedit_data %>%
  filter(Type == "HET")
smedit_data_hol <- smedit_data %>%
  filter(Type == "HOL") 

smedit_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = smedit_data_het, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 3), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
  }, breaks = c(2e03, 4e03, 6e03))+  
    theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera mediterranea") * " abundance"),
       x = "Longitude", y = "Latitude", size =  expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
       subtitle = "a)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))


smedit_hol_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = smedit_data_hol, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HOL" = "red")) +
  scale_size_continuous(range = c(1, 3),  labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
  }, breaks = c(2e03, 4e03, 6e03)) +
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(#title = expression(italic("Syracosphaera pulchra") * " abundance"),
    x = "Longitude", y = "Latitude", size =  expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
    subtitle = "b)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5), order=2),
         size=guide_legend(order=1))

smedit_het_plot/smedit_hol_plot

#Syracosphaera_protrudens
sprot_data <- data_new %>%
  filter(grepl("Syracosphaera protrudens", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

sprot_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = sprot_data, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue"))+  
  scale_size_continuous(range = c(1, 4), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(0.5e04, 1e04, 1.5e04, 2e04))+   
   theme_minimal() +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera protrudens") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

#Syracosphaera_nodosa
snodosa_data <- data_new %>%
  filter(grepl("Syracosphaera nodosa", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .)) 

snodosa_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = snodosa_data, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue"))+
  scale_size_continuous(range = c(1, 5) , labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(0.5e04, 1e04, 1.5e04, 2e04, 2.5e04)) +  
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera nodosa") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

#Syracosphaera_corolla
scor_data <- data_new %>%
  filter(grepl("Syracosphaera corolla", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

scor_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = scor_data, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(2, 4), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(1e04, 2e04, 3e04))+  
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera corolla") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

#Syracosphaera_marginiporata
smarg_data <- data_new %>%
  filter(grepl("Syracosphaera marginiporata", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

smarg_data_het <- smarg_data %>%
  filter(Type == "HET")
smarg_data_hol <- smarg_data %>%
  filter(Type == "HOL")

smarg_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = smarg_data_het, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 5), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(1e04, 2e04, 3e04))+ 
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera marginiporata") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
       subtitle="a)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

smarg_hol_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = smarg_data_hol, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HOL" = "red")) +
  scale_size_continuous(range = c(1, 3), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
  }, breaks = c(2e03, 4e03, 6e03))+ 
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(#title = expression(italic("Syracosphaera arethusae") * " abundance"),
    x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
    subtitle="b)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5), order=2),
         size=guide_legend(order=1))

smarg_het_plot/smarg_hol_plot

#Syracosphaera_halldalii
shald_data <- data_new %>%
  filter(grepl("Syracosphaera halldalii", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

shald_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = shald_data, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 4), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e04, " %*% 10^4")))
  }, breaks = c(0.5e04, 1e04, 1.5e04, 2e04))+   
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera halldalii") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    #plot.subtitle = element_text(size = 16, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

#Syracosphaera_anthos
santhos_data <- data_new %>%
  filter(grepl("Syracosphaera anthos", Species, ignore.case = TRUE)) %>%
  mutate(Type = case_when(
    grepl("HET", Species, ignore.case = TRUE) ~ "HET",
    grepl("HOL", Species, ignore.case = TRUE) ~ "HOL",
    grepl("COMB", Species, ignore.case = TRUE) ~ "COMB",
    TRUE ~ NA_character_)) %>%  # This will assign NA if none of the conditions are met
  rename_with(~ gsub("cells.L.1", "abundance", .))

santhos_data_het <- santhos_data %>%
  filter(Type == "HET")
santhos_data_hol <- santhos_data %>%
  filter(Type == "HOL")

santhos_het_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = santhos_data_het, aes(x= Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HET" = "blue")) +
  scale_size_continuous(range = c(1, 3), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
  }, breaks = c(2e03, 4e03, 6e03, 8e03)) +  
    theme_minimal() +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(title = expression(italic("Syracosphaera anthos") * " abundance"),
       x = "Longitude", y = "Latitude", size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")), color = "Type",
       subtitle="a)")  +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

santhos_hol_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_point(data = santhos_data_hol, aes(x = Longitude, y = Latitude, size = `abundance`, color = Type), alpha = 0.7) +
  scale_color_manual(values = c("HOL" = "red")) +
  scale_size_continuous(range = c(1, 3), labels = function(x) {
    sapply(x, function(x) parse(text = paste0(x / 1e03, " %*% 10^3")))
  }, breaks = c(2e03, 4e03, 6e03)) +  
   theme_minimal() +
   coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60°S", "30°S", "0°", "30°N", "60°N")) +
  labs(#title = expression(italic("Syracosphaera arethusae") * " abundance"),
    x = "Longitude", y = "Latitude", 
    size = expression(atop("Abundance", "(cells L" ^ {-1} * ")")),
    color = "Type", 
    subtitle="b)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   # Title text size
    plot.subtitle = element_text(size = 25, face = "bold"),  # Change subtitle size here
    axis.title.x = element_text(size = 25),                # X-axis title size
    axis.title.y = element_text(size = 25),                # Y-axis title size
    axis.text.x = element_text(size = 20),                 # X-axis text size
    axis.text.y = element_text(size = 20),                 # Y-axis text size
    legend.title = element_text(size = 25),                # Legend title size
    legend.text = element_text(size = 25)                  # Legend text size
  )+
  guides(color = guide_legend(override.aes = list(size = 4.5)))

santhos_het_plot/santhos_hol_plot