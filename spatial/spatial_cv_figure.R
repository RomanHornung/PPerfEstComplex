# DONE
#######

# Set working directory:

setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")



# Fig. 3: Visualization of different spatial CV approaches.
#############################################################


# Load the shape file of Bavaria and generate random points 
# on the surface of Bavaria:


library("ggplot2")
library("sf")
library("dplyr")

# Read the shapefile
germany <- st_read("./spatial/data/DEU_adm2.shp")

# Filter the state of Bavaria and unify it as a whole
bavaria <- germany %>%
  filter(NAME_1 == "Bayern") %>%
  st_union()

# Generate a random set of points within the Bavaria polygon
set.seed(42) # for reproducibility
n_points <- 2000 # number of points to generate
grid_points <- st_sample(bavaria, size = n_points, type = "random", crs = st_crs(bavaria))





# Select color scheme and some general plot parameters for the three subplots:


library("RColorBrewer")

display.brewer.all(n=3, type="div")

colors <- brewer.pal(3, "RdBu")
selectedColors <- c(colors[1], colors[3])
print(selectedColors)

pointsize <- 0.7
titlesize <- 18





# Rectangular tiles:


# Convert to a data frame for ggplot2
grid_points_df <- data.frame(st_coordinates(grid_points))

# Define equidistant points for vertical and horizontal lines
num_lines <- 6 # this will create 5 squares
x_coords <- seq(st_bbox(bavaria)[1], st_bbox(bavaria)[3], length.out = num_lines)
y_coords <- seq(st_bbox(bavaria)[2], st_bbox(bavaria)[4], length.out = num_lines)

# Initialize empty list to hold lines
grid_lines <- list()

# Create vertical and horizontal lines and intersect them with Bavaria
for (i in 1:(num_lines-1)) {
  vertical_line <- st_sfc(st_linestring(rbind(c(x_coords[i], min(y_coords)), 
                                              c(x_coords[i], max(y_coords)))), 
                          crs = st_crs(bavaria))
  horizontal_line <- st_sfc(st_linestring(rbind(c(min(x_coords), y_coords[i]), 
                                                c(max(x_coords), y_coords[i]))), 
                            crs = st_crs(bavaria))
  
  intersected_vline <- data.frame(st_coordinates(st_intersection(vertical_line, st_union(bavaria))))
  intersected_hline <- data.frame(st_coordinates(st_intersection(horizontal_line, st_union(bavaria))))
  
  # Add to list
  grid_lines[[paste0("vline", i)]] <- intersected_vline
  grid_lines[[paste0("hline", i)]] <- intersected_hline
}

# Determine square for each point
grid_points_df$square <- findInterval(grid_points_df$X, x_coords) +
  (findInterval(grid_points_df$Y, y_coords) - 1) * (num_lines - 1)

# Color all points in the second square from the top and from the left with one color,
# and all other points with another color
grid_points_df$color <- ifelse(grid_points_df$square == 18, scales::hue_pal()(2)[1], scales::hue_pal()(2)[2])

# Add the grid lines and colored points to the plot
p1 <- ggplot() +
  geom_point(data = grid_points_df, aes(X, Y, color = color), size=pointsize) +
  geom_sf(data = bavaria, fill = NA, color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[1]], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[2]], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[3]][1:2,], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[3]][3:8,], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[4]], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[5]], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[6]], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[7]], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[8]], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[9]][1:2,], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[9]][3:4,], aes(X, Y), color = "black", linewidth = 1) +
  geom_line(data = grid_lines[[10]], aes(X, Y), color = "black", linewidth = 1) +
  scale_color_manual(values=selectedColors[2:1]) +
  theme_bw() +
  theme(legend.position = "none", axis.text=element_blank(),
        axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        plot.title = element_text(size = titlesize)) +
  coord_sf() +
  ggtitle("Rectangular tiles")

p1






# Leave−one−out CV with buffer:


library("ggforce")  # for geom_ellipse

# Convert to a data frame for ggplot2
grid_points_df <- data.frame(st_coordinates(grid_points))
colnames(grid_points_df) <- c("X", "Y")

# Center of Munich
test_point <- data.frame(X = 11.5820, Y = 48.1351)

# Define the semi-major and semi-minor axes of the ellipse
semi_major_axis <- 0.35*0.2/0.13  # adjust this value to change the width of the ellipse
semi_minor_axis <- 0.35    # adjust this value to change the height of the ellipse

# Calculate the distance of each point to the test point
dist_to_test_point <- sqrt((grid_points_df$X - test_point$X)^2 / semi_major_axis^2 + 
                             (grid_points_df$Y - test_point$Y)^2 / semi_minor_axis^2)

# Filter points outside the buffer
points_outside_buffer <- grid_points_df[dist_to_test_point > 1, ]

# Create plot
p2 <- ggplot() +
  geom_point(data = points_outside_buffer, aes(X, Y), color = selectedColors[2], size=pointsize) +
  geom_point(data = test_point, aes(X, Y), color = selectedColors[1], size=pointsize) +
  geom_sf(data = bavaria, fill = NA, color = "black", linewidth = 1) +
  ggforce::geom_ellipse(aes(x0 = test_point$X, y0 = test_point$Y, a = semi_major_axis, b = semi_minor_axis, angle = 0), 
                        fill = NA, color = "black", linetype = "solid", linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none", axis.text=element_blank(),
        axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        plot.title = element_text(size = titlesize)) +
  coord_sf() +
  ggtitle("Leave-one-out CV with buffer")

p2





# Partitioning based on geographical units:


# Filter the districts in Bavaria
bavaria_districts <- germany %>%
  filter(NAME_1 == "Bayern") # 'NAME_1' is assumed to be the column containing the state names. This might change according to the structure of your data.

# Find out which points are within Oberbayern
oberbayern <- bavaria_districts %>% filter(NAME_2 == "Oberbayern")

# Convert to a data frame for ggplot2
grid_points_df <- data.frame(st_coordinates(grid_points))

# Define which district each point belongs to
grid_points_df$in_oberbayern <- st_within(grid_points, oberbayern, sparse = FALSE)

# Create the plot
p3 <- ggplot() +
  geom_point(data = grid_points_df, aes(X, Y, color = in_oberbayern), size=pointsize) +
  geom_sf(data = bavaria_districts, fill = NA, color = "black", linewidth = 1) +
  scale_color_manual(values = c("TRUE" = selectedColors[1], "FALSE" = selectedColors[2])) +
  theme_bw() +
  labs(color = "In Oberbayern") +
  theme(legend.position = "none", axis.text=element_blank(),
        axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        plot.title = element_text(size = titlesize)) +
  coord_sf() + ggtitle("Partitioning based on geographical units")

p3






# Combine the plots and save the combination:


library("gridExtra")
p <- grid.arrange(p1, p2, p3, ncol=3)
p

ggsave("./spatial/results/figures/Figure3.pdf", plot=p, width=10*1.5, height=3.5*1.5)
