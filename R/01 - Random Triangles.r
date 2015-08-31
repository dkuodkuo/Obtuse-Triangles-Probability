library(ggplot2) # For plotting purposes
set.seed(500)    # For reference purposes: repeatable code
m = 10           # Number of triangles to simulate

# Matrix format: each matrix represents one point of a triangle
# Each row is a single coordinate. Column 1 is X, 2 is Y

point1 = matrix(runif(2*m), ncol = 2)
point2 = matrix(runif(2*m), ncol = 2)
point3 = matrix(runif(2*m), ncol = 2)

# Reorganize matrices for visualizing triangles

num = c(1:m, 1:m, 1:m)                    # Used to keep track of which triangle each point is associated with
allpoints = rbind(point1, point2, point3) # Combine points into a matrix of X and Y coordinates
x = allpoints[,1]                         # Isolate X coordinates into its own matrix
y = allpoints[,2]                         # Isolate Y coordinates into its own matrix
triangles = data.frame(num, x, y)         # Associate all coordinates to their respective triangle

# Create unit box overlay

xbox = c(0, 1, 1, 0)
ybox = c(0, 0, 1, 1)

# Plot triangles

ggplot() +
  geom_polygon(aes(x = xbox, y = ybox)) +
  geom_polygon(data = triangles, aes(x = x, y = y, color=factor(num)), fill = NA, size = 1) +
  geom_point(data = triangles, aes(x = x, y = y), size = 3, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), axis.title=element_blank())
