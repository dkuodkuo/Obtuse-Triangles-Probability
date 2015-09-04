library(ggplot2) # For plotting purposes
set.seed(500)    # For reference purposes: repeatable code
m = 10           # Number of triangles to simulate

point1 = matrix(rnorm(2*m), ncol = 2)
point2 = matrix(rnorm(2*m), ncol = 2)
point3 = matrix(rnorm(2*m), ncol = 2)

num = c(1:m, 1:m, 1:m)                    # Used to keep track of which triangle each point is associated with
allpoints = rbind(point1, point2, point3) # Combine points into a matrix of X and Y coordinates
x = allpoints[,1]                         # Isolate X coordinates into its own matrix
y = allpoints[,2]                         # Isolate Y coordinates into its own matrix
triangles = data.frame(num, x, y)         # Associate all coordinates to their respective triangle

# Create bivariate scale overlay

bivar = 200
xbox = seq(-3,3,length=bivar)
ybox = xbox

f = function(x1, x2)
{
  (1/(2*pi))*exp(-0.5*((x1)^2+(x2)^2))
}

bivardens = c(outer(xbox, ybox, f))
xbox = rep(xbox, bivar)
ybox = rep(ybox, each = bivar)

# Plot triangles

ggplot() +
  geom_tile(aes(x = xbox, y = ybox, fill = bivardens)) + 
  scale_fill_gradient(low="grey20", high="red", "density", limits=c(0, 0.16)) +
  geom_polygon(data = triangles, aes(x = x, y = y, color=factor(num)), fill = NA, size = 1) +
  geom_point(data = triangles, aes(x = x, y = y), size = 3, colour = "white", shape = 19) +
  coord_fixed() +
  guides(color=FALSE) +
  theme(axis.text = element_text(size = 12), axis.title=element_blank())
