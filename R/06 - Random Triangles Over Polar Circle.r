library(ggplot2) # For plotting purposes
set.seed(500)    # For reference purposes: repeatable code
m = 10           # Number of triangles to simulate

# Polar Sampling Method
polrx = numeric(3*m); polry = numeric(3*m) # Storage vectors for uniform

for (i in 1:(3*m)){
  r = runif(1); theta = runif(1,min=0,max=(2*pi))
  x = r*cos(theta); y = r*sin(theta); # Convert to (x,y)
  polrx[i] = x; polry[i] = y
}

x = polrx; y = polry

# Reorganize matrices for visualizing triangles

num = c(1:m, 1:m, 1:m)                    # Used to keep track of which triangle each point is associated with
triangles = data.frame(num, x, y)         # Associate all coordinates to their respective triangle

# Create unit circle overlay

xcirc = rep(seq(-1,1, 0.01), 2)
ycirc = sqrt(1 - xcirc^2)
ycirc[201:402] = -ycirc[201:402]

# Plot triangles

ggplot() +
  geom_polygon(aes(x = xcirc, y = ycirc), color = "grey20", size = 0.5) +
  geom_polygon(data = triangles, aes(x = x, y = y, color=factor(num)), fill = NA, size = 1) +
  geom_point(data = triangles, aes(x = x, y = y), size = 3, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), axis.title=element_blank())
