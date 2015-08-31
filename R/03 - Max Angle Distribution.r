library(ggplot2) # For plotting purposes
set.seed(500)    # For reference purposes: repeatable code
m = 1000000      # Number of triangles to simulate

# Matrix format: each matrix represents one point of a triangle
# Each row is a single coordinate. Column 1 is X, 2 is Y

point1 = matrix(runif(2*m), ncol = 2)
point2 = matrix(runif(2*m), ncol = 2)
point3 = matrix(runif(2*m), ncol = 2)

# Obtain squared distance length (based on distance formula)
# Distance formula is d^2 = (X1 - X2)^2+(Y1 - Y2)^2

side1sq = rowSums((point1 - point2)^2)
side2sq = rowSums((point1 - point3)^2)
side3sq = rowSums((point2 - point3)^2)

# Store side squared values in matrix
# Each column represents 1 triangle

sidessq = matrix(c(side1sq, side2sq, side3sq), ncol = m, byrow = T)

# Sort matrix columns individually: min to max
# (This step takes the longest time to compute!)

sidessq_sort = apply(sidessq, 2, sort)

# Store sides into vectors representing relative size per triangle

max = sidessq_sort[3,]; med = sidessq_sort[2,]; min = sidessq_sort[1,]

# Obtain & plot max angle distribution (based on law of cosines)

maxangle = (180/pi) * acos((min + med - max) / (2 * sqrt(min) * sqrt(med)))

ggplot() +
  geom_vline(xintercept = 90, colour = "black", size = 1) +
  geom_histogram(aes(x = maxangle, y = ..density.., fill = ..density..), binwidth = 0.5, alpha = 0.7) +
  scale_fill_gradient("Density", low = "black", high = "green") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(60,180,by=10)) +
  scale_y_continuous(breaks = seq(0,0.015,by=0.001))
