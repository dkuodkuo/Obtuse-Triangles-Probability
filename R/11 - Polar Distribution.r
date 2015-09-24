library(ggplot2) # For plotting purposes
set.seed(500)    # For reference purposes: repeatable code
m = 1000000      # Number of triangles to simulate

# Polar Sampling Method
polrx = numeric(m); polry = numeric(m) # Storage vectors for uniform

for (i in 1:m){
  r = runif(1); theta = runif(1,min=0,max=(2*pi))
  x = r*cos(theta); y = r*sin(theta); # Convert to (x,y)
  xcoord[i] = x; ycoord[i] = y
}

point1 = cbind(xcoord[1:m],ycoord[1:m])
point2 = cbind(xcoord[(m+1):(2*m)],ycoord[(m+1):(2*m)])
point3 = cbind(xcoord[(2*m+1):(3*m)],ycoord[(2*m+1):(3*m)])

side1sq = rowSums((point1 - point2)^2)
side2sq = rowSums((point1 - point3)^2)
side3sq = rowSums((point2 - point3)^2)

sidessq = matrix(c(side1sq, side2sq, side3sq), ncol = m, byrow = T)
sidessq_sort = apply(sidessq, 2, sort)

max = sidessq_sort[3,]; med = sidessq_sort[2,]; min = sidessq_sort[1,]

maxangle = (180/pi) * acos((min + med - max) / (2 * sqrt(min) * sqrt(med)))

ggplot() +
  geom_vline(xintercept = 90, colour = "black", size = 1) +
  geom_histogram(aes(x = maxangle, y = ..density.., fill = ..density..), binwidth = 0.5, alpha = 0.7) +
  scale_fill_gradient("Density", low = "grey20", high = "red") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(60,180,by=10)) +
  scale_y_continuous(breaks = seq(0,0.015,by=0.001))

p_obtuse = mean(maxangle > 90); p_obtuse
error = qnorm(1 - 0.05/2)*sqrt((p_obtuse)*(1-p_obtuse)/(m)); error
