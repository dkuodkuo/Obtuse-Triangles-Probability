library(ggplot2) # For plotting purposes
set.seed(500)    # For reference purposes: repeatable code
m = 1000000      # Number of triangles to simulate

# Uniform Rejection Method: Use Rejection Generator from Appendix E
xcoord = ycoord = numeric(3*m)
for(i in 1:(3*m)){
  x = runif(1,min=-1,max=1); y = runif(1,min=-1,max=1)
  while((x^2 + y^2) > 1) {
    x = runif(1,min=-1,max=1); y = runif(1,min=-1,max=1)
  }
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
