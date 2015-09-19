library(ggplot2)          # For plotting purposes
set.seed(500); m = 250000 # Lowered due to processing time
p_obtuse = numeric(16)    # Initialize vector to store probability for each ratio
maxmatrix = data.frame(matrix(, nrow = 16 * m, ncol = 2))
maxmatrix[,1] = rep((1:16), each = m)
colnames(maxmatrix) = c("ratio", "sample")

for(i in 1:16){ # 'i' will represent the ratio used
  xcoord = runif(3*m, max = i) # Increase based on size
  ycoord = runif(3*m, max = 16)
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
  maxmatrix[(1:m)+(m * (i - 1)),2] = maxangle
  p_obtuse[i] = mean(maxangle > 90)
}

error = qnorm(1 - 0.05/2)*sqrt((p_obtuse)*(1-p_obtuse)/(m)); error
ratios = 1:16

plot1 = ggplot() +
  geom_vline(xintercept = 90, colour = "black", size = 1) +
  geom_density(data = maxmatrix, aes(x=sample, color = ratio, group = ratio), size = 1) +
  scale_color_gradient(breaks = c(4, 8, 12, 16), label = paste(c(4, 8, 12, 16), "/ 16"), low = "red", high = "grey20") +
  scale_x_continuous(breaks = seq(60,180,by=10), "maxangle") +
  scale_y_continuous(breaks = seq(0,1,by=0.005))

plot2 = ggplot() + 
  geom_path(aes(x = ratios, y = p_obtuse, color = ratios), size = 1) +
  geom_errorbar(aes(ymin = p_obtuse - error, ymax = p_obtuse + error, x = ratios, color = ratios), width = 0.5) +
  scale_color_gradient(breaks = c(4, 8, 12, 16), label = paste(c(4, 8, 12, 16), "/ 16"), low = "red", high = "grey20") +
  scale_x_continuous(breaks = 1:16, "Ratio (n/16)") +
  scale_y_continuous(breaks = seq(0,1,by=0.05), "Obtuse Probability") +
  coord_cartesian(xlim = c(-3, 16.5), ylim = c(0.635, 1)) +
  guides(color=FALSE) +
  geom_text(aes(x = ratios, y = p_obtuse), label = paste(1:16, "/16:", round(p_obtuse, 4), "\u00b1", round(error, 4)), size = 4.5, angle = 45, hjust = 1.1)

cbind(ratios, p_obtuse, error)

