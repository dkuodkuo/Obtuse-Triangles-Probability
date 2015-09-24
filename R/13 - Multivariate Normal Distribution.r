library(ggplot2)
set.seed(500); m = 250000
p_obtuse = numeric(16)
maxmatrix = data.frame(matrix(, nrow = 16 * m, ncol = 2))
maxmatrix[,1] = rep((1:16), each = m)
colnames(maxmatrix) = c("dimensions", "sample")
dimensions = 2:17

for (i in 1:16){
  point1 = matrix(rnorm(m*dimensions[i]), ncol = dimensions[i])
  point2 = matrix(rnorm(m*dimensions[i]), ncol = dimensions[i])
  point3 = matrix(rnorm(m*dimensions[i]), ncol = dimensions[i])
  
  side1sq = rowSums((point1 - point2)^2)
  side2sq = rowSums((point1 - point3)^2)
  side3sq = rowSums((point2 - point3)^2)
  
  sidessq = matrix(c(side1sq, side2sq, side3sq), nrow = 3, byrow=T)
  sidessq_sort = apply(sidessq, 2, sort)
  
  max = sidessq_sort[3,]; med = sidessq_sort[2,]; min = sidessq_sort[1,]
  
  maxangle = (180/pi) * acos((min + med - max) / (2 * sqrt(min) * sqrt(med)))
  maxmatrix[(1:m)+(m * (i - 1)),2] = maxangle
  p_obtuse[i] = mean(maxangle > 90)
}

error = qnorm(1 - 0.05/2)*sqrt((p_obtuse)*(1-p_obtuse)/(m)); error

plot1 = ggplot() +
  geom_vline(xintercept = 90, colour = "black", size = 1) +
  geom_density(data = maxmatrix, aes(x=sample, color = dimensions, group = dimensions), size = 1) +
  scale_color_gradient(breaks = c(4, 8, 12, 16), label = paste(c(4, 8, 12, 16)), low = "red", high = "grey20") +
  scale_x_continuous(breaks = seq(60,180,by=10), "maxangle") +
  scale_y_continuous(breaks = seq(0,1,by=0.005))

plot2 = ggplot() + 
  geom_path(aes(x = dimensions, y = p_obtuse, color = dimensions), size = 1) +
  geom_errorbar(aes(ymin = p_obtuse - error, ymax = p_obtuse + error, x = dimensions, color = dimensions), width = 0.5) +
  scale_color_gradient(breaks = c(4, 8, 12, 16), label = paste(c(4, 8, 12, 16), "/ 16"), low = "red", high = "grey20") +
  scale_x_continuous(breaks = 2:17, "Dimension") +
  scale_y_continuous(breaks = seq(0,1,by=0.05), "Obtuse Probability") +
  coord_cartesian(xlim = c(-2, 17.5), ylim = c(-0.15, 0.8)) +
  guides(color=FALSE) +
  geom_text(aes(x = dimensions, y = p_obtuse), label = paste(2:17, ":", round(p_obtuse, 4), "\u00b1", round(error, 4)), size = 4.5, angle = 45, hjust = 1.1)

cbind(dimensions, p_obtuse, error)
