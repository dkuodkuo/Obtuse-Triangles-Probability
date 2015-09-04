library(ggplot2)   # For plotting purposes
library(grid)      # For plotting purposes
library(gridExtra) # For plotting purposes
set.seed(500)      # For reference purposes: repeatable code
m = 10             # Number of triangles to simulate

x = runif(3*m, max = 1)         # Initialize x coordinates
for(i in 2:16)                  # 'i' will represent the ratio used
{
  x = c(x, runif(3*m, max = i)) # Progressively closer to 16
}

y = runif(3*16*m, max = 16)     # All 16, to increment ratio by 1/16

num = rep(c(1:m),16*3)          # Keep track of point-triangle association
ratio = rep(c(1:16), each = 30) # Keep track of rectangle ratio

triangles = data.frame(num, ratio, x, y)

# Create unit box overlay

xbox = rep(c(0, 1, 1, 0), 16)
ybox = rep(c(0, 0, 16, 16), 16)

for (i in 2:16)
{
  xbox[(1:4)+4*(i-1)] = i * xbox[(1:4)+4*(i-1)]
}
ratio2 = rep(c(1:16), each = 4)
box = data.frame(ratio2, xbox, ybox)

# Plot triangles


p1 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 1), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 1), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 1), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p2 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 2), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 2), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 2), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p3 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 3), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 3), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 3), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p4 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 4), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 4), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 4), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p5 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 5), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 5), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 5), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p6 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 6), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 6), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 6), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p7 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 7), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 7), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 7), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p8 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 8), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 8), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 8), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p9 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 9), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 9), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 9), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p10 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 10), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 10), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 10), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p11 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 11), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 11), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 11), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p12 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 12), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 12), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 12), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p13 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 13), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 13), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 13), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p14 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 14), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 14), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 14), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p15 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 15), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 15), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 15), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

p16 = ggplot() +
  geom_polygon(data = subset(box, ratio2 == 16), aes(x = xbox, y = ybox)) +
  geom_polygon(data = subset(triangles, ratio == 16), aes(x = x, y = y, color=factor(num)), fill = NA) +
  geom_point(data = subset(triangles, ratio == 16), aes(x = x, y = y), size = 2, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank())

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, ncol = 4)
