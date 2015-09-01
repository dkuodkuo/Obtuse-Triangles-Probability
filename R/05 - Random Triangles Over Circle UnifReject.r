library(ggplot2) # For plotting purposes
set.seed(500)    # For reference purposes: repeatable code
m = 10           # Number of triangles to simulate

# Uniform + Rejection Sampling Method
unifx = numeric(3 * m); unify = numeric(3 * m) # Storage vectors for uniform

for (i in 1:(3 * m)){
  x = runif(1,min=-1,max=1); y = runif(1,min=-1,max=1); # Square uniform sample space
  while((x^2 + y^2) > 1) { # Resample until point falls inside circle
    x = runif(1,min=-1,max=1); y = runif(1,min=-1,max=1);
  }
  unifx[i] = x; unify[i] = y
}

# Reorganize matrices for visualizing triangles

num = c(1:m, 1:m, 1:m)
x = unifx
y = unify
triangles = data.frame(num, x, y)

# Create unit circle overlay

xcirc = rep(seq(-1,1, 0.01), 2)
ycirc = sqrt(1 - xcirc^2)
ycirc[201:402] = -ycirc[201:402]

# Plot triangles

ggplot() +
  geom_polygon(aes(x = xcirc, y = ycirc), size = 0.5) +
  geom_polygon(data = triangles, aes(x = x, y = y, color=factor(num)), fill = NA, size = 1) +
  geom_point(data = triangles, aes(x = x, y = y), size = 3, colour = "white", shape = 19) +
  coord_fixed() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), axis.title=element_blank())
