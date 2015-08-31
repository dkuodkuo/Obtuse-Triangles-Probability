set.seed(500) # For reference purposes: repeatable code
m = 1000000   # Number of triangles to simulate

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

# Obtain 95% CI estimate of obtuse probability with sidessq only
# Useful for reducing computation time: no sorting required
p_obtuse1 =(mean(side1sq > side2sq + side3sq) 
          + mean(side2sq > side1sq + side3sq) 
          + mean(side3sq > side1sq + side2sq)); p_obtuse1
error1 = qnorm(1 - 0.05/2)*sqrt((p_obtuse1)*(1-p_obtuse1)/(m)); error1
