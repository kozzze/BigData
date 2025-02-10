vec1 <- 1 / ((1:50) * (2:51))
sum_v1 <- sum(vec1)
print(sum_v1)

vec2 <- 1 / 2 ^ (0:20)
sum_v2 <- sum(vec2)
print(sum_v2)

n <- 0:20
vec3 <- (1 + 3 * n) / 3^n
sum_v3 <- sum(vec3)
more_05 <- sum(vec3 > 0.5)
print(sum_v3)
print(more_05)