vec3 <- seq(from=3, to=27, by=3)
print(vec3)

v_257 <- vec3[c(2,5,7)]
print(v_257)

v_predpos <- vec3[length(vec3)-1]
print(v_predpos)

v_without_predpos <- vec3[-(length(vec3)-1)]
print(v_without_predpos)

v_6 <- vec3[-6]
print(v_6)

v_100 <- vec3[100]
print(v_100)

v_n1_nlast <- vec3[-c(1,length(vec3))]
print(v_n1_nlast)

v_m4_l10 <- vec3[vec3>4 & vec3<10]
print(v_m4_l10)

v_l4_m10 <- vec3[vec3<4 | vec3>10]
print(v_l4_m10)