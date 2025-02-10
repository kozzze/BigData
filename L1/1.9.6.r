resp_100 <- seq(1,100)
print(resp_100)

count_resp <- as.integer(readline())
if (count_resp <= 200){
  resp_lo_eq_200 <- seq(1,count_resp)
  print(resp_lo_eq_200)
}else{
  print("Ты что дурак? Не больше 200")
}