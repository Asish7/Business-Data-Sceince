roll<- function(dice,num_of_dice=2){
  dices<-sample(x=dice,size = num_of_dice,replace = TRUE)
  sum(dices)
}

roll(1:6,3)



EOQ<-function(k,h,d=1000){
  value<-sqrt(2*d*k/h)
  value
}

EOQ(5,0.25,1000)
EOQ(5,0.25,4000)

hist(mtcars$mpg)
