datause <- read_csv("D:/GREmfzl/STAT641HW/result.csv")
data1 <- datause[,c('business_id','stars_x','GoodForMeal')]
data1 <- na.omit(data1)
data2 <- datause[,c('business_id','stars_x','Ambience')]
data2 <- na.omit(data2)
data3 <- datause[,c('business_id','stars_x','BusinessParking')]
data3 <- na.omit(data3)
data4 <- datause[,c('business_id','stars_x','Music')]
data4 <- na.omit(data4)
data5 <- datause[,c('business_id','stars_x','BestNights')]
data5 <- na.omit(data5)
write.csv(data1,"D:/GREmfzl/STAT641HW/data1.csv")
write.csv(data2,"D:/GREmfzl/STAT641HW/data2.csv")
write.csv(data3,"D:/GREmfzl/STAT641HW/data3.csv")
write.csv(data4,"D:/GREmfzl/STAT641HW/data4.csv")
write.csv(data5,"D:/GREmfzl/STAT641HW/data5.csv")
