### Exploratory 

explot <-function () 
{
  ### Reading and subsetting the data
  echo = TRUE
  datos<-read.csv("activity.csv" , stringsAsFactors = TRUE , header = TRUE)
  Vcompl <- complete.cases(datos)
  datos_sin_na<- datos[Vcompl,]
  
  V_dayly_steps<-tapply(datos_sin_na$steps,datos_sin_na$date, sum  )
  summary (V_dayly_steps)

  
  ### Drawing the plot and saving it to a PNG file
  hist(V_dayly_steps , main = "Total number of steps taken per day", col= "blue" , xlab = "steps" )

  ## What is the average daily activity pattern?
  
  datos_sin_na$time<- as.character(datos_sin_na$interval)
  datos_sin_na[datos_sin_na$interval < 10, ]$time <- paste("00:0", datos_sin_na[datos_sin_na$interval < 10, ]$interval , sep="" )
  datos_sin_na[datos_sin_na$interval >= 10 & datos_sin_na$interval < 100, ]$time <- paste("00:", datos_sin_na[datos_sin_na$interval >= 10 & datos_sin_na$interval < 100, ]$interval , sep="" )
  datos_sin_na[datos_sin_na$interval >= 100 & datos_sin_na$interval < 1000, ]$time <- paste("0", substr(datos_sin_na[datos_sin_na$interval >= 100 & datos_sin_na$interval < 1000 , ]$interval,1,1),":", substr(datos_sin_na[datos_sin_na$interval >= 100 & datos_sin_na$interval < 1000, ]$interval,2,3), sep="") 
  datos_sin_na[datos_sin_na$interval >= 1000 , ]$time <- paste( substr(datos_sin_na[datos_sin_na$interval >= 1000 , ]$interval,1,2),":", substr(datos_sin_na[datos_sin_na$interval >= 1000 , ]$interval,3,4), sep="") 

  datos_intv <- aggregate(datos_sin_na$steps, by=list( datos_sin_na$time , datos_sin_na$interval), FUN= mean)
  datos_intv$time <-strptime(datos_intv$Group.1 , format = "%H:%M")
  plot(datos_intv$time,datos_intv$x , type="l", main="", ylab="steps", xlab="time (hour)" )
  
  datos_intv[max(datos_intv$x)==datos_intv$x,]$Group.1
  
  ## Imputing missing values
  
  sum(is.na(datos$steps))
  
  datos_imputed<-datos
  datos_imputed[is.na(datos_imputed$steps), "steps"] <- mean(na.omit(datos$steps))
  V_dayly_steps_imputed<-tapply(datos_imputed$steps,datos_imputed$date, sum  )

  
  par(mfrow=c(1,2))

  hist(V_dayly_steps , main = "Total number of steps taken per day", col= "blue" , xlab = "steps" , ylim =c(0,35))
  hist(V_dayly_steps_imputed , main = "Total number of steps taken per day (NA values Imputed", col= "gray" , xlab = "steps" )

  ## Are there differences in activity patterns between weekdays and weekends?
  
  datos_imp_new<-transform(datos_imputed, date = as.character(date))
  datos_imp_new$dayofweek <- weekdays(strptime(datos_imp_new$date, format="%Y-%m-%d"))
  datos_imp_new$dayofweek <- replace(datos_imp_new$dayofweek, datos_imp_new$dayofweek %in% c("Saturday","Sunday"), "Weekend" )
  datos_imp_new$dayofweek <- replace(datos_imp_new$dayofweek, !(datos_imp_new$dayofweek %in% c("Weekend")), "Weekday" )
  datos_imp_new$dayofweek <- as.factor(datos_imp_new$dayofweek)

  datos_int_imp <- aggregate(datos_imp_new$steps, by=list( datos_imp_new$interval, datos_imp_new$dayofweek), FUN= mean)
  names(datos_int_imp)<-c("Interval","Dayofweek","steps")
  library(ggplot2)
  p<-ggplot(datos_int_imp, aes(Interval, steps)) +
    geom_line() +
    coord_cartesian() +
    scale_color_gradient() +
    theme(strip.background = element_rect(fill = "cornsilk", color = "cornsilk", size = 2)) +
    ylab("Number of steps") +
    facet_grid( Dayofweek ~ .)
    
  print(p)
    
}
