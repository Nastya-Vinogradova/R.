# проверяем рабочую директорию
setwd("C:/R); getwd()
# устанавливаем пакеты 
#install.packages("tidyverse")
#install.packages("rnoaa")

# открываем нужные нам пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

# устанавливаем список метеостанций
station_data = ghcnd_stations()
write.csv(station_data,file = "station_data.csv")
station_data=read.csv("station_data.csv")

#После получения списка всех станций, получаем список станций ближайших 
# к столице нашего региона,
#создем таблицу с именем региона и координатами его столицы
Blagoveshchensk = data.frame(id="Blagoveshchensk",latitude=50.2796, longitude=127.54)
Blagoveshchensk_around = meteo_nearby_stations(lat_lon_df = Blagoveshchensk, 
                                   station_data = station_data,
                                   limit=20,
                                   var=c("TAVG"),
                                   year_min = 2006, year_max = 2011)
                                   
  #Blagoveshchensk_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от столицы, первым элементом таблицы будет идентификатор метеостанции Благовещенска, получим его

Blagoveshchensk_id = Blagoveshchensk_around [["Blagoveshchensk"]][["id"]][1]
summary(Blagoveshchensk_id)         
Blagoveshchensk_id

# для получения таблицы со всеми метеостанциями вокруг Благовещенска
# необходимо выбрать целиком первый объект из списка
Blagoveshchensk_table=Blagoveshchensk_around[[1]]
summary(Blagoveshchensk_table)

# отфильтурем метеостанции в радиусе не более 300 км с помщью комнды фильтр
Blagoveshchensk_stations=Blagoveshchensk_table %>%filter(distance>=200&distance<=300)

# сформируем список неоходимых станций
Blagoveshchensk_stations=Blagoveshchensk_table
str(Blagoveshchensk_stations)

# список содержит 20 метеостанций расположенных вблизи Благовещенска выведем индетификаторы отфильрованных метеостанций 
Blagoveshchensk_stations$id

# скачаем погодые данных для наших метеостанций
# чтобы получить все данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_Blagoveshchensk_data=meteo_tidy_ghcnd(stationid = Blagoveshchensk_id)
summary(all_Blagoveshchensk_data)

# создадим цикл, в котором бы скачивались  нужные данные для всех метеостанций 
# cоздадим объект, куда скачаем все данные всех метеостанций
all_Blagoveshchensk_meteodata = data.frame()

 #создадим цикл для наших 20 метеостанций
stations_names=Blagoveshchensk_stations$id
stations_names=stations_names[1:20]

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2006-01-01",
                              date_max = "2011-12-31")
 
 station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_Blagoveshchensk_meteodata=rbind(all_Blagoveshchensk_meteodata, one_meteo)}

# записываем полученные результаты 
write.csv(all_Blagoveshchensk_meteodata,"all_Blagoveshchensk_meteodata.csv")
# считываем данные 
all_Blagoveshchensk_meteodata=read.csv("all_Blagoveshchensk_meteodata.csv")

str(all_Blagoveshchensk_meteodata)

# добавляем год, месяц, день
all_Blagoveshchensk_meteodata=all_Blagoveshchensk_meteodata %>% mutate(year=year(date), 
                                               month=month(date), 
                                               day=day(date))
# превращаем NA в 0 и где tavg<5
all_Blagoveshchensk_meteodata[is.na(all_Blagoveshchensk_meteodata$tavg),"tavg"] = 0
all_Blagoveshchensk_meteodata[all_Blagoveshchensk_meteodata$tavg<5, "tavg"] = 0
summary(all_Blagoveshchensk_meteodata)

# сгруппируем метеостанции по id, месяцам и проссумируем темперетатуру
# по этим группам, далее сгурппируем данные по месяцам и найдем среднее по месяцам для всех метеостанций
group_meteodata =all_Blagoveshchensk_meteodata %>% group_by(id,year,month) 
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

# Подготовка к расчету по формуле Урожая ##
# Ввдим константы
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000) 
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000) 
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) 
y = 1.0 
Kf = 300
Qj = 1600
Lj = 2.2 
Ej = 25 

# Рассчитываем Fi по месяцаv
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  Расчитаем урожай 
Yield = (sum(sumT_month$Yi)) 
Yield
# результат 35,5 ц/га
 
                             