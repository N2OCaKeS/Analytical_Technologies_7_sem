# Обработка данных в языке R 2
Филиппенко Максим Дмитриевич БИСО-02-20

## Цель работы:

------------------------------------------------------------------------

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Развить пркатические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

------------------------------------------------------------------------

## Задание:

Проанализировать встроенные в пакет nycflights13 наборы данных с помощью
языка R и ответить на вопро- сы: 1. Сколько встроенных в пакет
nycflights13 датафреймов?

2\. Сколько строк в каждом датафрейме?

3\. Сколько столбцов в каждом датафрейме?

4\. Как просмотреть примерный вид датафрейма?

5\. Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных
(представлено в наборах дан- ных)?

6\. Сколько рейсов принял аэропорт John F Kennedy Intl в мае?

7\. Какой самый северный аэропорт?

8\. Какой аэропорт самый высокогорный (находится выше всех над уровнем
моря)?

9\. Какие бортовые номера у самых старых самолетов?

10\.
КакаясредняятемпературавоздухабылавсентябреваэропортуJohnFKennedyIntl(вградусахЦельсия).

11\. Самолеты какой авиакомпании совершили больше всего вылетов в июне?

12\. Самолеты какой авиакомпании задерживались чаще других в 2013 году?

13\. Оформить отчет в соответствии с шаблоном

## Ход работы:

1.  Установка и подключение пакета “nycflights13”, подключение пакета
    “dplyr”

``` r
library(nycflights13)
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

1.  Ответить на 1 вопрос

``` r
num_dataframes <- length(data(package = "nycflights13")[['results']][, 'Item'])
print(paste("1. Всего датафреймов в пакете nycflights13:", num_dataframes))
```

    [1] "1. Всего датафреймов в пакете nycflights13: 5"

1.  Ответить на 2 вопрос

``` r
for (df_name in data(package = "nycflights13")[['results']][, 'Item']) {
  df <- get(df_name)
  print(paste("2. Строк в датафрейме", df_name, ":", nrow(df)))
}
```

    [1] "2. Строк в датафрейме airlines : 16"
    [1] "2. Строк в датафрейме airports : 1458"
    [1] "2. Строк в датафрейме flights : 336776"
    [1] "2. Строк в датафрейме planes : 3322"
    [1] "2. Строк в датафрейме weather : 26115"

1.  Ответить на 3 вопрос

``` r
for (df_name in data(package = "nycflights13")[['results']][, 'Item']) {
  df <- get(df_name)
  print(paste("3. Столбцов в датафрейме", df_name, ":", ncol(df)))
}
```

    [1] "3. Столбцов в датафрейме airlines : 2"
    [1] "3. Столбцов в датафрейме airports : 8"
    [1] "3. Столбцов в датафрейме flights : 19"
    [1] "3. Столбцов в датафрейме planes : 9"
    [1] "3. Столбцов в датафрейме weather : 15"

1.  Ответить на 4 вопрос

``` r
head(flights)
```

    # A tibble: 6 × 19
       year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
      <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    1  2013     1     1      517            515         2      830            819
    2  2013     1     1      533            529         4      850            830
    3  2013     1     1      542            540         2      923            850
    4  2013     1     1      544            545        -1     1004           1022
    5  2013     1     1      554            600        -6      812            837
    6  2013     1     1      554            558        -4      740            728
    # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    #   hour <dbl>, minute <dbl>, time_hour <dttm>

1.  Ответить на 5 вопрос

``` r
unique_carriers <- unique(flights$carrier)
print(paste("5. Количество уникальных перевозчиков:", length(unique_carriers)))
```

    [1] "5. Количество уникальных перевозчиков: 16"

1.  Ответить на 6 вопрос

``` r
flights_jfk_may <- subset(flights, month == 5 & dest == "JFK")
print(paste("6. Рейсов аэропорта John F Kennedy Intl в мае:", nrow(flights_jfk_may)))
```

    [1] "6. Рейсов аэропорта John F Kennedy Intl в мае: 0"

1.  Ответить на 7 вопрос

``` r
northern_airport <- airports[which.max(airports$lat), "name"]
print(paste("7. Самый северный аэропорт:", northern_airport))
```

    [1] "7. Самый северный аэропорт: Dillant Hopkins Airport"

1.  Ответить на 8 вопрос

``` r
highest_airport <- airports[which.max(airports$alt), "name"]
print(paste("8. Самый высокогорный аэропорт:", highest_airport))
```

    [1] "8. Самый высокогорный аэропорт: Telluride"

1.  Ответить на 9 вопрос

``` r
oldest_planes <- tail(order(flights$year), 5)
tail(flights[oldest_planes, "tailnum"])
```

    # A tibble: 5 × 1
      tailnum
      <chr>  
    1 <NA>   
    2 <NA>   
    3 N535MQ 
    4 N511MQ 
    5 N839MQ 

1.  Ответить на 10 вопрос

``` r
mean_temp_jfk_september <- mean((weather$temp[weather$origin == "JFK" & weather$month == 9] - 32) * 5/9)
print(paste("10. Средняя температура в сентябре в JFK:", mean_temp_jfk_september, "градусов Цельсия"))
```

    [1] "10. Средняя температура в сентябре в JFK: 19.3876388888889 градусов Цельсия"

1.  Ответить на 11 вопрос

``` r
most_flights_carrier_june <- names(sort(table(flights$carrier[flights$month == 6]), decreasing = TRUE)[1])
print(paste("11. Авиакомпания с наибольшим количеством вылетов в июне:", most_flights_carrier_june))
```

    [1] "11. Авиакомпания с наибольшим количеством вылетов в июне: UA"

1.  Ответить на 12 вопрос

``` r
most_delayed_carrier_2013 <- names(sort(table(flights$carrier[flights$year == 2013 & flights$dep_delay > 0]), decreasing = TRUE)[1])
print(paste("12. Авиакомпания с наибольшей задержкой в 2013 году:", most_delayed_carrier_2013))
```

    [1] "12. Авиакомпания с наибольшей задержкой в 2013 году: UA"

## Оценка результатов:

В результате выполнения лабораторной работы были улучшены базовые навыки
языка R, а так же улучшены навыки по работе с пакетом dplyr
