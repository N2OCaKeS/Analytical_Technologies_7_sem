# Обработка данных в языке R
Филиппенко Максим Дмитриевич БИСО-02-20

## Практическая работа 1-2 Основы обработки данных с помощью R

Филиппенко Максим БИСО-02-20

## Цель работы:

------------------------------------------------------------------------

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить пркатические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

------------------------------------------------------------------------

## Задание:

Проанализировать встроенный в пакет dplyr набор данных starwars с
помощью языка R и ответить на вопро- сы:

1\. Сколько строк в датафрейме?

2\. Сколько столбцов в датафрейме?

3\. Как просмотреть примерный вид датафрейма? 4. Сколько уникальных рас
персонажей (species) представлено в данных?

5\. Найти самого высокого персонажа.

6\. Найти всех персонажей ниже 170

7\. Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ
подсчитать по формуле 𝐼 = 𝑚 , где 𝑚 – масса (weight), а h – рост
(height).

8\. Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
отношению массы (mass) к росту (height) персонажей.

9\. Найти средний возраст персонажей каждой расы вселенной Звездных
войн.

10\. Найти самый распространенный цвет глаз персонажей вселенной
Звездных войн.

11\. Подсчитать среднюю длину имени в каждой расе вселенной Звездных
войн.

12\. Оформить отчет в соответствии с шаблоном —

## Ход работы:

1.  Загрузить пакет “dplyr”

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

1.  Ответить на 1 вопрос

``` r
starwars %>% nrow()
```

    [1] 87

1.  Ответить на 2 вопрос

``` r
starwars %>% ncol()
```

    [1] 14

1.  Ответить на 3 вопрос

``` r
starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

1.  Ответить на 4 вопрос

``` r
starwars %>% distinct(species) %>% nrow()
```

    [1] 38

1.  Ответить на 5 вопрос

``` r
starwars %>% filter(height == max(height, na.rm = TRUE))
```

    # A tibble: 1 × 14
      name      height  mass hair_color skin_color eye_color birth_year sex   gender
      <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
    1 Yarael P…    264    NA none       white      yellow            NA male  mascu…
    # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
    #   vehicles <list>, starships <list>

1.  Ответить на 6 вопрос

``` r
starwars %>% filter(height < 170)
```

    # A tibble: 22 × 14
       name     height  mass hair_color skin_color eye_color birth_year sex   gender
       <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
     1 C-3PO       167    75 <NA>       gold       yellow           112 none  mascu…
     2 R2-D2        96    32 <NA>       white, bl… red               33 none  mascu…
     3 Leia Or…    150    49 brown      light      brown             19 fema… femin…
     4 Beru Wh…    165    75 brown      light      blue              47 fema… femin…
     5 R5-D4        97    32 <NA>       white, red red               NA none  mascu…
     6 Yoda         66    17 white      green      brown            896 male  mascu…
     7 Mon Mot…    150    NA auburn     fair       blue              48 fema… femin…
     8 Wicket …     88    20 brown      brown      brown              8 male  mascu…
     9 Nien Nu…    160    68 none       grey       black             NA male  mascu…
    10 Watto       137    NA black      blue, grey yellow            NA male  mascu…
    # ℹ 12 more rows
    # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
    #   vehicles <list>, starships <list>

1.  Ответить на 7 вопрос

``` r
starwars %>% mutate(BMI = mass / ((height / 100) ^ 2))
```

    # A tibble: 87 × 15
       name     height  mass hair_color skin_color eye_color birth_year sex   gender
       <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
     1 Luke Sk…    172    77 blond      fair       blue            19   male  mascu…
     2 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu…
     3 R2-D2        96    32 <NA>       white, bl… red             33   none  mascu…
     4 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
     5 Leia Or…    150    49 brown      light      brown           19   fema… femin…
     6 Owen La…    178   120 brown, gr… light      blue            52   male  mascu…
     7 Beru Wh…    165    75 brown      light      blue            47   fema… femin…
     8 R5-D4        97    32 <NA>       white, red red             NA   none  mascu…
     9 Biggs D…    183    84 black      light      brown           24   male  mascu…
    10 Obi-Wan…    182    77 auburn, w… fair       blue-gray       57   male  mascu…
    # ℹ 77 more rows
    # ℹ 6 more variables: homeworld <chr>, species <chr>, films <list>,
    #   vehicles <list>, starships <list>, BMI <dbl>

1.  Ответить на 8 вопрос

``` r
starwars %>% mutate(stretchiness = mass / height) %>% top_n(10, stretchiness)
```

    # A tibble: 10 × 15
       name     height  mass hair_color skin_color eye_color birth_year sex   gender
       <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
     1 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
     2 Owen La…    178   120 brown, gr… light      blue            52   male  mascu…
     3 Chewbac…    228   112 brown      unknown    blue           200   male  mascu…
     4 Jabba D…    175  1358 <NA>       green-tan… orange         600   herm… mascu…
     5 Jek Ton…    180   110 brown      fair       blue            NA   <NA>  <NA>  
     6 IG-88       200   140 none       metal      red             15   none  mascu…
     7 Bossk       190   113 none       green      red             53   male  mascu…
     8 Dexter …    198   102 none       brown      yellow          NA   male  mascu…
     9 Grievous    216   159 none       brown, wh… green, y…       NA   male  mascu…
    10 Tarfful     234   136 brown      brown      blue            NA   male  mascu…
    # ℹ 6 more variables: homeworld <chr>, species <chr>, films <list>,
    #   vehicles <list>, starships <list>, stretchiness <dbl>

1.  Ответить на 9 вопрос

``` r
starwars %>% group_by(species) %>% summarise(avg_age = mean(birth_year, na.rm = TRUE))
```

    # A tibble: 38 × 2
       species   avg_age
       <chr>       <dbl>
     1 Aleena      NaN  
     2 Besalisk    NaN  
     3 Cerean       92  
     4 Chagrian    NaN  
     5 Clawdite    NaN  
     6 Droid        53.3
     7 Dug         NaN  
     8 Ewok          8  
     9 Geonosian   NaN  
    10 Gungan       52  
    # ℹ 28 more rows

1.  Ответить на 10 вопрос

``` r
starwars %>% count(eye_color) %>% top_n(1, n)
```

    # A tibble: 1 × 2
      eye_color     n
      <chr>     <int>
    1 brown        21

1.  Ответить на 11 вопрос

``` r
starwars %>% group_by(species) %>% summarise(avg_name_length = mean(nchar(name)))
```

    # A tibble: 38 × 2
       species   avg_name_length
       <chr>               <dbl>
     1 Aleena              12   
     2 Besalisk            15   
     3 Cerean              12   
     4 Chagrian            10   
     5 Clawdite            10   
     6 Droid                4.83
     7 Dug                  7   
     8 Ewok                21   
     9 Geonosian           17   
    10 Gungan              11.7 
    # ℹ 28 more rows

## Оценка результатов:

В результате выполнения лабораторной работы были освоены базовые навыки
языка R, а так же получены навыки по работе с пакетом dplyr

## Вывод

Были получены базовые навыки обработки данных с помощью языка R
