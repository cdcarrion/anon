Output from library

# Using `anonimizacion_tabla()` function

A minimal example should be like this:

```r
tabla_datos <- tibble::tibble(identificador = sample(letters,
                                                       size = 100,
                                                       replace = TRUE),
                                numerico = rnorm(100))
                                

## Test resultado:
## A tibble: 100 x 2
#identificador numerico
#<chr>            <dbl>
#  1 z               0.0440
#2 e              -0.531 
#3 y              -0.0287
#4 h              -0.390 
#5 h              -0.882 
#6 r              -1.60  
#7 l              -0.563 
#8 n              -0.950 
#9 c               1.14  
#10 j               0.815 
## ... with 90 more rows
## i Use `print(n = ...)` to see more rows

tabla_central <- tibble::tibble(id_real = sample(letters),
                                  id_anon = sample(LETTERS))
                                  
## A tibble: 26 x 2
#id_real id_anon
#<chr>   <chr>  
#  1 m       Q      
#2 r       Y      
#3 x       Z      
#4 u       X      
#5 v       C      
#6 f       P      
#7 a       T      
#8 p       B      
#9 c       O      
#10 y       V      
## ... with 16 more rows
## i Use `print(n = ...)` to see more rows

result <- anonimizacion_tabla(tabla_datos = tabla_datos ,
                                tabla_central = tabla_central ,
                                td_id = "identificador" ,
                                tc_id = list(id_real = "id_real",
                                             anon = "id_anon") )
## A tibble: 100 x 3
#identificador identificador_an numerico
#<chr>         <chr>               <dbl>
#  1 z             N                  0.0440
#2 e             S                 -0.531 
#3 y             V                 -0.0287
#4 h             A                 -0.390 
#5 h             A                 -0.882 
#6 r             Y                 -1.60  
#7 l             F                 -0.563 
#8 n             G                 -0.950 
#9 c             O                  1.14  
#10 j             K                  0.815 
## ... with 90 more rows
## i Use `print(n = ...)` to see more rows 
```
