library("rvest")
library("jsonlite")

linie <- "10,17,33"
token2 <- "35dbb2ebd27b23cf....."

res <- GET(url = paste0("https://vavel.mini.pw.edu.pl/api/vehicles/v1/short/?line=", linie),
           add_headers(Authorization = paste("Token", token2)))
jsonlite::fromJSON(as.character(res))
# brigade line delay        status                 time      lon      lat
# 1      025   10     0       STOPPED 2017-09-25T23:47:39Z 21.00311 52.18877
# 2        8   17    90        MOVING 2017-09-26T00:04:51Z 20.99225 52.24342
# 3       17   17     0       STOPPED 2017-09-25T21:43:11Z 21.00261 52.18883
# 4        9   10     0        MOVING 2017-09-25T21:19:04Z 21.00261 52.18885
# 5       11   10     0        MOVING 2017-09-25T21:22:37Z 20.96954 52.23154
# 6      036   17     0        MOVING 2017-09-25T20:52:11Z 20.93416 52.29925

res <- GET(url = paste0("https://vavel.mini.pw.edu.pl/api/vehicles/v1/full/?line=", linie),
           add_headers(Authorization = paste("Token", token2)))
jsonlite::fromJSON(as.character(res))


curl -i \
    -H 'Accept:application/json' \
    -H 'Authorization:Token 35dbb2ebd27b23cf.....' \
    "https://vavel.mini.pw.edu.pl/api/vehicles/v1/short/?line=10,17,33"

curl -i \
    -H 'Accept:application/json' \
    -H 'Authorization:Token 35dbb2ebd27b23cf.....' \
    "https://vavel.mini.pw.edu.pl/api/vehicles/v1/full/?line=10,17,33"

