# install.packages("pacman")
# library("pacman")

# # CREATE DATABASE - run only at the first time
# db_con = dbConnect(MySQL(), user='root', password = db_password, host='localhost')
# dbSendQuery(db_con, statement = "CREATE DATABASE quotes")
# dbSendQuery(db_con, statement = "USE quotes")
# dbSendQuery(db_con, statement = "CREATE TABLE quotes(id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
#             quotes VARCHAR(2000), nchar INT, title VARCHAR(2000), description VARCHAR(10000));")
# dbSendQuery(db_con, statement = "CREATE TABLE links(id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
#             link VARCHAR(2083), title VARCHAR(2000));")
# dbDisconnect(db_con)
# #\CREATE DATABASE

p_load("RMySQL")

source(file = "download_data.R")

db_con = dbConnect(MySQL(), user='root', password = db_password, host='localhost', dbname = "quotes")
res <- extend_database(db_connection = db_con, n_page = 1)
