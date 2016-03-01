# Using R to query a database and return a data frame

# load package
require(RODBC)

# create a connection
my_connection = odbcConnect(dsn="SQL38", uid="USERID", pwd=.rs.askForPassword("Please enter password"))
# if 'sourcing' this code from another R script, you can include the password, as it will be hidden
# my_connection = odbcConnect(dsn="SQL38", uid="USERID", pwd="PASSWORD")

# create a query
my_query = "
select 
a.*,
b.*

from my_database..table_A as a

inner join my_database..table_B as b
on (a.something = b.something)

where everything = 'awesome'
"

# run query through connection and return data frame
my_data_frame = sqlQuery(my_connection, my_query, stringsAsFactors=F, as.is=T)

# close connection
close(SQL38_conn)
