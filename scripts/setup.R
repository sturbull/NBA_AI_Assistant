library(duckdb)
library(DBI)
library(here)

db_path <- here("NBA.duckdb")

# Delete if exists
if (file.exists(db_path)) {
  unlink(db_path)
}

# Load NBA.csv into a table named `NBA`
conn <- dbConnect(duckdb(), dbdir = db_path)
duckdb_read_csv(conn, "NBA", here("NBA.csv"))
dbDisconnect(conn)
