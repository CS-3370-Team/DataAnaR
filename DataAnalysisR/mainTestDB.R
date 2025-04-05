devtools::clean_dll()
devtools::document()
devtools::build()
devtools::install()
devtools::load_all() # must run this

test_db_conn = DatabaseConnector$new()

test_db_conn$connect_db(db_type="azure", host="vifinancenews.database.windows.net",
                        port = 1433, dbname ="Vietnam_Finance_News",
                        user="ViFinanceNews", password = "ViFinanceNew#2025",in_drv="/opt/homebrew/lib/libmsodbcsql.18.dylib" )

article_table = test_db_conn$fetch_table_as_dataframe(table_name = "article")

article_table$show_dp()
test_db_conn$disconnect()
