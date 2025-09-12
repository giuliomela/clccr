clean_xlsx_data <- function(
    xlsx_path
    ){

# cleaning xlsx file

  wb <- # workbook to work into
    openxlsx2::wb_load(xlsx_path)


  wb_clean <-
    openxlsx2::wb_to_df(
    file = xlsx_path,
    sheet = 1,
    skip_hidden_rows = TRUE,
    skip_hidden_cols = TRUE,
    skip_empty_rows  = TRUE,
    skip_empty_cols  = TRUE
  )


  return(wb_clean)

}
