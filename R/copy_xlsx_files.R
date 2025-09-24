#' Copies inventories in from an origin to a working folder and replaces extension
#'
#' This function copies LCA inventories (to be used for CLCC calculations) from an origin to a destination folder. It also
#' changes the file extension from `.XLSX` to `.xlsx` if needed. Sometimes spreadsheets generated with the LCA software SimaPro
#' have the extension in caps lock and this can cause the `clcc()` or the `clcc_mc()` functions to crash.
#'
#' @param source_folder Path to the folder in which raw `.XLSX` or `.xlsx` files from SimaPro are stored
#' @param destination_folder Path to the folder in which modified `.xlsx` files have to be saved
#' @export
#'
#' @examples
#' \dontrun{
#' source_folder <- path_to_inventory_folder
#' destination_folder <- path_where_to_save_data
#'
#' copy_xlsx_files(source_folder, destination_folder)
#'
#' }
copy_xlsx_files <-
  function(
    source_folder,
    destination_folder
  ){

    # Crea la cartella di destinazione se non esiste
    if (!dir.exists(destination_folder)) {
      dir.create(destination_folder, recursive = TRUE)
    }

    # Elenca tutti i file .XLSX e .xlsx nella cartella
    original_files <- list.files(
      source_folder,
      pattern = "\\.XLSX$|\\.xlsx$", # pattern che cattura entrambe le estensioni
      full.names = TRUE
    )

    # Se non ci sono file da copiare, esci
    if (length(original_files) == 0) {
      message("Nessun file .xlsx o .XLSX trovato nella cartella di origine.")
      return(invisible(NULL))
    }

    purrr::walk(
      original_files,
      \(x){

        file_name <- basename(x)

        new_file_name <- stringr::str_replace(file_name, "\\.XLSX$", ".xlsx")
        new_file_name <- stringr::str_replace(new_file_name, "\\.xlsx$", ".xlsx")

        destination_file_path <- file.path(destination_folder, new_file_name)

        # Checking if destination file already exists

        if(file.exists(destination_file_path)){

          source_time <- file.mtime(x)

          destination_time <- file.mtime(destination_file_path)

          if(source_time > destination_time){# file in origin folder more recent than in the destination folder

            tryCatch({
              sheets_data <- readxl::read_excel(x, col_names = TRUE, .name_repair = "unique_quiet")

              writexl::write_xlsx(sheets_data, destination_file_path)

              message(paste("The", new_file_name, "file has been updated in the destination folder"))

            }, error = function(e){

              warning(paste(file_name, "impossible to read or write", "-", e$message))

            }
            )
          } else {

            message(paste("The", new_file_name, "version in the destination folder is already up to date"))

          }
        } else {

        tryCatch({
          sheets_data <- readxl::read_excel(x, col_names = TRUE, .name_repair = "unique_quiet")

          writexl::write_xlsx(sheets_data, destination_file_path)

          message(paste(new_file_name, "copied in the destination folder"))

        }, error = function(e){

          warning(paste(file_name, "impossible to read or write", "-", e$message))

        }
        )
        }
      }
    )
  }
