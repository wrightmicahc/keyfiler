################################################################################
#' A Function to Write Batch for Running FVS Keyfiles
#'
#'This function creates a batch file to run FVS. Keyfiles can be created using
#'writekeyfile.
#' @param keyfile name of the .key file.
#' @param variant Specify the FVS variant. Character string, two letters,
#'  lower case. See example.
#' @param path_to_bat Character string with .bat extension, file path where
#' .bat file will be created.
#'
#'@param look Logical. Open .out file with notepad?
#'
#' @export
#'
#' @examples \dontrun{
#' writebatchfile(example.key, "nc", "temp/path/file.bat")
#' }
################################################################################
writebatchfile <- function(keyfile, variant, path_to_bat, look = TRUE) {
  # create arguments in batch file
  bat_args <- c("rem StdFVS run on DOS.",
              paste0("echo ", keyfile, ".key > ", keyfile, ".rsp"),
              paste0("echo ", keyfile, ".tre >> ", keyfile, ".rsp"),
              paste0("echo ", keyfile, ".out >> ", keyfile, ".rsp"),
              paste0("echo ", keyfile, ".trl >> ", keyfile, ".rsp"),
              paste0("echo ", keyfile, ".sum >> ", keyfile, ".rsp"),
              paste0("echo ", keyfile, ".chp >> ", keyfile, ".rsp"),
              paste0("if not exist ", keyfile, " mkdir ", keyfile, ""),
              paste0("\"C:\\FVSbin\\FVS", variant, ".exe \" < ", keyfile, ".rsp"),
              paste0("if not exist ", keyfile, "_index.svs rmdir ", keyfile, ""),
              paste0("del ", keyfile, ".rsp"),
              paste0("del ", keyfile, ".tre"),
              paste0("del ", keyfile, ".sum"),
              paste0("del ", keyfile, ".chp"))
              if(look = TRUE) {
                      bat_args <- c(bat_args,
                                    paste0("notepad ", keyfile, ".out"))
                      }
  # open path to batch file location
  fileConn <- file(path_to_bat)
  # write the batch file
  writeLines(bat_args, fileConn)
  # close path
  close(fileConn)
}
