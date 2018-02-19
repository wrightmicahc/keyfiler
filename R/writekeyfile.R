################################################################################
#' A Function to Write FVS keyfiles
#'
#' @description  This script writes a keyfile that will run FVS using the fire
#' and fuels extension FFE. Most arguments refer to FVS keywords. Descriptions
#' for the keyword fields have been included if possible.
#'
#' @param STDIDENT Stand ID number
#'
#' @param MGMTID Management/treatment ID, up to 40 characters
#'
#' @param STDINFO Stand information
#'
#' @param NUMCYCLE Integer. Number of simulation cycles, defaults to 10.
#'
#' @param TIMEINT Integer vector, length 2. time interval between simulations.
#'
#' @param DSNIN Name of input access database.
#'
#' @param TreeSQL SQL code for tree list table in DSNIN. Enter as single
#' quoted string.
#'
#' @param DSNOut: Name of output access database. Must be a preexisting access
#'  database with .accdb extension.
#'
#' @param FFE Logical. If TRUE, must specify the following FFE keywords:
#' FUELINIT, POTFTEMP, POTFWIND, POTFSEAS, FUELMOVE_C, FUELMOVE, POTFMOIS_S,
#' POTFMOIS_M, SIMFIRE
#'
#' @param FUELINIT: Numeric vector, length up to 12. Initializes fuel table for
#' "hard" fuels. From the FVS documentation:
#' field 1: Initial fuel load for the 0-1 inch class (tons/acre). This loading
#' gets divided equally between the 0-0.25 inch class and the 0.25-1 inch class.
#' field 2: Initial fuel load for the 1-3 inch class (tons/acre).
#' field 3: Initial fuel load for the 3-6 inch class (tons/acre).
#' field 4: Initial fuel load for the 6-12 inch class (tons/acre).
#' field 5: Initial fuel load for the 12-20 inch class (tons/acre).
#' field 6: Initial fuel load for litter (tons/acre).
#' field 7: Initial fuel load for duff (tons/acre).
#' field 8: Initial fuel load for the 0-0.25 inch class (tons/acre).
#' field 9: Initial fuel load for the 0.25-1 inch class (tons/acre).
#' field 10: Initial fuel load for the 20-35 inch class (tons/acre).
#' field 11: Initial fuel load for the 35-50 inch class (tons/acre).
#' field 12: Initial fuel load for the ≥ 50 inch class (tons/acre).
#'
#' @param POTFTEMP: Numeric vector, length 2. Temperature for severe [1] and
#' moderate [2] fires
#'
#' @param POTFWIND: Numeric vector, length 2. 20ft wind speed for severe [1] and
#' moderate [2] fires
#'
#' @param POTFSEAS: Numeric vector, length 2. Season for severe [1] and
#' moderate [2] fires
#'
#' @param FUELMOVE_C: Classes of fuels to remove, integers 1:11. 0=none,
#'  1=<0.25 inch, 2=0.25-1 inch, 3=1-3 inch, 4=3-6 inch, 5=6-12 inch,
#'  6=12-20 inch, 7=20-35 inch, 8=35-50 inch, 9=≥50 inch, 10=litter, 11=duff
#'
#' @param FUELMOVE: Proportion of fuel to remove. Needs to be included for
#' every fuel class altered. Defaults to 50. Currently only supports a single
#' value to remove for all classes listed in FUELMOVE_C.
#'
#' @param POTFMOIS_S: Numeric, lenth 6. Fuel moisture for severe fires
#'
#' @param POTFMOIS_M: Numeric, length 6. Fuel moisture for moderate fires
#'
#' @param SIMFIRE: Numeric, length 1. How often to simulate fires, 0 is every
#' cycle
#'
#' @param writefile Logical. Write output to text file?
#'
#' @param keypath File path were keyfile will be written.
#'
#' @export
#'
#' @references
#' Van Dyck, M.G. and Smith-Mateja, E.E., 2000. Keyword reference guide for the
#' Forest Vegetation Simulator. USDA Forest Service Forest Management Service
#' Center, Fort Collins, CO.
#'
#' Reinhardt, E.D. and Crookston, N.L., 2003. The fire and fuels extension to
#' the forest vegetation simulator. Fort Collins, CO: US Department of
#' Agriculture, Forest Service, Rocky Mountain Research Station.
#'
#' Crookston, N.L., Gammel, D.L., Rebain, S., Robinson, D.C.E. and Keyser, C.,
#' 2006. User’s guide to the database extension of the forest vegetation
#' simulator version 2.0. USDA Forest Service. Rocky Mountain Research Station.
################################################################################
# function to create FVS .key file
writekeyfile <- function(STDIDENT,
                         MGMTID = "None",
                         STDINFO = NULL,
                         NUMCYCLE = 10,
                         TIMEINT = c(0, 10),
                         DSNIN,
                         TreeSQL,
                         DSNOut,
                         FFE = FALSE,
                         FUELINIT = NULL,
                         POTFTEMP = NULL,
                         POTFWIND = NULL,
                         POTFSEAS = NULL,
                         FUELMOVE_C = NULL,
                         FUELMOVE = c( 50),
                         POTFMOIS_S = NULL,
                         POTFMOIS_M = NULL,
                         SIMFIRE = NULL,
                         writefile = FALSE,
                         keypath = NULL) {
        # browser()
        # function to pad whitespaces, -l is left justified
        add_space <- function(x, l){
                pad_x <- sprintf(paste0("% ", l, "s"), x)
                return(pad_x)}
        # create basic key word arguments
        base_args <- c("STDIDENT",
                       add_space(STDIDENT, -26),
                       "MGMTID",
                       MGMTID,
                       paste0(add_space("STDINFO", -10),
                              paste0(add_space(STDINFO, 10),
                                     collapse = "")),
                       add_space("DESIGN", -10),
                       paste0(add_space("NUMCYCLE", -10),
                              add_space(NUMCYCLE, 10)),
                       paste0(add_space("TIMEINT", -10),
                              paste0(add_space(TIMEINT, 10),
                                     collapse = "")),
                       "DataBase",
                       "DSNIN",
                       DSNIN,
                       "TreeSQL",
                       TreeSQL,
                       "EndSQL",
                       "End")

        # create FFE keyword arguments
        fire_args <- c("FMIN",
                       paste0(add_space("FUELINIT", -10),
                              paste0(add_space(FUELINIT, 10),
                                     collapse = "")),
                       paste0(add_space("POTFTEMP", -10),
                              paste0(add_space(POTFTEMP, 10),
                                     collapse = "")),
                       paste0(add_space("POTFWIND", -10),
                              paste0(add_space(POTFWIND, 10),
                                     collapse = "")),
                       paste0(add_space("POTFSEAS", -10),
                              paste0(add_space(POTFSEAS, 10),
                                     collapse = "")),
                       for (i in length(FUELMOVE_C)) {
                               paste0(add_space("FUELMOVE", -10),
                                      add_space(1, 10),
                                      add_space(FUELMOVE_C[i], 10),
                                      add_space(0, 10),
                                      add_space(0, 10),
                                      add_space(FUELMOVE, 10))
                       },
                       paste0(add_space("POTFMOIS_S", -10),
                              paste0(add_space(POTFMOIS_S, 10),
                                     collapse = "")),
                       paste0(add_space("POTFMOIS_M", -10),
                              paste0(add_space(POTFMOIS_M, 10),
                                     collapse = "")),
                       paste0(add_space("SIMFIRE", -10),
                              paste0(add_space(SIMFIRE, 10),
                                     collapse = "")),
                       "DataBase",
                       "DSNOut",
                       DSNOut,
                       "MORTREPT",
                       "POTFIRE",
                       "BURNREPT",
                       "FUELREPT",
                       "End",
                       "PROCESS",
                       "STOP")


        # create end key word arguments
        end_args  <- c("DataBase",
                       "DSNOut",
                       DSNOut,
                       "Compute",
                       "Summary",
                       "End",
                       "PROCESS",
                       "STOP")
        # combine
        if(FFE == FALSE){
                key_args <- c(base_args, end_args)
        } else {
                key_args <- c(base_args, fire_args)
        }


        if(writefile == FALSE){
                return(key_args)
        } else {
                fileConn <- file(keypath)
                writeLines(key_args, fileConn)
                close(fileConn)
        }
}


