#PWD GSI MARS
#Written by Taylor Heffernan, PWD, and Nicholas Manna, AKRF
#Last modified: 06/10/2019
#Function based on steps to identify private SMPs for 1 tracking number, created by Taylor Heffernan


#lookupPrivateSMPs---------------------
#Rogygen
#' Return query results of private SMPs
#'
#' Returns private SMP tracking number, project name, SMP ID, and plan label.
#'
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' @param tracking_number chr, tracking number for the requested SMP
#'
#' @return Output will be a dateframe. For tracking numbers were found in database, output will be private SMP
#'   "tracking_number" (chr), "project name" (chr), "smp_id" (int), and "plan_label" (chr).
#'   If a tracking number is not found, that row will include an error message
#'   in the "project name" column, and NAs in "smp_id" and "plan_label".
#'
#' @export


lookupPrivateSMPs <- function(con, tracking_numbers){
  #Validate DB connection
  if(!dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }

  #Rather than validating each individual tracking number and selecting them one at a time
  #We can grab the entire table and filter by our tracking numbers to find the valid ones
  planreviewtable <- dbGetQuery(con, "select p.\"TrackingNumber\" as tracking_number, p.\"Projectname\" as project_name, p.\"SMPID\" as smp_id, p.\"Plan Label\" as plan_label from planreview_view_smpsummary_crosstab_asbuiltall p")
  hits <- filter(planreviewtable, tracking_number %in% tracking_numbers)

  #If any of the tracking numbers weren't found, we can return an error message
  misses <- tracking_numbers[!(tracking_numbers %in% planreviewtable$tracking_number)] %>%
    {data.frame(tracking_number = ., project_name = "Tracking number not found. Did you type it wrong?", smp_id = NA, plan_label = NA)}
  #{} around a function in a pipeline means that the pipe does not automatically insert the pipe antecedent value as the first argument of the next function
  #Instead, you direct where the antecedent value goes with a .
  #In this case, it is being named tracking number. If we didn't do it with the {}, the column of the data frame would be named .

  privateSMPs <- bind_rows(hits, misses)

  return(privateSMPs)
}

