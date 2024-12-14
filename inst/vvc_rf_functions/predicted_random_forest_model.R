#' @title predicted_random_forest_model
#' @param dbPath_liver Mandatory, character \cr
#'   Studyid number
#' @param dbPath_not_liver Mandatory, character \cr
#'   path of database
#' @return random forst model
#'
#' @examples
#' \dontrun{
#' get_mi_score(studyid='1234123', path_db='path/to/database.db')
#' }
#' @export


predicted_random_forest_model <- function(dbPath_liver, dbPath_not_liver) {

#Database Load
dbtoken_liver <- sendigR::initEnvironment(dbType = 'sqlite',
                                          dbPath = dbPath_liver,
                                          #dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db",
                                          dbCreate = FALSE)
# get the studies for the rat only species
STUDYID_liver <- sendigR::genericQuery(dbtoken_liver, queryString = "SELECT DISTINCT STUDYID, DOMAIN
                             FROM bw ", queryParams = NULL)

selected_studies_liver <- as.vector(STUDYID_liver$STUDYID)



Liver_get_liver_om_lb_mi_tox_score_list <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies_liver,
                                                                               path_db = dbPath_liver,
                                                                               #path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',
                                                                               fake_study = TRUE,
                                                                               use_xpt_file = FALSE,
                                                                               output_individual_scores = TRUE)

# Print the result for debugging
print("Liver Toxicology Scores:")
print(head(Liver_get_liver_om_lb_mi_tox_score_list))  # Print the first few rows
#~~~~~~~for-not-liver~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dbtoken_not_liver <- sendigR::initEnvironment(dbType = 'sqlite',
                                              dbPath = dbPath_not_liver,
                                              #dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/fake_not_liver_update.db",
                                              dbCreate = FALSE)

# get the studies for the rat only species
STUDYID_not_liver <- sendigR::genericQuery(dbtoken_not_liver, queryString = "SELECT DISTINCT STUDYID, DOMAIN
                             FROM bw ", queryParams = NULL)

selected_studies_not_liver <- as.vector(STUDYID_not_liver$STUDYID)


not_Liver_get_liver_om_lb_mi_tox_score_list <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies_not_liver,
                                                                                   path_db = dbPath_not_liver,
                                                                                   #path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/fake_not_liver_update.db',
                                                                                   fake_study = TRUE,
                                                                                   use_xpt_file = FALSE,
                                                                                   output_individual_scores = TRUE)

# Print the result for debugging
print("Not Liver Toxicology Scores:")
print(head(not_Liver_get_liver_om_lb_mi_tox_score_list))  # Print the first few rows

# Check if the variables are not NULL before calling the random forest function
if (is.null(Liver_get_liver_om_lb_mi_tox_score_list) || is.null(not_Liver_get_liver_om_lb_mi_tox_score_list)) {
  stop("One or both toxicology score lists are NULL!")
}
browser()
# Inspect the structure of the liver toxicology scores
str(Liver_get_liver_om_lb_mi_tox_score_list)
# Inspect the structure of the non-liver toxicology scores
str(not_Liver_get_liver_om_lb_mi_tox_score_list)


# random-forest-model----------------------------
#amin_get_random_forest_model <- get_random_forest_model(Liver_get_liver_om_lb_mi_tox_score_list , not_Liver_get_liver_om_lb_mi_tox_score_list )
# Random Forest Model
amin_get_random_forest_model <- get_random_forest_model(
  liver_om_lb_mi_tox_score_list = Liver_get_liver_om_lb_mi_tox_score_list,
  not_liver_om_lb_mi_tox_score_list = not_Liver_get_liver_om_lb_mi_tox_score_list)

return(amin_get_random_forest_model)  # Return the random forest model or relevant results
 }
