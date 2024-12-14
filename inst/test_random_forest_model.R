

### ###########################-randomforestmodel-#############################
# rm(list = ls())
# devtools::load_all(".")
#
# # #Database Load
# dbtoken_liver <- sendigR::initEnvironment(dbType = 'sqlite',
#                                     #dbPath = "/opt/rstudio/users/MdAminulIslam.Prodhan/DataCentral.db",
#                                     dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db",
#                                     dbCreate = FALSE)
# # get the studies for the rat only species
# STUDYID_liver <- sendigR::genericQuery(dbtoken_liver, queryString = "SELECT DISTINCT STUDYID, DOMAIN
#                              FROM bw ", queryParams = NULL)
#
# selected_studies_liver <- as.vector(STUDYID_liver$STUDYID)
#
#
#
# Liver_get_liver_om_lb_mi_tox_score_list <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies_liver,
#                                                                                path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',
#                                                                                fake_study = TRUE,
#                                                                                use_xpt_file = FALSE,
#                                                                                output_individual_scores = TRUE)
#
#
#
# dbtoken_not_liver <- sendigR::initEnvironment(dbType = 'sqlite',
#                                           #dbPath = "/opt/rstudio/users/MdAminulIslam.Prodhan/DataCentral.db",
#                                           dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/fake_not_liver_update.db",
#                                           dbCreate = FALSE)
#
# # get the studies for the rat only species
# STUDYID_not_liver <- sendigR::genericQuery(dbtoken_not_liver, queryString = "SELECT DISTINCT STUDYID, DOMAIN
#                              FROM bw ", queryParams = NULL)
#
# selected_studies_not_liver <- as.vector(STUDYID_not_liver$STUDYID)
#
#
# not_Liver_get_liver_om_lb_mi_tox_score_list <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies_not_liver,
#                                                             path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/fake_not_liver_update.db',
#                                                             fake_study = TRUE,
#                                                             use_xpt_file = FALSE,
#                                                             output_individual_scores = TRUE)
#
#
#
# amin_get_random_forest_model <- get_random_forest_model(Liver_get_liver_om_lb_mi_tox_score_list,
#                                                         not_Liver_get_liver_om_lb_mi_tox_score_list)
#
#


#
dbPath_liver = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db"
dbPath_not_liver = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/fake_not_liver_update.db"

predicted_rf <- predicted_random_forest_model(dbPath_liver, dbPath_not_liver)




