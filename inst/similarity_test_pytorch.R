rm(list = ls())
devtools::load_all(".")

read_data <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/Pytorch/rat_only_score_ois_4643.csv")

harmonized_df = get_col_harmonized_scores_df(liver_score_data_frame = read_data,
                                             Round=FALSE)
