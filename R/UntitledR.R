if(fake_study == TRUE & use_xpt_file == FALSE){


} else if(fake_study == TRUE & use_xpt_file == TRUE) {


} else {
  #

}

add condition in the above code block where fake_study == FALSE & use_xpt_file == TRUE

the above code block is inside this below function
get_compile_data <- function(studyid = NULL,
                             path_db,
                             fake_study = FALSE,
                             use_xpt_file = FALSE) { }
