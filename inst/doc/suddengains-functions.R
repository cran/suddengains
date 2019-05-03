## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, include = FALSE-------------------------------------------
# Load packages
library(suddengains)
library(dplyr)
library(ggplot2)
library(knitr)
library(DT)

## ------------------------------------------------------------------------
citation("suddengains")

## ----data-bdi, echo = FALSE----------------------------------------------

sgdata_bdi <- sgdata %>% 
    dplyr::select(id, 
                  bdi_s0:bdi_s12, 
                  bdi_fu1:bdi_fu2
                  )
    
DT::datatable(sgdata_bdi,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(pageLength = 5,
                             scrollX = TRUE,
                             fixedColumns = TRUE))

## ----data-rq, echo = FALSE-----------------------------------------------

sgdata_rq <- sgdata %>% 
    dplyr::select(id, 
                  rq_s0:rq_s12, 
                  rq_fu1:rq_fu2)
    
DT::datatable(sgdata_rq,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(pageLength = 5,
                             scrollX = TRUE,
                             fixedColumns = TRUE))

## ----select, eval = FALSE------------------------------------------------
#  # 1. method = "pattern"
#  select_cases(data = sgdata,
#               id_var_name = "id",
#               sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                               "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                               "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#               method = "pattern",
#               return_id_lgl = FALSE)
#  
#  # 2. method = "min_sess"
#  select_cases(data = sgdata,
#               id_var_name = "id",
#               sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                               "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                               "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#               method = "min_sess",
#               min_sess_num = 9,
#               return_id_lgl = TRUE)

## ------------------------------------------------------------------------
sgdata_select <- select_cases(data = sgdata,
                              id_var_name = "id",
                              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                             "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                              method = "pattern",
                              return_id_lgl = FALSE) %>% 
                 dplyr::filter(sg_select == TRUE)

## ------------------------------------------------------------------------
# Test define_crit1_cutoff function ----
define_crit1_cutoff(data_sessions = sgdata,
                    data_item = NULL,
                    tx_start_var_name = "bdi_s0",
                    tx_end_var_name = "bdi_s12",
                    reliability = 0.931)


## ---- eval = FALSE-------------------------------------------------------
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = .25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE)
#  

## ---- eval = FALSE-------------------------------------------------------
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = .25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  

## ---- eval = FALSE-------------------------------------------------------
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = .25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s0",
#                              "bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  identify_sl(data = sgdata,
#              sg_crit1_cutoff = -7,
#              sg_crit2_pct = .25,
#              sg_crit3 = TRUE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  # This example only uses the first and second sudden gains criteria
#  # All following examples work the same for the "identify_sl()" function
#  # The argument "crit123_details = TRUE" returns details about each between session interval for each criterion.
#  # Details about the third criterion will show NAs for each between session interval because it's not being used (sg_crit3 = FALSE)
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = .25,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  
#  # This example only uses the first criterion and a modified second criterion (50%)
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = .50,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  
#  # This example only uses the first criterion
#  # Details about the second and third criterion will show NAs for each between session interval
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = NULL,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)
#  
#  # This example only uses the first criterion
#  # Details about the second and third criterion will show NAs for each between session interval
#  identify_sg(data = sgdata,
#              sg_crit1_cutoff = 7,
#              sg_crit2_pct = NULL,
#              sg_crit3 = FALSE,
#              id_var_name = "id",
#              sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
#                              "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
#                              "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
#              identify_sg_1to2 = FALSE,
#              crit123_details = TRUE)

## ------------------------------------------------------------------------
bysg <- create_bysg(data = sgdata,
                    sg_crit1_cutoff = 7,
                    id_var_name = "id",
                    tx_start_var_name = "bdi_s1",
                    tx_end_var_name = "bdi_s12",
                    sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                    "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                    "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                    sg_measure_name = "bdi",
                    identify = "sg")

## ---- echo = FALSE-------------------------------------------------------
DT::datatable(bysg,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(
                  pageLength = 5,
                  scrollX = TRUE,
                  fixedColumns = TRUE))

## ------------------------------------------------------------------------
bysl <- create_bysg(data = sgdata,
                    sg_crit1_cutoff = -7,
                    id_var_name = "id",
                    tx_start_var_name = "bdi_s1",
                    tx_end_var_name = "bdi_s12",
                    sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                    "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                    "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                    sg_measure_name = "bdi",
                    identify = "sl")

## ---- echo = FALSE-------------------------------------------------------
DT::datatable(bysl,
              rownames = FALSE,
              extensions = 'FixedColumns',
              options = list(
                  pageLength = 5,
                  scrollX = TRUE,
                  fixedColumns = TRUE))

## ------------------------------------------------------------------------
byperson_first <- create_byperson(data = sgdata,
                                  sg_crit1_cutoff = 7,
                                  id_var_name = "id",
                                  tx_start_var_name = "bdi_s1",
                                  tx_end_var_name = "bdi_s12",
                                  sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                                  "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                                  "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                                  sg_measure_name = "bdi",
                                  identify_sg_1to2 = FALSE,
                                  multiple_sg_select = "first")

## ---- echo = FALSE-------------------------------------------------------
datatable(byperson_first,
          rownames = FALSE,
          extensions = 'FixedColumns',
          options = list(
              pageLength = 5,
              scrollX = TRUE,
              fixedColumns = TRUE))

## ------------------------------------------------------------------------
byperson_largest <- create_byperson(data = sgdata,
                                    sg_crit1_cutoff = 7,
                                    id_var_name = "id",
                                    tx_start_var_name = "bdi_s1",
                                    tx_end_var_name = "bdi_s12",
                                    sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4", 
                                                    "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8", 
                                                    "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                                    sg_measure_name = "bdi",
                                    identify_sg_1to2 = FALSE,
                                    multiple_sg_select = "largest")

## ---- echo = FALSE-------------------------------------------------------
DT::datatable(byperson_largest,
          rownames = FALSE,
          extensions = 'FixedColumns',
          options = list(
              pageLength = 5,
              scrollX = TRUE,
              fixedColumns = TRUE))

## ------------------------------------------------------------------------
# For bysg dataset select "id" and "rq" variables first
sgdata_rq <- sgdata %>% 
    dplyr::select(id, rq_s0:rq_s12)

# Join them with the sudden gains data set, here "bysg"
bysg_rq <- bysg %>%
    dplyr::left_join(sgdata_rq, by = "id")

# Extract "rq" scores around sudden gains on "bdi" in the bysg dataset
bysg_rq <- extract_values(data = bysg_rq,
                          id_var_name = "id_sg",
                          extract_var_list = c("rq_s1", "rq_s2", "rq_s3", "rq_s4", 
                                               "rq_s5", "rq_s6", "rq_s7", "rq_s8", 
                                               "rq_s9", "rq_s10", "rq_s11", "rq_s12"),
                          extract_measure_name = "rq",
                          add_to_data = TRUE)

## ---- echo = FALSE-------------------------------------------------------
datatable(bysg_rq,
          rownames = FALSE,
          extensions = 'FixedColumns',
          options = list(
              pageLength = 5,
              scrollX = TRUE,
              fixedColumns = TRUE))

## ------------------------------------------------------------------------
# Create plot of average change in depression symptoms around the gain
plot_sg_bdi <- plot_sg(data = bysg,
                       tx_start_var_name = "bdi_s1",
                       tx_end_var_name = "bdi_s12",
                       sg_pre_post_var_list = c("sg_bdi_2n", "sg_bdi_1n", "sg_bdi_n",
                                                "sg_bdi_n1", "sg_bdi_n2", "sg_bdi_n3"),
                       ylab = "BDI", xlab = "Session",
                       colour = "#239b89ff")

# Create plot of average change in rumination around the gain
plot_sg_rq <- plot_sg(data = bysg_rq,
                       tx_start_var_name = "rq_s1",
                       tx_end_var_name = "rq_s12",
                       sg_pre_post_var_list = c("sg_rq_2n", "sg_rq_1n", "sg_rq_n",
                                                "sg_rq_n1", "sg_rq_n2", "sg_rq_n3"),
                       ylab = "RQ", xlab = "Session",
                       colour = "#440154FF") 


# It is possible apply other ggplot2 functions to the plot now,
# e.g. y axis scale, or x axis labels ...

plot_sg_bdi <- plot_sg_bdi + 
               ggplot2::coord_cartesian(ylim = c(0, 50))

plot_sg_rq <- plot_sg_rq + 
              ggplot2::scale_x_discrete(labels = c("First", "n-2", "n-1", "n",
                                                   "n+1", "n+2", "n+3", "Last"))

## ---- fig.show = 'hold'--------------------------------------------------
plot_sg_bdi
plot_sg_rq 

## ------------------------------------------------------------------------
count_intervals(data = sgdata_select,
                id_var_name = "id",
                sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3", "bdi_s4",
                                "bdi_s5", "bdi_s6", "bdi_s7", "bdi_s8",
                                "bdi_s9", "bdi_s10", "bdi_s11", "bdi_s12"),
                identify_sg_1to2 = FALSE)

## ------------------------------------------------------------------------
# Describe bysg dataset ----
describe_sg(data = bysg, 
            sg_data_structure = "bysg")

# Describe byperson dataset ----
describe_sg(data = byperson_first, 
            sg_data_structure = "byperson")

