
lib_path <- "lib"

#source(here(lib_path, "conv_log_to_transitions.R"))
#source(here(lib_path, "conv_markov_to_fitted_transition.R"))
#source(here(lib_path, "conv_transition_to_markov.R"))
#source(here(lib_path, "conv_transition_to_path.R"))
#source(here(lib_path, "conv_transition_to_probs.R"))
#source(here(lib_path, "format_probs_matrix.R"))
#source(here(lib_path, "ggplot_options.R"))
#source(here(lib_path, "info_log.R"))
#source(here(lib_path, "info_markov.R"))
#source(here(lib_path, "info_path.R"))
#source(here(lib_path, "info_transition.R"))
#source(here(lib_path, "paste_trans.R"))
#source(here(lib_path, "plot_transition.R"))
#source(here(lib_path, "prepare_event_log.R"))
#source(here(lib_path, "process_data.R"))
#source(here(lib_path, "vis_markov.R"))
#source(here(lib_path, "vis_npath.R"))
#source(here(lib_path, "vis_path.R"))
#source(here(lib_path, "vis_probs.R"))
#source(here(lib_path, "vis_sojourn.R"))
#source(here(lib_path, "vis_transition.R"))

# Alternative, more automatized approach

# lib_path <- file.path(getwd(), 'lib')
# r_files <- list.files(lib_path, pattern = "\\.R$", full.names = TRUE)
# for (file in r_files) {
# 	source(file)
# }
