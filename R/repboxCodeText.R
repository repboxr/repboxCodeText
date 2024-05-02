example = function() {
  library(repboxCodeText)
  library(repboxHtml)
  project_dir = "/home/rstudio/repbox/projects_reg/testart"
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  code_project_find_refs(project_dir)

  rstudioapi::filesPaneNavigate(project_dir)

  html.dir = file.path(project_dir,"reports")
  rstudioapi::filesPaneNavigate(html.dir)
}

code_project_find_refs = function(project_dir, parcels = NULL) {
  restore.point("code_project_find_refs")
  parcels = repdb_load_parcels(project_dir, c("stata_source","stata_cmd"))
  source_df = parcels$stata_source$script_source
  if (NROW(source_df)==0) return(parcels)

  cmd_df = parcels$stata_cmd$stata_cmd

  i = 2
  res_li = lapply(seq_rows(source_df), function(i) {
    fp = source_df$file_path[i]
    code_find_table_figure_ref(source_df[i,], cmd_df %>% filter(file_path==fp))
  })
  cmd_ref = bind_rows(res_li) %>%
    filter(!is.na(ref_type)) %>%
    mutate(artid = basename(project_dir))

  if (any(duplicated(cmd_ref[c("file_path","line")]))) {
    stop("Repbox found multiple table or figure references for a code line. This should not be the case and can lead to errors in later computations.")
  }

  repdb_check_data(cmd_ref, "stata_cmd_tab_fig_ref")

  parcels$stata_cmd_tab_fig_ref = list(stata_cmd_tab_fig_ref = cmd_ref)
  repdb_save_parcels(parcels["stata_cmd_tab_fig_ref"], file.path(project_dir, "repdb"))

  parcels
}


# TO DO: Add refs based on filename
# Think of how to deal with refs to multiple tables
code_find_table_figure_ref = function(source, cmd_df) {
  restore.point("code_find_table_figure_ref")
  txt = source$text
  ltxt = tolower(txt)

  tab1_pos = stri_locate_all_regex(ltxt, "table[ ]*[0-9][0-9]?(?![a-z])", omit_no_match = TRUE)[[1]] %>%
    loc_to_df(txt, .)

  tab2_pos = stri_locate_all_regex(ltxt, "table[ ]*[a-z][0-9]?[0-10](?![a-z])", omit_no_match = TRUE)[[1]] %>%
    loc_to_df(txt, .)


  tab_pos = bind_rows(
    tab1_pos ,
    tab2_pos
  ) %>% mutate(type = "tab")
  id_start = stri_locate_first_regex(tolower(tab_pos$str),"table[ ]*")[,2]+1
  tab_pos$id = stri_sub(tab_pos$str, id_start)

  # Figures
  fig1_pos = stri_locate_all_regex(ltxt, "figure[ ]*[0-9][0-9]?(?![a-z])", omit_no_match = TRUE)[[1]] %>%
    loc_to_df(txt, .)

  fig2_pos = stri_locate_all_regex(ltxt, "figure[ ]*[a-z][0-9]?[0-10](?![a-z])", omit_no_match = TRUE)[[1]] %>%
    loc_to_df(txt, .)

  fig_pos = bind_rows(
    fig1_pos ,
    fig2_pos
  ) %>% mutate(type = "fig")
  id_start = stri_locate_first_regex(tolower(fig_pos$str),"figure[ ]*")[,2]+1
  fig_pos$id = stri_sub(fig_pos$str, id_start)

  pos = bind_rows(tab_pos, fig_pos) %>% arrange(start)

  loc = loc_sep_lines(txt, pos) %>%
    rename(orgline = row) %>%
    mutate(file_path = source$file_path)

  # We only want to use table or figure references that are
  # not in a code line but in a comment
  loc = anti_join(loc, cmd_df, by = c("file_path", "orgline"))

  loc$.loc.row = seq_rows(loc)
  cmd_df$.loc.row = findInterval(cmd_df$orgline, loc$orgline)

  cmd_ref = left_join(select(cmd_df, file_path, line, cmd,in_program, .loc.row), select(loc, .loc.row, ref_type=type, ref_id=id), by=".loc.row")

  cmd_ref = cmd_ref %>%
    mutate(ref_source = "co")

  # TO DO: If we add also references from do file names, ensure that there
  # are no duplicates. We don't want the same table reference twice with
  # a comment and a do file name, instead we can specify a new ref_source
  # "cofi"

  # dupl = duplicated(cmd_ref[,c("file_path","line","ref_type","ref_id")])
  # cmd_ref = cmd_ref[!dupl,]

  cmd_ref

}
