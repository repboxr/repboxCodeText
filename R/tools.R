loc_to_df = function(txt,loc, add.left=0, add.right=0) {
  if (NROW(loc)==0) {
    return(tibble(start=integer(0), end=integer(0), str=character(0)))
  }

  tibble(
    start = loc[,1],
    end = loc[,2],
    # stri_sub is faster
    str = stri_sub(txt, loc[,1]-add.left, loc[,2]+add.right)
    #str = substring(txt, loc[,1]-add.left, loc[,2]+add.right)
  )
}

my_rank = function(x) {
  restore.point("my_rank")
  if (length(x)==0) return(integer(0))
  vals = unique(x)
  match(x, vals)
}


loc_sep_lines = function(txt, loc) {
  restore.point("loc_sep_lines")

  if (NROW(loc)==0) {
    loc$row = integer(0)
    loc$col_start = integer(0)
    loc$col_end = integer(0)
    return(loc)
  }


  lines_loc = stri_locate_all_fixed(txt, "\n")[[1]]
  lines_start = c(1, lines_loc[,1]+1)
  loc$row = findInterval(loc$start, lines_start)
  loc$col_start = loc$start-lines_start[loc$row]+1
  loc$col_end = loc$end-lines_start[loc$row]+1
  loc


}
