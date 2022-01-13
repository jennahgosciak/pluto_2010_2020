gt_style <- function(gt) {
  gt %>%
    tab_options(table.align = "left") %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Calibri"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      ),
      locations = cells_column_labels(
        columns = gt::everything()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        color = "white",
        weight = px(0)
      ),
      locations = cells_body()
    )
}

theme_fc_map <- function(...) {
  #  theme_fc(...) %+replace%
  ggplot2::theme(
    line = ggplot2::element_blank(),
    rect = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.ticks.length =  ggplot2::unit(0, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    legend.key.size = ggplot2::unit(15, "pt"),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 7),
    complete = TRUE
  )
}

verify_isid <- function(df, x) {
  stopifnot(eeptools::isid(df, x))
  
  return(df)
}

# use function to compare building classes
recode_pluto <- function(df,bldgcodes, lu_desc, lu_codes) {
  df %>%
    as.data.frame() %>% 
    filter(bldgclass_20 != bldgclass_10 
           & !is.na(bldgclass_20) 
           & !is.na(bldgclass_10)) %>% 
    mutate(across(c("bldgclass_20", "bldgclass_10",
                    "landuse_20", "landuse_10"), .fns = lst(orig = ~.))) %>%
    ## add description for building class
    mutate(across(
      c("bldgclass_20", "bldgclass_10"),
      ~ (bldgcodes$Description[match(., bldgcodes$`Building Code`)])
    )) %>%
    ## add description for land use
    mutate(across(
      c("landuse_20", "landuse_10"),
      ~ (lu_desc[match(., lu_codes)])
    )) %>%
    # check that we didn't introduce missing values
    verify((is.na(bldgclass_20_orig) | bldgclass_20_orig == "  ") == is.na(bldgclass_20)) %>%
    verify((is.na(landuse_20_orig) | landuse_20_orig == "  ") == is.na(landuse_20)) %>%
    mutate(bldgclass_changes = str_c(bldgclass_10, " TO ", bldgclass_20),
           landuse_changes = str_c(landuse_10, " TO ", landuse_20)) %>% 
    # in order to avoid counting duplicate bbl_10 units
    group_by(bbl_10) %>% 
    mutate(unitsres_10 = case_when((n() > 1 & row_number() != 1) ~ NA_integer_,
                                   TRUE ~ unitsres_10)) %>% 
    ungroup() 
}