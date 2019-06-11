theme_col_series = function() {

  require(ggthemes)
  require(RColorBrewer)
  scale_color_distiller(direction=-1,palette = "Spectral")+theme_pander()
}
