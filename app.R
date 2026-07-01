# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
biooceanobserver::run_app(MAPBOX_PUBLIC_TOKEN="pk.eyJ1IjoiamFzZWV2ZXJldHQiLCJhIjoiY21tdHA1dDllMDJ3czJwcTdrcDU5OG93aiJ9.OO3DjqVgqcDL5Etpgk_qzg") 
# add parameters here (if any) ## add data pth here for 
