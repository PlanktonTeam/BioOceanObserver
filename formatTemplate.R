## Header title
# Tab title

# function for UI module

HeaderTabUI <- function(id){
  
  nsHeaderTab <- NS(id)
  
  tagList(
    # add code from tab panel level in ui
  )
}

# function for server

HeaderTab <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      # add code to produce graphs etc.
      
    }
  )
}
    