

version_style <- function(){"font-size: 14px; color:#93A3A3;"}
version_style_additional <- function(){
  "-webkit-user-select: none;
  -khtml-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  -o-user-select: none;
  user-select: none;"
}

rokaiLogo <- div(
  style = "margin-bottom:0px; padding-bottom:0px;",
  div(
    style = "position: relative; width: 100%",
    img(src='rokaiXplorer_logo.png', align = "left", style = "height: 53px; margin-bottom:0px; margin-top: 10px;"),
    
    tags$p(version_text(), style = paste(version_style(), version_style_additional(), "position: absolute; top: 35px; left:287px; width: 70%;", sep = ""))
  )
)
