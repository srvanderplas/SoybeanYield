# R code to generate collapsible panel groups. 
library(shiny)
library(htmltools)
library(plyr)
library(magrittr)
library(stringr)

# <img src="photo I.png" width='80%' height='auto'>
#   <img src="photo II soybean experiment.png" width='80%' height='auto'>
#   <img src="Photo III SoybeanEmergen.png" width='80%' height='auto'>
  
paper <- data.frame(
  title = readLines("./Data/titles.txt"),
  text = readLines("./Data/text.txt"),
  figures = readLines("./Data/figures.txt"),
  captions = readLines("./Data/captions.txt"),
  stringsAsFactors=F
  )

paper$unique <- 1
paper$unique[paper$title==""] <- 0
paper$idx <- cumsum(paper$unique)

paper.condensed <- ddply(paper, .(idx), function(df){
  data.frame(
    title=df$title[1],
    text=paste0(paste0("<p align='left'>", df$text, "</p>", df$figures, "<br>", "<p align='center'>", df$captions, "</p>"), collapse="<br></br>"),
    stringsAsFactors=FALSE
  )
})

paper.condensed$text <- paper.condensed$text %>%
  str_replace(pattern="<br><p align='center'></p>$", replacement="")

paper.condensed$headings <- sprintf("heading%02d", paper.condensed$idx)
paper.condensed$href <-  sprintf("#collapse%02d", paper.condensed$idx)
paper.condensed$aria.controls <- sprintf("collapse%02d", paper.condensed$idx)

list.of.panels <- 
  sprintf(
    '<div class="panel panel-success">
        <div class="panel-heading" role="tab" id="%s">
          <h4 class="panel-title">
              <a href="%s" data-toggle="collapse" data-parent="#accordion" aria-expanded="false" aria-controls="%s">%s</a>
          </h4>
        </div>
        <div id="%s" class="panel-collapse collapse", role="tabpanel" aria-labelledby="%s">
          <div class="panel-body"> %s </div>
        </div>
     </div>', paper.condensed$headings, paper.condensed$href, paper.condensed$aria.controls, paper.condensed$title, paper.condensed$aria.controls, paper.condensed$headings, paper.condensed$text)

list.of.panels[1] <- list.of.panels[1] %>% 
  str_replace(pattern="panel-collapse collapse", replacement="panel-collapse collapse in") %>%
  str_replace(pattern='aria-expanded="false"', replacement='aria-expanded="true"') 
  

   
    
# <div class="panel-group" id="accordion" role="tablist" aria-multiselectable="true">
#   <div class="panel panel-default">
#   <div class="panel-heading" role="tab" id="headingOne">
#   <h4 class="panel-title">
#   <a data-toggle="collapse" data-parent="#accordion" href="#collapseOne" aria-expanded="true" aria-controls="collapseOne">
#   Collapsible Group Item #1
# </a>
#   </h4>
#   </div>
#   <div id="collapseOne" class="panel-collapse collapse" role="tabpanel" aria-labelledby="headingOne">
#   <div class="panel-body">
#   Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
# </div>
# </div>
# </div>
# <div class="panel panel-default">
# <div class="panel-heading" role="tab" id="headingTwo">
# <h4 class="panel-title">
# <a class="collapsed" data-toggle="collapse" data-parent="#accordion" href="#collapseTwo" aria-expanded="false" aria-controls="collapseTwo">
# Collapsible Group Item #2
# </a>
# </h4>
# </div>
# <div id="collapseTwo" class="panel-collapse collapse" role="tabpanel" aria-labelledby="headingTwo">
# <div class="panel-body">
# Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
#       </div>
#     </div>
#   </div>
#   <div class="panel panel-default">
#     <div class="panel-heading" role="tab" id="headingThree">
#       <h4 class="panel-title">
#         <a class="collapsed" data-toggle="collapse" data-parent="#accordion" href="#collapseThree" aria-expanded="false" aria-controls="collapseThree">
#           Collapsible Group Item #3
#         </a>
#       </h4>
#     </div>
#     <div id="collapseThree" class="panel-collapse collapse" role="tabpanel" aria-labelledby="headingThree">
#       <div class="panel-body">
#         Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
# </div>
# </div>
# </div>
# </div>