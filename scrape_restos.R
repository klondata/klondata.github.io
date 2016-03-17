library(rvest)
library(stringr)
library(magrittr)
library(maptools)
library(RColorBrewer)
library(classInt)
library(rgeos)

start = 0
scrape_page = function(start){
  
  restos = read_html(paste0("http://www.yelp.ca/search?find_loc=Montr%C3%A9al,+QC,+Canada&start=", start, "&cflt=food"))
  
  n1 = restos %>% html_nodes(".main-attributes")
  n2 = restos %>% html_nodes(".secondary-attributes")
  
  df = data.frame(neighborhood=factor(), rating=numeric(), votes=numeric())
  
  for(i in 1:10){
    
    main_attr = n1 %>% extract2(i)
    sec_attr = n2 %>% extract2(i)
    
    if(length(sec_attr %>% html_nodes(".neighborhood-str-list")) == 1
       && length(main_attr %>% html_nodes(".star-img")) == 1
       && length(main_attr %>% html_nodes(".review-count")) == 1){
      
      neighborhood = iconv(sec_attr %>% html_nodes(".neighborhood-str-list") %>% html_text() %>% str_trim(), 
                            from="UTF-8", to="UTF-8")
      
      if(str_detect(neighborhood, "Notre-Dame") || str_detect(neighborhood, "des-Neiges")){
        neighborhood = "Côte-des-Neiges-Notre-Dame-de-Grâce"
      }
      else if(str_detect(neighborhood, "Plateau")){
        neighborhood = "Le Plateau-Mont-Royal"
      }
      else if(str_detect(neighborhood, "Sud")){
        neighboorhood = "Le Sud-Ouest"
      }
      else if(str_detect(neighborhood, "Ville-Marie")){
        neighboorhood = "Ville-Marie"
      }
      else if(str_detect(neighborhood, "Villeray")){
        neighborhood = "Villeray-Saint-Michel-Parc-Extension"
      }
      else if(str_detect(neighborhood, "Saint-Luc")){
        neighborhood = "Côte-Saint-Luc"
      }
      else if(str_detect(neighborhood, "Trembles")){
        neighborhood = "Rivière-des-Prairies-Pointe-aux-Trembles"
      }
      else if(str_detect(neighborhood, "onard")){
        neighborhood = "Saint-Léonard"
      }
      else if(str_detect(neighborhood, "Montr")){
        neighborhood = "Montréal-Ouest"
      }
      else if(str_detect(neighborhood, "Ahun")){
        neighborhood = "Ahuntsic-Cartierville"
      }
      
      rating = as.numeric(main_attr %>% html_nodes(".star-img") %>% html_attr("title") %>% str_extract("[0-9]\\.[0-9]"))
      votes = as.numeric(main_attr %>% html_nodes(".review-count") %>% html_text() %>% str_extract("[0-9]+"))
      df = rbind(df, data.frame(neighborhood=neighborhood, rating=rating, votes=votes))
      
    }    
  }
  
  df
}

all_ratings = lapply(seq(from=0, to=990, by=10), scrape_page)
all_ratings_df = do.call(rbind, all_ratings)
all_ratings_df$points = all_ratings_df$rating * all_ratings_df$votes

neighborhood_points = aggregate(points ~ neighborhood, data=all_ratings_df, sum)

tmp_dir = tempdir()
url_data = "http://donnees.ville.montreal.qc.ca/dataset/00bd85eb-23aa-4669-8f1b-ba9a000e3dd8/resource/62f7ce10-36ce-4bbd-b419-8f0a10d3b280/download/limadmin-shp.zip"
zip_file = paste0(tmp_dir, "/montreal.zip")
download.file(url_data, zip_file)
unzip(zip_file, exdir=tmp_dir)

montreal = readShapeSpatial(paste0(tmp_dir, "/LIMADMIN.shp"))

df = merge(montreal@data, neighborhood_points, by.x="NOM", by.y="neighborhood", all.x=TRUE)

#reorder df to match montreal@data
df = df[rank(montreal@data$NOM),]

ci = classIntervals(df$points, n=3, style="quantile")

palette = brewer.pal(8, "YlOrRd")[c(2,4,6)]

colors = findColours(ci, palette)

plot(montreal, col=colors)
legend('topleft', legend=c("Starvation area", "Survivable", "Foodie's heaven"), fill=palette)

centroids = gCentroid(montreal, byid=TRUE)[!is.na(df$points)]
text(centroids$x, centroids$y, labels=montreal@data$NOM[!is.na(df$points)], cex=.7)
