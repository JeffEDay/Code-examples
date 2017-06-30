# Load and initialize the needed libraries
memory.limit(size = 5120000000)
require(rJava)
require("openNLP")
library(NLP)
library(dplyr)
library(stringi)
library(stringr)
library(RWeka)
library(magrittr)
library(googleVis)

# need this for first time 
# if(!require("openNLPmodels.en")) {
# install.packages("openNLPmodels.en",
#                  repos = "http://datacube.wu.ac.at/",
#                  type = "source")
# }

# Read in the text files (Assumes project is in use and the working dictory for that
# project is where the text files reside.
# If not, add path to the Sys.glob function argument)
# This will read one or more files
# And create a single character vector with a space in between
# And convert to a string

filenames <- Sys.glob("d:/Auto Discourse/text files/*.txt")
filenames

texts <- filenames %>%
  lapply(readLines) %>%
  lapply(paste0, collapse = " ") %>%
  lapply(as.String)

names(texts) <- basename(filenames)

str(texts, max.level = 1)

# Function to annotate a document and return an object of class 
  annotate_entities <- function(doc, annotation_pipeline) {
  annotations <- annotate(doc, annotation_pipeline)
  AnnotatedPlainTextDocument(doc, annotations)
}

# Extract entities from an AnnotatedPlainTextDocument
  entities <- function(doc, kind) {
    s <- doc$content
    a <- annotations(doc)[[1]]
    if(hasArg(kind)) {
      k <- sapply(a$features, `[[`, "kind")
      s[a[k == kind]]
    } else {
      s[a[a$type == "entity"]]
    }
  }  
  
# Define the pipeline of annotations 
    pipeline <- list(
    Maxent_Sent_Token_Annotator(),
    Maxent_Word_Token_Annotator(),
    Maxent_POS_Tag_Annotator(),
    Maxent_Entity_Annotator(kind = "person"),
    Maxent_Entity_Annotator(kind = "location"),
    Maxent_Entity_Annotator(kind = "organization"),
    Maxent_Entity_Annotator(kind = "money"),
    Maxent_Entity_Annotator(kind = "date"),
    Maxent_Entity_Annotator(kind = "percentage")
  )  
# call the annotate_entities function and get the annotations in pipeline  
texts_annotated <- texts %>%
      lapply(annotate_entities, pipeline)
    

# Now we can pull out certain ones and examine the results
places <- texts_annotated %>%
  lapply(entities, kind = "location")
table(places)
persons <- texts_annotated %>%
  lapply(entities, kind = "person")
table(persons)
orgs <- texts_annotated %>%
  lapply(entities, kind = "organization")
table(orgs)

dates  <- texts_annotated %>%
  lapply(entities, kind = "date")

monies <- texts_annotated %>%
  lapply(entities, kind = "money")
table(monies)
percents <- texts_annotated %>%
  lapply(entities, kind = "percentage")

# Statistics will give us a sense of what we have managed to extract. 
# We can count up the number of items, as well as the number of unique items for each text.
# Total place mentions 

places %>%
  sapply(length)

persons %>%
  sapply(length)

orgs %>%
  sapply(length)

dates %>%
  sapply(length)

monies %>%
  sapply(length)

percents %>% 
  sapply(length)

# Unique 
places %>%
  lapply(unique) %>%
  sapply(length)

orgs %>%
  lapply(unique) %>%
  sapply(length)

persons %>%
  lapply(unique) %>%
  sapply(length)

dates %>%
  lapply(unique) %>%
  sapply(length)

monies %>%
  lapply(unique) %>%
  sapply(length)

percents %>%
  lapply(unique) %>%
  sapply(length)

# initialize ggmap and ggplot2
library(ggmap)

# all_places <- union(places[["pratt-parley.txt"]], 
#                    places[["cartwright-peter.txt"]]) %>% union(places[["lee-jarena.txt"]])
# all_places_geocoded <- geocode(all_places)

# Visualize with googleVis



