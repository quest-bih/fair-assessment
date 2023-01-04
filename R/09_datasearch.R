library(rdatacite)

test <- dc_dois(query = "publicationYear:2016", resource_type_id = "dataset")

dc_dois(query = "creators.affiliation:charite")

dc_dois(query = "creators.familyName:mil*")

dc_dois(query = 'creators.name:"Zhao, Dahe"')


test_3 <- dc_dois(query = "creators.affiliation:*", resource_type_id = "dataset")


test_3 <- dc_dois(query = "creators.affiliation:Karlsruhe", resource_type_id = "dataset")


test_3 <- dc_dois(query = "creators.affiliation:c*")

test_4 <- dc_dois(query = "publisher:charit*")


test_2 <- dc_dois(resource_type_id = "dataset")


dc_dois(query = 'creators.affiliation:"Karlsruhe Institute of Technology"', resource_type_id = "dataset")



library(httr)

url <- "https://api.test.datacite.org/dois"

queryString <- list(query = '"Zhao, Dahe"')

response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))

content(response, "text")

content(response)


library(httr)

url <- "https://api.test.datacite.org/dois"

queryString <- list(query = 'charite', # creators.familyName:mil* AND publicationYear:2022 '  titles.title:charite ' AND publicationYear:(2010 OR 2011)
                    `resource-type-id` = "Dataset") # ,publisher = "Charite"

response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))

test_dc2 <- content(response)




require(httr)

headers = c(
  `accept` = 'application/json;charset=UTF-8'
)

params = list(
  `query` = 'charite', # institutions:(+Berlin +Charit? -Architekturmuseum) #Berlin Institute of Health # 'institutions:charite OR affiliations:charite' # charite AND berlin
  `pageSize` = '50',
  #`institutionId` = '60026245',
 # `requester` = 'DS',
  `institutionName` = 'BIH',
  `repositoryType` = 'NON_ARTICLE_BASED_REPOSITORY',
  `publicationYear` = '2020'
 #,
#  `sort` = 'RELEVANCY_DATE'
)

res <- httr::GET(url = 'https://api.datasearch.elsevier.com/api/v3/search', httr::add_headers(.headers=headers), query = params)

test5 <- content(res)

data_search_list <- test5[["results"]]

test_dfr <- map_dfr(seq_along(data_search_list), 
                     ~ bind_cols(data_search_list[[.x]][["url"]]) %>%
                      mutate(publisher_name = data_search_list[[.x]][["publisherName"]]) %>%
                      mutate(institution = data_search_list[[.x]][["institutions"]][[1]][["name"]]))


test_dfr <- map_dfr(seq_along(data_search_list), 
                    ~ bind_cols(data_search_list[[.x]][["url"]]) %>%
                      mutate(publisher_name = data_search_list[[.x]][["publisherName"]]) %>%
                      mutate(institution = data_search_list[[.x]][["institutions"]]))


bind_rows(test5[["results"]][[2]][["url"]]) %>%
  mutate(rd_id = test5[["results"]][[2]][["url"]])

# Ist Dataset Doi zu MassIVE, das auch in 2020-Daten ist (allerdings als URL)
"https://doi.org/10.25345%2Fc5nt93"


require(httr)

headers = c(
  `accept` = 'application/json;charset=UTF-8'
)

res <- httr::GET(url = 'https://api.datasearch.elsevier.com/api/v3/sources', httr::add_headers(.headers=headers))

test5 <- content(res)

test6 <- bind_cols(test5)

test6 <- bind_rows(test5)

test6 <- as.data.frame(matrix(unlist(test5),nrow=length(test5),byrow=TRUE))

### END ###
