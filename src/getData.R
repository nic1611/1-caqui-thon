library(httr)

bodyJ = '{
	"size": 2633,
	"_source": {
		"includes": [
			"email",
			"Cellphone",
			"name",
			"idCompany",
			"companyName",
			"createdOn",
			"status"
		]
	},
	"query": {
		"match_all": {}
	}
}'

get <- function(){
  res <- POST("https://fb2c51f4c54f41db874d8634b833b693.eastus2.azure.elastic-cloud.com:9243/alias-share-hom/_search", 
              content_type_json(),
              body = bodyJ, 
              authenticate('caquithon', 'caquithon#2021', type = "basic"))
}