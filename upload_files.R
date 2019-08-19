# load libraries
library("RCurl")

# get FTP username:password
userpass <- '"mattashby@lesscrime.info:kX3-HRh-YeC-cXe"'

# get list of directories and files output by Hugo
files <- here::here("public") %>% 
	dir(full.names = TRUE, recursive = TRUE, include.dirs = TRUE) %>% 
	file.info() %>% 
	rownames_to_column("path") %>% 
	mutate(short_path = str_remove(path, here::here("public/"))) %>% 
	as_tibble()

# check if each directory exists, and if not then create it
response <- c("some-random-dir") %>% 
	# files %>%
	# filter(isdir == TRUE) %>%
	# head(1) %>%
	# pull("short_path") %>%
	map(function (x) {
		
		# set up FTP URL
		ftp_url <- paste0("ftp://91.208.99.4/public_html/", x, "/")
		message(ftp_url, appendLF = TRUE)
		
		# check if directory exists
		curl_handle = getCurlHandle()
		tryCatch(
			getURL(ftp_url, dirlistonly = TRUE, userpwd = userpass, 
						 curl = curl_handle),
			Error = function (e) warning(e)
		)
		curl_info <- getCurlInfo(curl_handle)
		
		# if directory doesn't exist, create it
		if (curl_info$response.code == 550) {
			tryCatch(
				getURL(ftp_url, ftp_create_dirs = TRUE, userpwd = userpass, 
							 curl = curl_handle),
				Error = function (e) warning(e)
			)
			curl_info <- getCurlInfo(curl_handle)
			
			if (between(curl_info$response.code, 200, 299)) {
				message("Attempting to create directory", ftp_url, "ended with result",
								curl_info$response.code)
			} else {
				warning("Attempting to create directory", ftp_url, "ended with result",
								curl_info$response.code)
			}
			
		} else if (between(curl_info$response.code, 200, 299)) {
			
			curl_info$response.code
			
		} else {
			
			warning("FTP response code", curl_info$response.code, "for URL", ftp_url)
			curl_info$response.code
			
		}
		
	})
