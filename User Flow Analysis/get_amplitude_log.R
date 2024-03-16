library(httr)

params = list(
  'start' = '20231231T15', # start: (start_date - 1) + T15
  'end' = '20240229T14' # end: (end_date) + T14
)

amplitude_log <- httr::GET(url = 'https://amplitude.com/api/2/export', query = params, 
                 httr::authenticate('KEY', 'SECRET_KEY'))



file_name = '240101-240229'
file_name_zip = paste0(file_name, '.zip')
unzip_file_name = 'AMPLITUDE_ORIGINAL_NUMBER' # original number

writeBin(content(amplitude_log, "raw"), file_name_zip) # write zip file
unzip(file_name_zip) # unzip

file.rename(unzip_file_name, file_name)
file.remove(file_name_zip) # remove raw zip file



