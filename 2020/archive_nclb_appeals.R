# Archive NCLBAppeals
# Script to archive files

acct_app_files <- list.files("N:/ORP_accountability/projects/NCLBAppeals/Accountability Web Files", full.names = TRUE)

for (file_name in acct_app_files) {
  file.copy(
    from = file_name,
    to = gsub(
      pattern = "N:/ORP_accountability/projects/NCLBAppeals/Accountability Web Files",
      replacement = "N:/ORP_accountability/projects/NCLBAppeals/2019-20 Archive Accountability Web Files/20201016",
      x = file_name
    )
  )
  # print(
  #   gsub(
  #         pattern = "N:/ORP_accountability/projects/NCLBAppeals/Accountability Web Files",
  #         replacement = "N:/ORP_accountability/projects/NCLBAppeals/2019-20 Archive Accountability Web Files/20201016",
  #         x = file_name
  #       )
  # )
}


