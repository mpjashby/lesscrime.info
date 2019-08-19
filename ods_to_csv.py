import unicodecsv as csv
import pyexcel

def ods_to_csv(ods_file, ods_sheet, csv_file):

  # load data
  data = pyexcel.get_sheet(file_name = ods_file, sheet_name = ods_sheet)
  # print("Finished reading ODS file, now beginning to write")

  # save data
  with open(csv_file, "w") as write_file:
    writer = csv.writer(write_file, encoding = "utf-8")
    writer.writerows(data)
