# function for writine dataframes as csvs
def write_csvs(df, fileName):
    from datetime import datetime

    datetoday = datetime.today().strftime("%Y-%m-%d")


csv_name = "fs_national_effort.csv"
df.to_csv(csv_name, index=False)
