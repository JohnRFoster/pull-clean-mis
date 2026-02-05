import oracledb
import pandas as pd
import os
import csv
import sys
from datetime import datetime
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

connection = oracledb.connect(
    user=os.getenv("misUser"),
    password=os.getenv("misPassword"),
    dsn=os.getenv("misdDSN"),
)

cursor = connection.cursor()

sys_path = os.getenv("sysPath")
if sys_path is not None:
    sys.path.append(sys_path)

import queries

myDate = "2014-01-01"  # we do not need data prior to 2014
always_pull = True  # override to always pull data

# create output directory for today's date
datetoday = datetime.today().strftime("%Y-%m-%d")
raw_dir = "data\\raw"
file_path = os.path.join(raw_dir, datetoday)
if not os.path.exists(file_path):
    os.makedirs(file_path)

# single pull
csv_name = os.path.join(file_path, "fs_national_all.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping pull.")
else:
    print("Pulling all data...")

    df = queries.single(cursor)

    print("single data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)
