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

cursor

sys_path = os.getenv("sysPath")
if sys_path is not None:
    sys.path.append(sys_path)

import queries

always_pull = True  # override to always pull data

# create output directory for today's date
datetoday = datetime.today().strftime("%Y-%m-%d")
data_dir = os.getenv("dataPath")
raw = "raw"
file_path = os.path.join(data_dir, datetoday, raw)
print(f"Writing MIS data to: {file_path}")
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

# effort pull
csv_name = os.path.join(file_path, "fs_national_effort.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping effort pull.")
else:
    print("Pulling effort data...")

    df = queries.effort(cursor)

    print("Effort data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)

# property pull
csv_name = os.path.join(file_path, "fs_national_property.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping property pull.")
else:
    print("Pulling property data...")

    df = queries.property(cursor)

    print("property data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)


connection.close()
