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
always_pull = False  # override to always pull data

# create output directory for today's date
datetoday = datetime.today().strftime("%Y-%m-%d")
raw_dir = "data\\raw"
file_path = os.path.join(raw_dir, datetoday)
if not os.path.exists(file_path):
    os.makedirs(file_path)

# effort pull
csv_name = os.path.join(file_path, "fs_national_effort.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping effort pull.")
else:
    print("Pulling effort data...")

    df = queries.effort(cursor, myDate)

    print("Effort data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)

# take by method pull
csv_name = os.path.join(file_path, "fs_national_take_by_method.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping take by method pull.")
else:
    print("Pulling take by method data...")

    df = queries.take_by_method(cursor, myDate)

    print("take by method data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)

# take by property pull
csv_name = os.path.join(file_path, "fs_national_take_by_property.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping take by property pull.")
else:
    print("Pulling take by property data...")

    df = queries.take_by_property(cursor, myDate)

    print("take by property data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)


# property pull
csv_name = os.path.join(file_path, "fs_national_property.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping property pull.")
else:
    print("Pulling property data...")

    df = queries.property(cursor, myDate)

    print("property data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)

# damage pull
csv_name = os.path.join(file_path, "fs_national_damage.csv")

if os.path.exists(csv_name) and not always_pull:
    print(f"{csv_name} already exists, skipping damage pull.")
else:
    print("Pulling damage data...")

    df = queries.damage(cursor, myDate)

    print("damage data pulled successfully.")

    # write as csv
    df.to_csv(csv_name, index=False)


connection.close()
