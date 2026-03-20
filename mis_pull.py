print("Script to pull MIS data")

print("Load libraries...")
import oracledb
import pandas as pd
import os
import csv
import sys
from datetime import datetime
from dotenv import load_dotenv

print("Libraries loaded.")


start_time = datetime.now()

# --- 1. Check if environment variables are being loaded ---
print("Attempting to load environment variables...")
load_dotenv()
print("Environment variables loaded (or attempted to load).")

# --- 2. Check if environment variables are accessible ---
# For debugging, printing the *presence* (not value) or parts of DSN can be useful.
mis_user = os.getenv("misUser")
mis_password = os.getenv("misPassword")  # Don't print this value!
mis_dsn = os.getenv("misdDSN")

print(f"misUser loaded: {'Yes' if mis_user else 'No'}")
print(f"misdDSN loaded: {'Yes' if mis_dsn else 'No'}")

sys_path = os.getenv("sysPath")
if sys_path is not None:
    sys.path.append(sys_path)

import queries

always_pull = True  # override to always pull data

# create output directory for today's date
datetoday = datetime.today().strftime("%Y-%m-%d")
data_dir = os.getenv("dataPath")
if data_dir is None:
    raise ValueError("dataPath environment variable is not set")

raw = "raw"
file_path = os.path.join(data_dir, datetoday, raw)
print(f"Writing MIS data to: {file_path}")
if not os.path.exists(file_path):
    os.makedirs(file_path)


# --- 3. Check database connection attempt and success ---
try:
    print("Attempting to connect to Oracle database...")
    connection = oracledb.connect(
        user=mis_user,
        password=mis_password,
        dsn=mis_dsn,
    )
    print("Successfully connected to Oracle database!")

    # --- 4. Check cursor creation ---
    print("Attempting to create database cursor...")
    cursor = connection.cursor()
    print("Successfully created database cursor.")

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

except oracledb.Error as e:
    # --- 5. Handle connection errors ---
    (error,) = e.args
    print(f"Database connection error: {error.code} - {error.message}")
    sys.exit(1)  # Exit if connection fails

except Exception as e:
    # --- 6. Catch any other unexpected errors ---
    print(f"An unexpected error occurred: {e}")
    sys.exit(1)  # Exit on other errors

finally:
    # --- 7. Ensure resources are closed ---
    if "cursor" in locals() and cursor:
        print("Closing cursor...")
        cursor.close()
        print("Cursor closed.")
    if "connection" in locals() and connection:
        print("Closing connection...")
        connection.close()
        print("Connection closed.")

    end_time = datetime.now()
    elapsed_time = end_time - start_time
    total_seconds = elapsed_time.total_seconds()
    total_minutes = round(total_seconds / 60, 2)
    print(f"Total minutes elapsed: {total_minutes} minutes")
    print("Script finished.")
