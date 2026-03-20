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


def single(cursor):
    cursor.execute(
        """
   SELECT
    -- Core IDs
    M2_WORK_TASK.ID AS WT_ID,
    M2_AGREEMENT_PROPERTY.AGRP_PRP_ID,
    M2_WORK_TASK.WT_AGRPROP_ID,

    -- Property information (LEFT JOINs per Will semantics)
    M2_PROPERTY.PRP_NAME,
    M2_COUNTY.CNTY_NAME,
    M2_COUNTY.CNTY_GSA_CNTY_CD,
    M2_STATE.ST_NAME,
    M2_STATE.ST_GSA_STATE_CD,
    M2_PROPERTY_SIZE.PRPS_QTY,
    M2_PROPERTY_SIZE.PRPS_PROP_TYPE,
    M2_PROPERTY_UOL.PRPU_N_LAT,
    M2_PROPERTY_UOL.PRPU_E_LONG,
    M2_UNIT_OF_MEASURE.UOM_NAME AS PROPERTY_SIZE_UOM,

    -- Allowed species (kept for ALWS_AGRPROP_ID)
    M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID,

    -- Work task info
    M2_WORK_TASK.WT_WORK_DATE,

    -- Component & usage info
    M2_COMPONENT.CMP_NAME,
    M2_COMPONENT.CMP_TYPE,
    M2_USE_TYPE.USET_NAME,
    M2_WORK_TASK_COMP_UOM.WTCM_QTY,

    -- Damage agent (driven by work_result to match Will)
    M2_DAMAGE_AGENT.ID AS DAMAGE_AGENT_ID,
    M2_DAMAGE_AGENT.DA_NAME,

    -- Fate & work result (inner joins + killed filter)
    M2_FATE.FATE_WKR_ID,
    M2_FATE.FATE_FATE,
    M2_WORK_RESULT.WKR_QTY,
    M2_WORK_RESULT.WKR_INTENTIONAL,
    M2_WORK_RESULT.WKR_TARGET

FROM M2_WORK_TASK

    -- Components (required)
    INNER JOIN M2_WORK_TASK_COMPONENT
        ON M2_WORK_TASK_COMPONENT.WTC_WT_ID = M2_WORK_TASK.ID
    INNER JOIN M2_COMPONENT
        ON M2_COMPONENT.ID = M2_WORK_TASK_COMPONENT.WTC_CMP_ID

    -- Use Type (optional in Will, so LEFT)
    LEFT JOIN M2_USE_TYPE
        ON M2_USE_TYPE.ID = M2_WORK_TASK_COMPONENT.WTC_USET_ID

    -- Component UOM (optional in Will, so LEFT)
    LEFT JOIN M2_WORK_TASK_COMP_UOM
        ON M2_WORK_TASK_COMPONENT.ID = M2_WORK_TASK_COMP_UOM.WTCM_WTC_ID

    -- Agreement & property (optional per Will, so LEFT)
    LEFT JOIN M2_AGREEMENT_PROPERTY
        ON M2_AGREEMENT_PROPERTY.ID = M2_WORK_TASK.WT_AGRPROP_ID
    LEFT JOIN M2_PROPERTY
        ON M2_PROPERTY.ID = M2_AGREEMENT_PROPERTY.AGRP_PRP_ID
    LEFT JOIN M2_COUNTY
        ON M2_COUNTY.ID = M2_PROPERTY.PRP_CNTY_ID
    LEFT JOIN M2_STATE
        ON M2_STATE.ID = M2_PROPERTY.PRP_ST_ID
    LEFT JOIN M2_PROPERTY_SIZE
        ON M2_PROPERTY.ID = M2_PROPERTY_SIZE.PRPS_PRP_ID
    LEFT JOIN M2_UNIT_OF_MEASURE
        ON M2_UNIT_OF_MEASURE.ID = M2_PROPERTY_SIZE.PRPS_UOM_ID
    LEFT JOIN M2_PROPERTY_UOL
        ON M2_PROPERTY.ID = M2_PROPERTY_UOL.PRPU_PRP_ID

    -- Allowed species (optional; included to surface ALWS_AGRPROP_ID)
    LEFT JOIN M2_ALLOWED_SPECIES
        ON M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID = M2_AGREEMENT_PROPERTY.ID

    -- Fate & work result (match Will: inner joins, tied to component)
    INNER JOIN M2_FATE
        ON M2_FATE.FATE_WTC_ID = M2_WORK_TASK_COMPONENT.ID
    INNER JOIN M2_WORK_RESULT
        ON M2_WORK_RESULT.ID = M2_FATE.FATE_WKR_ID

    -- Damage agent from work_result (match Will)
    INNER JOIN M2_DAMAGE_AGENT
        ON M2_DAMAGE_AGENT.ID = M2_WORK_RESULT.WKR_DA_ID

WHERE
    M2_FATE.FATE_FATE = 'KILLED' AND
    M2_WORK_TASK.WT_WORK_DATE >= to_date(' 2001-01-01 ','yyyy-mm-dd')      
          """
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names

    return df


def effort(cursor):
    cursor.execute(
        """
          SELECT 
               M2_AGREEMENT_PROPERTY.AGRP_PRP_ID,
			M2_WORK_TASK.WT_AGRPROP_ID,
               M2_WORK_TASK_UOM.WTM_WT_ID,
               M2_WORK_TASK.WT_WORK_DATE,
               M2_WORK_TASK_UOM.WTM_QTY,
               M2_UNIT_OF_MEASURE.UOM_NAME,
               M2_DAMAGE_AGENT.ID,
               M2_DAMAGE_AGENT.DA_NAME,
               M2_WORK_TASK_COMP_UOM.WTCM_QTY,
               M2_USE_TYPE.USET_NAME,
               M2_COMPONENT.CMP_NAME,
               M2_COMPONENT.CMP_TYPE
          FROM 
               M2_AGREEMENT_PROPERTY
          INNER JOIN M2_WORK_TASK ON 
               M2_AGREEMENT_PROPERTY.ID = M2_WORK_TASK.WT_AGRPROP_ID
          INNER JOIN M2_CONFLICT_WORK ON
               M2_WORK_TASK.ID = M2_CONFLICT_WORK.CNFW_WT_ID
          INNER JOIN M2_DAMAGE_AGENT ON 
               M2_DAMAGE_AGENT.ID = M2_CONFLICT_WORK.CNFW_DA_ID
          INNER JOIN M2_WORK_TASK_UOM ON 
               M2_WORK_TASK.ID = M2_WORK_TASK_UOM.WTM_WT_ID
          INNER JOIN M2_UNIT_OF_MEASURE ON 
               M2_UNIT_OF_MEASURE.ID = M2_WORK_TASK_UOM.WTM_UOM_ID
          INNER JOIN M2_WORK_TASK_COMPONENT ON 
               M2_WORK_TASK_COMPONENT.WTC_WT_ID = M2_WORK_TASK.ID
          INNER JOIN M2_WORK_TASK_COMP_UOM ON 
               M2_WORK_TASK_COMPONENT.ID = M2_WORK_TASK_COMP_UOM.WTCM_WTC_ID
          INNER JOIN M2_COMPONENT ON 
               M2_COMPONENT.ID = M2_WORK_TASK_COMPONENT.WTC_CMP_ID
          INNER JOIN M2_USE_TYPE ON 
               M2_USE_TYPE.ID = M2_WORK_TASK_COMPONENT.WTC_USET_ID
          INNER JOIN M2_PROPERTY ON 
               M2_PROPERTY.ID = M2_AGREEMENT_PROPERTY.AGRP_PRP_ID
          WHERE M2_DAMAGE_AGENT.ID = 8 AND
    M2_WORK_TASK.WT_WORK_DATE >= to_date(' 2001-01-01 ','yyyy-mm-dd') """
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names
    return df


def property(cursor):
    cursor.execute(
        """
          SELECT 
               M2_AGREEMENT_PROPERTY.AGRP_PRP_ID,
               M2_PROPERTY.PRP_NAME,
               M2_COUNTY.CNTY_NAME,
               M2_COUNTY.CNTY_GSA_CNTY_CD,
               M2_STATE.ST_NAME,
               M2_STATE.ST_GSA_STATE_CD,
               M2_PROPERTY_SIZE.PRPS_QTY,
               M2_UNIT_OF_MEASURE.UOM_NAME,
               M2_PROPERTY_SIZE.PRPS_PROP_TYPE,
               -- M2_PROPERTY_UOL.PRPU_N_LAT,
               -- M2_PROPERTY_UOL.PRPU_E_LONG,
               M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID,
               M2_ALLOWED_SPECIES.ALWS_DA_ID
          FROM 
               M2_PROPERTY
          INNER JOIN M2_AGREEMENT_PROPERTY ON 
               M2_PROPERTY.ID = M2_AGREEMENT_PROPERTY.AGRP_PRP_ID
          INNER JOIN M2_COUNTY ON 
               M2_COUNTY.ID = M2_PROPERTY.PRP_CNTY_ID
          INNER JOIN M2_STATE ON 
               M2_STATE.ID  = M2_PROPERTY.PRP_ST_ID
               AND M2_STATE.ID = M2_COUNTY.CNTY_ST_ID
          INNER JOIN M2_PROPERTY_SIZE ON 
               M2_PROPERTY.ID = M2_PROPERTY_SIZE.PRPS_PRP_ID
          -- INNER JOIN M2_PROPERTY_UOL ON 
               -- M2_PROPERTY.ID = M2_PROPERTY_UOL.PRPU_PRP_ID
          INNER JOIN M2_UNIT_OF_MEASURE ON 
               M2_UNIT_OF_MEASURE.ID = M2_PROPERTY_SIZE.PRPS_UOM_ID
          INNER JOIN M2_ALLOWED_SPECIES ON 
               M2_AGREEMENT_PROPERTY.ID = M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID
          WHERE M2_ALLOWED_SPECIES.ALWS_DA_ID = 8"""
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names
    return df


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

        df = single(cursor)

        print("single data pulled successfully.")

        # write as csv
        df.to_csv(csv_name, index=False)

    # effort pull
    csv_name = os.path.join(file_path, "fs_national_effort.csv")

    if os.path.exists(csv_name) and not always_pull:
        print(f"{csv_name} already exists, skipping effort pull.")
    else:
        print("Pulling effort data...")

        df = effort(cursor)

        print("Effort data pulled successfully.")

        # write as csv
        df.to_csv(csv_name, index=False)

    # property pull
    csv_name = os.path.join(file_path, "fs_national_property.csv")

    if os.path.exists(csv_name) and not always_pull:
        print(f"{csv_name} already exists, skipping property pull.")
    else:
        print("Pulling property data...")

        df = property(cursor)

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
