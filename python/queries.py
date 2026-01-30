# mis queries

import pandas as pd
import oracledb


def single(cursor):
    cursor.execute(
        """
     select m2_work_task.id as wt_id,
    M2_STATE.ST_ABBR,
    M2_STATE.ST_NAME,
    m2_county.cnty_name,
    M2_COUNTY.CNTY_GSA_CNTY_CD,
    M2_STATE.ST_GSA_STATE_CD,
    m2_property.prp_name,
    M2_PROPERTY_UOL.PRPU_N_LAT,
    M2_PROPERTY_UOL.PRPU_E_LONG,
    M2_PROPERTY_SIZE.PRPS_QTY,
    M2_PROPERTY_SIZE.PRPS_PROP_TYPE,
    M2_AGREEMENT_PROPERTY.AGRP_PRP_ID,
    M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID AS ALWS_AGRPROP_ID,    -- Explicit alias
    M2_ALLOWED_SPECIES.ALWS_DA_ID AS ALWS_DA_ID,              -- Explicit alias
    m2_project.proj_name,
    m2_work_task.wt_work_date,
    m2_employee.emp_last_name,
    m2_employee.emp_first_name,
    m2_component.cmp_name,
    m2_work_task_comp_uom.wtcm_qty as CMP_QTY,
    M2_USE_TYPE.USET_NAME as USE_TYPE,
    m2_fate.fate_fate,
    M2_FATE.FATE_WKR_ID,
    m2_work_result.wkr_qty as TAKE,
    M2_WORK_RESULT.WKR_INTENTIONAL,
    M2_WORK_RESULT.WKR_TARGET,
    M2_WORK_TASK.WT_ENTRY_DATE,
    M2_WORK_TASK_COMPONENT.WTC_WT_ID,
    M2_WORK_TASK_UOM.WTM_QTY,
    M2_UNIT_OF_MEASURE1.UOM_NAME AS UOM_NAME1,
    m2_damage_agent.da_name,
    M2_DAMAGE_AGENT.ID
from m2_work_task
    INNER JOIN M2_WORK_TASK_COMPONENT
        ON M2_WORK_TASK_COMPONENT.WTC_WT_ID = M2_WORK_TASK.ID
    inner join m2_component
        on m2_work_task_component.wtc_cmp_id=m2_component.id
    inner join m2_fate
        on m2_fate.fate_wtc_id = m2_work_task_component.id
    inner join m2_work_result
        on m2_work_result.id = m2_fate.fate_wkr_id
    inner join m2_damage_agent
        on m2_damage_agent.id = m2_work_result.wkr_da_id
    left join m2_work_project
        on m2_work_task.id=m2_work_project.wkpr_wt_id
    left join m2_project
        on m2_work_project.wkpr_proj_id = m2_project.id
    left join m2_work_task_comp_uom
        on m2_work_task_component.id = m2_work_task_comp_uom.wtcm_wtc_id
    LEFT JOIN M2_WORK_TASK_UOM
        ON M2_WORK_TASK.ID = M2_WORK_TASK_UOM.WTM_WT_ID
    LEFT JOIN M2_AGREEMENT_PROPERTY
        ON M2_AGREEMENT_PROPERTY.ID = M2_WORK_TASK.WT_AGRPROP_ID
    LEFT JOIN M2_PROPERTY
        ON M2_PROPERTY.ID = M2_AGREEMENT_PROPERTY.AGRP_PRP_ID
    LEFT JOIN M2_PROPERTY_UOL
        ON M2_PROPERTY_UOL.PRPU_PRP_ID = M2_PROPERTY.ID
    LEFT JOIN M2_PROPERTY_SIZE
        ON M2_PROPERTY.ID = M2_PROPERTY_SIZE.PRPS_PRP_ID
    LEFT JOIN M2_ALLOWED_SPECIES
        ON M2_AGREEMENT_PROPERTY.ID = M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID
        AND M2_ALLOWED_SPECIES.ALWS_DA_ID = 8                 -- Filter for damage agent 8
    left join m2_county
        on m2_county.id = m2_property.prp_cnty_id
    LEFT JOIN M2_STATE
        ON M2_STATE.ID = M2_PROPERTY.PRP_ST_ID
        AND M2_STATE.ID = M2_COUNTY.CNTY_ST_ID
    LEFT JOIN M2_UNIT_OF_MEASURE
        ON M2_UNIT_OF_MEASURE.ID = M2_PROPERTY_SIZE.PRPS_UOM_ID
    LEFT JOIN M2_UNIT_OF_MEASURE M2_UNIT_OF_MEASURE1
        ON M2_UNIT_OF_MEASURE1.ID = M2_WORK_TASK_UOM.WTM_UOM_ID
    LEFT JOIN M2_USE_TYPE
        ON M2_USE_TYPE.ID = M2_WORK_TASK_COMPONENT.WTC_USET_ID
    left join m2_employee
        on m2_employee.id = m2_work_task.wt_emp_id
WHERE M2_DAMAGE_AGENT.ID = 8 AND
    M2_FATE.FATE_FATE = 'KILLED' and
    m2_work_task.wt_work_date >= to_date(' 2001-01-01 ','yyyy-mm-dd') and
    (M2_PROJECT.PROJ_NAME IN ('') OR M2_PROJECT.PROJ_NAME IS NULL)
          
          """
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names

    return df
