# mis queries

import pandas as pd
import oracledb


def single(cursor):
    cursor.execute(
        """
     SELECT 
    m2_work_task.id as wt_id,
    
    -- Enhanced state/location info (from Amy's approach)
    M2_STATE.ST_ABBR,
    M2_STATE.ST_NAME,
    M2_COUNTY.CNTY_GSA_CNTY_CD,        -- Added from Amy's
    M2_STATE.ST_GSA_STATE_CD,          -- Added from Amy's
    m2_county.cnty_name,
    m2_property.prp_name,
    
    -- Property details (from Amy's approach)
    M2_PROPERTY_UOL.PRPU_N_LAT,        -- Added from Amy's
    M2_PROPERTY_UOL.PRPU_E_LONG,       -- Added from Amy's  
    M2_PROPERTY_SIZE.PRPS_QTY,         -- Added from Amy's
    M2_PROPERTY_SIZE.PRPS_PROP_TYPE,   -- Added from Amy's
    M2_UNIT_OF_MEASURE.UOM_NAME AS PROPERTY_SIZE_UOM,  -- Added from Amy's
    
    -- Agreement and allowed species (from Amy's approach)
    M2_AGREEMENT_PROPERTY.AGRP_PRP_ID, -- Added from Amy's
    M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID, -- Added from Amy's
    M2_ALLOWED_SPECIES.ALWS_DA_ID,     -- Added from Amy's
    
    -- Project and employee (Will's original)
    m2_project.proj_name,
    m2_employee.emp_last_name,
    m2_employee.emp_first_name,
    
    -- Work task details
    m2_work_task.wt_work_date,
    M2_WORK_TASK.WT_ENTRY_DATE,
    
    -- Enhanced work task UOM info (from Amy's approach)
    M2_WORK_TASK_UOM.WTM_QTY,          -- Added from Amy's
    M2_UNIT_OF_MEASURE1.UOM_NAME AS WORK_TASK_UOM,  -- Added from Amy's
    
    -- Component details (enhanced from Amy's approach)
    m2_component.cmp_name,
    M2_COMPONENT.CMP_TYPE,             -- Added from Amy's
    m2_work_task_comp_uom.wtcm_qty as CMP_QTY,
    M2_USE_TYPE.USET_NAME as USE_TYPE,
    
    -- Fate and results (Will's original)
    m2_fate.fate_fate,
    M2_FATE.FATE_WKR_ID,               -- Added from Amy's
    m2_work_result.wkr_qty as TAKE,
    M2_WORK_RESULT.WKR_INTENTIONAL,
    M2_WORK_RESULT.WKR_TARGET,
    
    -- Damage agent (enhanced)
    m2_damage_agent.da_name,
    M2_DAMAGE_AGENT.ID AS DAMAGE_AGENT_ID  -- Added explicit ID from Amy's

FROM m2_work_task
    -- Core work task relationships (Will's approach)
    INNER JOIN M2_WORK_TASK_COMPONENT
        ON M2_WORK_TASK_COMPONENT.WTC_WT_ID = M2_WORK_TASK.ID
    INNER JOIN m2_component
        ON m2_work_task_component.wtc_cmp_id = m2_component.id
    INNER JOIN m2_fate
        ON m2_fate.fate_wtc_id = m2_work_task_component.id
    INNER JOIN m2_work_result
        ON m2_work_result.id = m2_fate.fate_wkr_id
    INNER JOIN m2_damage_agent
        ON m2_damage_agent.id = m2_work_result.wkr_da_id
    
    -- Optional relationships (Will's approach)
    LEFT JOIN m2_work_project
        ON m2_work_task.id = m2_work_project.wkpr_wt_id
    LEFT JOIN m2_project
        ON m2_work_project.wkpr_proj_id = m2_project.id
    LEFT JOIN m2_work_task_comp_uom
        ON m2_work_task_component.id = m2_work_task_comp_uom.wtcm_wtc_id
    LEFT JOIN M2_USE_TYPE
        ON M2_USE_TYPE.ID = M2_WORK_TASK_COMPONENT.WTC_USET_ID
    LEFT JOIN m2_employee
        ON m2_employee.id = m2_work_task.wt_emp_id
    
    -- Property relationships (keeping Will's LEFT JOIN approach)
    LEFT JOIN M2_AGREEMENT_PROPERTY
        ON M2_AGREEMENT_PROPERTY.ID = M2_WORK_TASK.WT_AGRPROP_ID
    LEFT JOIN M2_PROPERTY
        ON M2_PROPERTY.ID = M2_AGREEMENT_PROPERTY.AGRP_PRP_ID
    LEFT JOIN M2_PROPERTY_UOL
        ON M2_PROPERTY_UOL.PRPU_PRP_ID = M2_PROPERTY.ID
    LEFT JOIN m2_county
        ON m2_county.id = m2_property.prp_cnty_id
    LEFT JOIN M2_STATE
        ON M2_STATE.ID = M2_PROPERTY.PRP_ST_ID
    
    -- Enhanced property details (added from Amy's approach)
    LEFT JOIN M2_PROPERTY_SIZE 
        ON M2_PROPERTY.ID = M2_PROPERTY_SIZE.PRPS_PRP_ID
    LEFT JOIN M2_UNIT_OF_MEASURE 
        ON M2_UNIT_OF_MEASURE.ID = M2_PROPERTY_SIZE.PRPS_UOM_ID
    LEFT JOIN M2_ALLOWED_SPECIES 
        ON M2_AGREEMENT_PROPERTY.ID = M2_ALLOWED_SPECIES.ALWS_AGRPROP_ID
        AND M2_ALLOWED_SPECIES.ALWS_DA_ID = 8  -- Filter for consistency
    
    -- Enhanced work task UOM (added from Amy's approach)
    LEFT JOIN M2_WORK_TASK_UOM 
        ON M2_WORK_TASK.ID = M2_WORK_TASK_UOM.WTM_WT_ID
    LEFT JOIN M2_UNIT_OF_MEASURE M2_UNIT_OF_MEASURE1 
        ON M2_UNIT_OF_MEASURE1.ID = M2_WORK_TASK_UOM.WTM_UOM_ID

WHERE M2_DAMAGE_AGENT.ID = 8 
    AND M2_FATE.FATE_FATE = 'KILLED' 
    AND m2_work_task.wt_work_date >= to_date(' 2001-01-01 ','yyyy-mm-dd')
    AND (M2_PROJECT.PROJ_NAME IN ('') OR M2_PROJECT.PROJ_NAME IS NULL)

          """
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names

    return df
