# mis queries

import pandas as pd
import oracledb


def effort(cursor, startDate):
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
          WHERE M2_DAMAGE_AGENT.ID = 8
               AND M2_WORK_TASK.WT_WORK_DATE >= TO_DATE('"""
        + startDate
        + """', 'YYYY-MM-DD')"""
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names
    return df


def take_by_method(cursor, startDate):
    cursor.execute(
        """
          SELECT 
               M2_AGREEMENT_PROPERTY.AGRP_PRP_ID,
               M2_WORK_TASK.WT_AGRPROP_ID,
               M2_WORK_TASK_UOM.WTM_WT_ID,
               M2_WORK_TASK.WT_WORK_DATE,
               M2_DAMAGE_AGENT.ID,
               M2_DAMAGE_AGENT.DA_NAME,
               M2_WORK_TASK_COMP_UOM.WTCM_QTY,
               M2_USE_TYPE.USET_NAME,
               M2_COMPONENT.CMP_NAME,
               M2_FATE.FATE_WKR_ID,
               M2_WORK_RESULT.WKR_QTY,
               M2_WORK_RESULT.WKR_INTENTIONAL,
               M2_WORK_RESULT.WKR_TARGET
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
          INNER JOIN M2_FATE ON 
               M2_WORK_TASK.ID = M2_FATE.FATE_WT_ID
          INNER JOIN M2_WORK_TASK_COMPONENT ON 
               M2_WORK_TASK_COMPONENT.ID = M2_FATE.FATE_WTC_ID
          INNER JOIN M2_WORK_TASK_COMP_UOM ON 
               M2_WORK_TASK_COMPONENT.ID = M2_WORK_TASK_COMP_UOM.WTCM_WTC_ID
          INNER JOIN M2_COMPONENT ON 
               M2_COMPONENT.ID = M2_WORK_TASK_COMPONENT.WTC_CMP_ID
          INNER JOIN M2_USE_TYPE ON 
               M2_USE_TYPE.ID = M2_WORK_TASK_COMPONENT.WTC_USET_ID
          INNER JOIN M2_WORK_RESULT ON 
               M2_WORK_RESULT.ID = M2_FATE.FATE_WKR_ID
          WHERE M2_DAMAGE_AGENT.ID = 8
               AND M2_WORK_TASK.WT_WORK_DATE >= TO_DATE('"""
        + startDate
        + """', 'YYYY-MM-DD')"""
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names
    return df


def take_by_property(cursor, startDate):
    cursor.execute(
        """
          SELECT 
               M2_AGREEMENT_PROPERTY.AGRP_PRP_ID,
               M2_PROPERTY.PRP_NAME,
               M2_WORK_TASK.WT_AGRPROP_ID,
               M2_COUNTY.CNTY_NAME,
               M2_COUNTY.CNTY_GSA_CNTY_CD,
               M2_STATE.ST_NAME,
               M2_STATE.ST_GSA_STATE_CD,
               M2_PROPERTY_SIZE.PRPS_QTY,
               M2_UNIT_OF_MEASURE.UOM_NAME,
               M2_PROPERTY_SIZE.PRPS_PROP_TYPE,
               M2_PROPERTY_UOL.PRPU_N_LAT,
               M2_PROPERTY_UOL.PRPU_E_LONG,
               M2_WORK_TASK_COMPONENT.WTC_WT_ID,
               M2_WORK_TASK.WT_WORK_DATE,
               M2_WORK_TASK_UOM.WTM_QTY,
               M2_UNIT_OF_MEASURE1.UOM_NAME AS UOM_NAME1,
               M2_FATE.FATE_FATE,
               M2_COMPONENT.CMP_NAME,
               M2_WORK_TASK_COMP_UOM.WTCM_QTY,
               M2_FATE.FATE_WKR_ID,
               M2_WORK_RESULT.WKR_QTY,
               M2_WORK_RESULT.WKR_INTENTIONAL,
               M2_WORK_RESULT.WKR_TARGET,
               M2_DAMAGE_AGENT.DA_NAME,
               M2_DAMAGE_AGENT.ID
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
          INNER JOIN M2_PROPERTY_UOL ON 
               M2_PROPERTY.ID = M2_PROPERTY_UOL.PRPU_PRP_ID
          INNER JOIN M2_WORK_TASK ON 
               M2_AGREEMENT_PROPERTY.ID = M2_WORK_TASK.WT_AGRPROP_ID
          INNER JOIN M2_FATE ON 
               M2_WORK_TASK.ID = M2_FATE.FATE_WT_ID
          INNER JOIN M2_WORK_TASK_UOM ON 
               M2_WORK_TASK.ID = M2_WORK_TASK_UOM.WTM_WT_ID
          INNER JOIN M2_UNIT_OF_MEASURE ON 
               M2_UNIT_OF_MEASURE.ID = M2_PROPERTY_SIZE.PRPS_UOM_ID
          INNER JOIN M2_WORK_RESULT ON 
               M2_WORK_RESULT.ID = M2_FATE.FATE_WKR_ID
          INNER JOIN M2_DAMAGE_AGENT ON 
               M2_DAMAGE_AGENT.ID = M2_WORK_RESULT.WKR_DA_ID
          INNER JOIN M2_WORK_TASK_COMPONENT ON 
               M2_WORK_TASK_COMPONENT.ID = M2_FATE.FATE_WTC_ID
          INNER JOIN M2_WORK_TASK_COMP_UOM ON 
               M2_WORK_TASK_COMPONENT.ID = M2_WORK_TASK_COMP_UOM.WTCM_WTC_ID
          INNER JOIN M2_COMPONENT ON 
               M2_COMPONENT.ID = M2_WORK_TASK_COMPONENT.WTC_CMP_ID
          INNER JOIN M2_UNIT_OF_MEASURE M2_UNIT_OF_MEASURE1 ON 
               M2_UNIT_OF_MEASURE1.ID = M2_WORK_TASK_UOM.WTM_UOM_ID
          WHERE M2_DAMAGE_AGENT.ID  = 8
               AND M2_WORK_TASK.WT_WORK_DATE >= TO_DATE('"""
        + startDate
        + """', 'YYYY-MM-DD')"""
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names
    return df


def property(cursor, startDate):
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
               M2_PROPERTY_UOL.PRPU_N_LAT,
               M2_PROPERTY_UOL.PRPU_E_LONG,
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
          INNER JOIN M2_PROPERTY_UOL ON 
               M2_PROPERTY.ID = M2_PROPERTY_UOL.PRPU_PRP_ID
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


def damage(cursor, startDate):
    cursor.execute(
        """
          SELECT 
               M2_AGREEMENT_PROPERTY.AGRP_PRP_ID,
               M2_WORK_TASK.WT_AGRPROP_ID,
               M2_PROPERTY.PRP_NAME,
               M2_COUNTY.CNTY_NAME,
               M2_COUNTY.CNTY_GSA_CNTY_CD,
               M2_STATE.ST_NAME,
               M2_STATE.ST_GSA_STATE_CD,
               M2_DAMAGE_THREAT.DMGT_NAME,
               M2_RESRC.RES_NAME,
               M2_RESRC.RES_TYPE,
               M2_DAMAGE_AGENT.DA_NAME,
               M2_DAMAGE_AGENT.ID
          FROM 
               M2_PROPERTY
          INNER JOIN M2_STATE ON 
               M2_STATE.ID = M2_PROPERTY.PRP_ST_ID
          INNER JOIN M2_COUNTY ON 
               M2_COUNTY.ID = M2_PROPERTY.PRP_CNTY_ID
               AND M2_STATE.ID = M2_COUNTY.CNTY_ST_ID
          INNER JOIN M2_AGREEMENT_PROPERTY ON 
               M2_PROPERTY.ID = M2_AGREEMENT_PROPERTY.AGRP_PRP_ID
          INNER JOIN M2_WORK_TASK ON 
               M2_AGREEMENT_PROPERTY.ID = M2_WORK_TASK.WT_AGRPROP_ID
          INNER JOIN M2_CONFLICT_WORK ON 
               M2_WORK_TASK.ID = M2_CONFLICT_WORK.CNFW_WT_ID
          INNER JOIN M2_RESRC ON 
               M2_RESRC.ID = M2_CONFLICT_WORK.CNFW_RES_ID
          INNER JOIN M2_DAMAGE_AGENT ON 
               M2_DAMAGE_AGENT.ID = M2_CONFLICT_WORK.CNFW_DA_ID
          INNER JOIN M2_DAMAGE_THREAT ON 
               M2_DAMAGE_THREAT.ID = M2_CONFLICT_WORK.CNFW_DMGT_ID
          WHERE M2_DAMAGE_AGENT.ID = 8
               AND M2_WORK_TASK.WT_WORK_DATE >= TO_DATE('"""
        + startDate
        + """', 'YYYY-MM-DD')"""
    )

    col_names = [row[0] for row in cursor.description]

    df = pd.DataFrame(cursor)
    df.columns = col_names
    return df
