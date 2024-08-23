/*_______   ______   ________    ________     
 /"     "| /" _  "\ |"      "\  /"       )    
(: ______)(: ( \___)(.  ___  :)(:   \___/     
 \/ ___|  |  |      |: \   ) || \___  \       
 // ___)_ |  |   _  (| (___\ ||  __/  \\      
(:      "|(: | _) \ |:       :) /" \   :)     
 \_______) \_______)(________/ (_______/                                                                                      
    ______    _____  ___   ___       ___  ___  
   /    " \  (\"   \|"  \ |"  |     |"  \/"  | 
  // ____  \ |.\\   \    |||  |      \   \  /  
 /  /    ) :)|: \.   \\  ||:  |       \\  \/   
(: (____/ // |.  \    \. | \  |___    /   /    
 \        /  |    \    \ |( \_|:  \  /   /     
  \"_____/    \___|\____\) \_______)|___/             
*/

-- README
-- Creates a table containing a subset of records and variables
-- that may be useful for the modelling elements of work package 1.
-- Only years 2019/20 and 2023/24 are required here. From record-level
-- data in AEA and EC datasets within NHSE_SUSPlus_Live db in NCDR. 
-- Note: 24 is max for recorded investigations.

SELECT 'ecds' as data_source,

/* TIME-RELATED VARIABLES */

	  ec.Der_Financial_Year as fyear, 
    -- ec.EC_Ident AS id,
    Der_EC_Arrival_Date_Time AS dttm_arr,
    -- Arrival_Date,
    EC_Initial_Assessment_Time AS time_assess,
    EC_Seen_For_Treatment_Time AS time_treat,
    EC_Conclusion_Time AS time_concl,
    -- convert(DATE, EC_Departure_Date) AS date_depart,
	  -- EC_Departure_Time AS time_depart,
    Der_EC_Departure_Date_Time AS dttm_depart,
	  -- THIS MAY BE DIFFERENT FROM THE ABOVE:
    --cast(EC_Departure_Date AS DATETIME) + cast(EC_Departure_Time AS DATETIME) AS depart_dttm_manual,
    CAST(EC_Initial_Assessment_Time_Since_Arrival AS INT) AS dur_arr_assess,
    CAST(EC_Seen_For_Treatment_Time_Since_Arrival AS INT) AS dur_arr_treat,
    CAST(EC_Conclusion_Time_Since_Arrival AS INT) AS dur_arr_concl,
    CASE
        WHEN Der_EC_Duration < 0 THEN NULL -- 4320 mins = 72 hours = 3 days
        WHEN Der_EC_Duration > 4320 THEN NULL
        ELSE Der_EC_Duration
    END AS duration_ed,

/* ARRIVAL/DISCHARGE VARIABLES */

    -- ONLY DEP TYPE 1 IN WHERE CLAUSE, SO UNNECESSARY:
    -- EC_Department_Type,
    EC_Attendance_Source_SNOMED_CT AS att_source,
    ref_attsrc.AttendanceSourceDescription AS att_source_desc,
    CASE
        WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
        WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
        WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
        ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
    END AS arr_mode,
    Discharge_Destination_SNOMED_CT AS disdest,
    ref_dis_dest.DischargeDestinationDescription AS disdest_desc,
    -- see ECDS_Group1 field, tab 26.4, ECDS_ETOS_v4.0.7
    CASE
        WHEN Discharge_Destination_SNOMED_CT = '306689006' THEN 'discharged'
        WHEN Discharge_Destination_SNOMED_CT = '306691003' THEN 'discharged'
        WHEN Discharge_Destination_SNOMED_CT = '306694006' THEN 'discharged'
        WHEN Discharge_Destination_SNOMED_CT = '306705005' THEN 'discharged'
        WHEN Discharge_Destination_SNOMED_CT = '50861005' THEN 'discharged'
        WHEN Discharge_Destination_SNOMED_CT = '1066331000000109' THEN 'ambulatory'
        WHEN Discharge_Destination_SNOMED_CT = '1066341000000100' THEN 'ambulatory'
        WHEN Discharge_Destination_SNOMED_CT = '1066351000000102' THEN 'ambulatory'
        WHEN Discharge_Destination_SNOMED_CT = '306706006' THEN 'admitted'
        WHEN Discharge_Destination_SNOMED_CT = '1874161000000104' THEN 'admitted'
        WHEN Discharge_Destination_SNOMED_CT = '1066361000000104' THEN 'admitted'
        WHEN Discharge_Destination_SNOMED_CT = '1066371000000106' THEN 'admitted'
        WHEN Discharge_Destination_SNOMED_CT = '1066381000000108' THEN 'admitted'
        WHEN Discharge_Destination_SNOMED_CT = '1066391000000105' THEN 'admitted'
        WHEN Discharge_Destination_SNOMED_CT = '1066401000000108' THEN 'admitted'
        WHEN Discharge_Destination_SNOMED_CT = '19712007' THEN 'transfer'
        WHEN Discharge_Destination_SNOMED_CT = '183919006' THEN 'transfer'
        WHEN Discharge_Destination_SNOMED_CT = '305398007' THEN 'died'
        ELSE Discharge_Destination_SNOMED_CT
    END AS disdest_grp,
    EC_Discharge_Status_SNOMED_CT AS dis_status,
    ref_dis_stat.DischargeStatusDescription AS dis_status_desc,

/* CARE-RELATED (INVEST-DIAG-TREAT) VARIABLES */

    -- EC_Acuity_SNOMED_CT AS acuity_code, 
    ref_acuity.AcuityID AS acuity, -- 5 ACUITY LEVELS
    ref_acuity.AcuityDescription AS acuity_desc,
    ec.EC_Chief_Complaint_SNOMED_CT AS chief_comp, -- 149 DISTINCT CHIEF COMPLAINTS
    ref_chief_comp.ChiefComplaintDescription AS chief_comp_desc,
	  Clinical_Chief_Complaint_Code_Approved,
    ref_chief_comp_grp.ChiefComplaintGrouping AS chief_comp_grp, -- 15 CHIEF COMPLAINT GROUPS
    ec_diag.EC_Diagnosis_01 AS diag01_ec,
    ref_diag.DiagnosisDescription AS diag01_ec_desc,
	  -- DERIVED FROM CHIEF COMPLAINT BUT MAY STILL BE USEFUL IF USING COMPLAINT GROUP
    Clinical_Chief_Complaint_Injury_Related AS inj_flag,
  	Der_Number_EC_Investigation AS n_invst_ec,
    Der_Number_EC_Diagnosis AS n_diag_ec,
    Der_Number_EC_Treatment AS n_treat_ec,
	  -- MAY NOT BE ALL THAT WELL RECORDED (?):
    CASE
        WHEN ec_comorb.Comorbidity_01 IS NOT NULL THEN 1
        ELSE 0
    END + CASE
        WHEN ec_comorb.Comorbidity_02 IS NOT NULL THEN 1
        ELSE 0
    END + CASE
        WHEN ec_comorb.Comorbidity_03 IS NOT NULL THEN 1
        ELSE 0
    END + CASE
        WHEN ec_comorb.Comorbidity_04 IS NOT NULL THEN 1
        ELSE 0
	  END + CASE
        WHEN ec_comorb.Comorbidity_05 IS NOT NULL THEN 1
        ELSE 0
	  END + CASE
        WHEN ec_comorb.Comorbidity_06 IS NOT NULL THEN 1
        ELSE 0
	END AS n_cmrbd,
	Der_EC_Diagnosis_All,
	Der_EC_Investigation_All,
	Der_EC_Treatment_All,
   
/* DEMOGRAPHIC VARIABLES */

    CASE
        WHEN Sex = '1' THEN 'm'
        WHEN Sex = '2' THEN 'f'
        WHEN Sex IN ('0', '9', 'X') THEN 'NA'
        ELSE Sex
    END AS sex,
    CASE
        WHEN Age_at_Arrival >= 110 THEN NULL
        ELSE Age_At_Arrival
    END AS age,
    CASE
        WHEN Age_at_Arrival < 10 THEN '00-09'
        WHEN Age_at_Arrival >= 10
        AND Age_at_Arrival < 20 THEN '10-19'
        WHEN Age_at_Arrival >= 20
        AND Age_at_Arrival < 30 THEN '20-29'
        WHEN Age_at_Arrival >= 30
        AND Age_at_Arrival < 40 THEN '30-39'
        WHEN Age_at_Arrival >= 40
        AND Age_at_Arrival < 50 THEN '40-49'
        WHEN Age_at_Arrival >= 50
        AND Age_at_Arrival < 60 THEN '50-59'
        WHEN Age_at_Arrival >= 60
        AND Age_at_Arrival < 70 THEN '60-69'
        WHEN Age_at_Arrival >= 70
        AND Age_at_Arrival < 80 THEN '70-79'
        WHEN Age_at_Arrival >= 80
        AND Age_at_Arrival < 90 THEN '80-89'
        WHEN Age_at_Arrival >= 90
        AND Age_at_Arrival < 110 THEN '90+'
        WHEN Age_At_Arrival >= 110 THEN 'NA'
    END AS age_grp,
    Index_Of_Multiple_Deprivation_Decile AS imd_dec,
    Ethnic_Category AS ethnic_grp,

/* GEOGRAPHICAL VARIABLES */

    Provider_Code AS procode,
    Der_Postcode_Dist_Unitary_Auth AS lacd,
    Government_Office_Region AS region,
    Der_Postcode_LSOA_2011_Code AS lsoa11cd

INTO [NHSE_Sandbox_StrategyUnit].[dbo].[2232_diagnostics_ecds_only]
FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Attendance_Source] ref_attsrc ON ec.EC_Attendance_Source_SNOMED_CT = ref_attsrc.AttendanceSourceCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Acuity] ref_acuity ON ec.EC_Acuity_SNOMED_CT = ref_acuity.AcuityCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint] ref_chief_comp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp.ChiefComplaintCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint_Group] ref_chief_comp_grp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp_grp.ChiefComplaintCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode
    LEFT OUTER JOIN [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SUS_EC_Diagnosis] ec_diag ON ec.EC_Ident = ec_diag.EC_Ident
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Diagnosis] ref_diag ON ec_diag.EC_Diagnosis_01 = ref_diag.DiagnosisCode
    LEFT OUTER JOIN [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SUS_EC_Comorbidities] ec_comorb ON ec_diag.EC_Ident = ec_comorb.EC_Ident
WHERE ec.Der_Financial_Year IN (
        '2019/20',
        --'2020/21',
        --'2021/22',
        --'2022/23',
        '2023/24'
    )
    AND -- EXCLUSIONS MIRRORING SW PREVIOUS WORK BUT TYPE 1s ONLY (ECDS EQUIVALENTS TO AEA): 
    EC_Department_Type IN ('01')
    AND -- ARRIVAL MODE KNOWN (AMBULANCE OR, IN ECDS, VARIOUS NAMED OTHERS) :
    (NOT ArrivalModeDescription IS NULL)
    AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
    EC_AttendanceCategory = '1'
    AND -- NOT BROUGHT IN DEAD:
    (NOT Der_AEA_Patient_Group = '70')
    AND -- DURING ATTENDANCE DID NOT: DIE / LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
    (
        DischargeStatusDescription = 'Treatment completed (situation)'
        OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)'
        OR Discharge_Destination_SNOMED_CT = '305398007' -- died
    )
    AND SEX IN ('1', '2') 
    -- FROM PS INDUSTRIAL ACTION CODE:
    AND Der_Dupe_Flag = 0
    AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
    AND LEFT(Provider_Code, 1) = 'R'


