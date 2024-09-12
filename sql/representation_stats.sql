-- README
-- How representative are the provider samples used in our analyses?
-- The table output of this query can be used to answer the question 
-- above for both the trends work (using the table as is) and for 
-- the modelling work (by setting na_arrival = 0).
-- Note: We use only ECDS (19/20 to 23/24) to establish representation.

--  ___  ___ __| |___ 
-- / _ \/ __/ _` / __|
--|  __/ (_| (_| \__ \
-- \___|\___\__,_|___/
--

SELECT 'ecds' as data_source,
    CASE
        WHEN ArrivalModeDescription IS NULL THEN 1
        ELSE 0
    END AS na_arrival,
    ec.Der_Financial_Year as fyear,
    LEFT(Provider_Code, 3) AS procode,
    -- EC_Department_Type,
    CASE
        WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 1
        ELSE 0 
    END AS is_ambulance,
    CASE
        WHEN Discharge_Destination_SNOMED_CT IN (
		'306706006',
		'1874161000000104',
		'1066361000000104',
		'1066371000000106', 
        '1066381000000108', 
        '1066391000000105', 
        '1066401000000108'
		) THEN 1
        ELSE 0
    END AS is_adm,
	CASE 
	    WHEN Der_Age_At_CDS_Activity_Date < 18 THEN 1
		ELSE 0 
	END AS age_under_18,
	CASE 
	    WHEN Der_Age_At_CDS_Activity_Date > 74 THEN 1
		ELSE 0 
	END AS age_75_plus,
	CASE 
	    WHEN Index_Of_Multiple_Deprivation_Decile IN (1,2) THEN 1
		ELSE 0
	END AS imd_quint_1,
	CASE 
	    WHEN Rural_Urban_Indicator IN ('1', '5') THEN 1
		ELSE 0
	END AS is_urban,
    COUNT(*) AS n
INTO [NHSE_Sandbox_StrategyUnit].[dbo].[2232_diagnostics_provider_representation_stats]
FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
    LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode
WHERE ec.Der_Financial_Year IN (
        '2019/20',
        '2020/21',
        '2021/22',
        '2022/23',
        '2023/24'
    )
    AND -- EXCLUSIONS MIRRORING SW'S PREVIOUS WORK (ECDS EQUIVALENTS TO AEA): 
    EC_Department_Type IN ('01')
    -- SEE FLAG na_arrival COLUMN:
    -- AND -- ARRIVAL MODE KNOWN (AMBULANCE OR, IN ECDS, VARIOUS NAMED OTHERS) :
    -- (NOT ArrivalModeDescription IS NULL)
    AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
    EC_AttendanceCategory = '1'
    AND -- NOT BROUGHT IN DEAD OR DIED DURING ATTENDANCE:
      (
      NOT (Der_AEA_Patient_Group = '70' OR Discharge_Destination_SNOMED_CT = '305398007') -- died
      )
    AND -- DURING ATTENDANCE DID NOT LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
      (
      DischargeStatusDescription = 'Treatment completed (situation)'
      OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)'
      )
    AND SEX IN ('1', '2') 
    -- FROM PS'S INDUSTRIAL ACTION CODE:
    AND Der_Dupe_Flag = 0
    AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
    AND LEFT(Provider_Code, 1) = 'R'

GROUP BY  
    CASE
        WHEN ArrivalModeDescription IS NULL THEN 1
        ELSE 0
    END,
    ec.Der_Financial_Year,
    LEFT(Provider_Code, 3),
    -- EC_Department_Type,
    CASE
        WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 1
        ELSE 0 
    END,
    CASE
        WHEN Discharge_Destination_SNOMED_CT IN (
		'306706006',
		'1874161000000104',
		'1066361000000104',
		'1066371000000106', 
        '1066381000000108', 
        '1066391000000105', 
        '1066401000000108'
		) THEN 1
        ELSE 0
    END,
	CASE 
	    WHEN Der_Age_At_CDS_Activity_Date < 18 THEN 1
		ELSE 0 
	END,
	CASE 
	    WHEN Der_Age_At_CDS_Activity_Date > 74 THEN 1
		ELSE 0 
	END,
	CASE 
	    WHEN Index_Of_Multiple_Deprivation_Decile IN (1,2) THEN 1
		ELSE 0
	END,
	CASE 
	    WHEN Rural_Urban_Indicator IN ('1', '5') THEN 1
		ELSE 0
	END 