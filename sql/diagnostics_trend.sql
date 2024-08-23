/*___________  _______    _______  _____  ___   ________    
 ("     _   ")/"      \  /"     "|(\"   \|"  \ |"      "\  
 )__/  \\__/|:        |(: ______)|.\\   \    |(.  ___  :)
    \\_ /   |_____/   ) \/    |  |: \.   \\  ||: \   ) || 
    |.  |    //      /  // ___)_ |.  \    \. |(| (___\ ||   
    \:  |   |:  __   \ (:      "||    \    \ ||:       :)  
     \__|   |__|  \___) \_______) \___|\____\)(________/  
*/

-- README
-- Creates a table with counts of ED investigations, by provider, 
-- for a 12 year period (2012/13 - 2023/24). From record-level  
-- data in AEA and EC datasets within NHSE_SUSPlus_Live db in NCDR. 
-- Note: 24 is max for recorded investigations.
 
 
SELECT * INTO [NHSE_Sandbox_StrategyUnit].[dbo].[2232_diagnostics_trend]
FROM (
--  __ _  ___  __ _ 
-- / _` |/ _ \/ _` |
--| (_| |  __/ (_| |
-- \__,_|\___|\__,_|
-- 
        SELECT 'aea' as data_source,
            aea.Der_Financial_Year as fyear,
            -- AEA_Department_Type,
            CASE
                WHEN AEA_Arrival_Mode = 1 THEN 'amb'
                WHEN AEA_Arrival_Mode = 2 THEN 'walk_in'
                ELSE 'NA'
            END AS arr_mode,
            Der_Number_Investigation AS n_invst,
            n_invst_ec = NULL,
            Der_Investigation_All,
            Der_EC_Investigation_All = NULL,
            aea.AEA_Attendance_Disposal AS disdest,
            -- NOTE: NOT QUITE THE SAME AS EC DISCHARGE DESTINATION:
            AEA_Attendance_Disposal_Desc_Short AS disdest_desc,
            CASE
                WHEN aea.AEA_Attendance_Disposal IN ('02', '03', '11') THEN 'discharged'
                WHEN aea.AEA_Attendance_Disposal IN ('04', '05', '06') THEN 'ambulatory'
                WHEN aea.AEA_Attendance_Disposal = '01' THEN 'admitted'
                WHEN aea.AEA_Attendance_Disposal = '07' THEN 'transfer'
                ELSE 'other'
            END AS disdest_grp,
            LEFT(Provider_Code, 3) AS procode,
            COUNT(*) AS n
        FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_AEA aea
            LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_AEA_AttendanceDisposal] ref_disposal ON aea.[AEA_Attendance_Disposal] = ref_disposal.AEA_Attendance_Disposal
        WHERE Der_Financial_Year IN (
                '2012/13',
                '2013/14',
                '2014/15',
                '2015/16',
                '2016/17',
                '2017/18',
                '2018/19'
            )
            AND -- EXCLUSIONS MIRRORING SW PREVIOUS WORK:  (BUT ONLY DEPARTMENT TYPE 1)
            AEA_Department_Type IN ('01')
            -- CAN REMOVE ARRIVAL MODE CLAUSE FOR TIMESERIES WORK:
            --AND -- ARRIVAL MODE KNOWN (AMBULANCE OR OTHER):
            --AEA_Arrival_Mode IN ('1', '2')
            AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
            AEA_Attendance_Category = '1'
            AND -- NOT BROUGHT IN DEAD:
            (NOT AEA_Patient_Group = '70')
            AND -- DURING ATTENDANCE DID NOT DIE / LEAVE / OTHER DISPOSAL:
            (
                NOT (
                    aea.AEA_Attendance_Disposal IN ('10', '12', '13', '99')
                    OR aea.AEA_Attendance_Disposal IS NULL
                )
            )
            AND SEX IN ('1', '2') 
            -- EQUIVALENTS TO PS CLAUSES FROM INDUSTRIAL ACTION CODE:
            -- AND Der_Dupe_Flag = 0 -- NOT IN DATA
            AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
            AND LEFT(Provider_Code, 1) = 'R'

        GROUP BY aea.Der_Financial_Year,
            AEA_Department_Type,
            CASE
                WHEN AEA_Arrival_Mode = 1 THEN 'amb'
                WHEN AEA_Arrival_Mode = 2 THEN 'walk_in'
                ELSE 'NA'
            END,
            Der_Number_Investigation,
            Der_Investigation_All,
            aea.AEA_Attendance_Disposal,
            AEA_Attendance_Disposal_Desc_Short,
            CASE
                WHEN aea.AEA_Attendance_Disposal IN ('02', '03', '11') THEN 'discharged'
                WHEN aea.AEA_Attendance_Disposal IN ('04', '05', '06') THEN 'ambulatory'
                WHEN aea.AEA_Attendance_Disposal = '01' THEN 'admitted'
                WHEN aea.AEA_Attendance_Disposal = '07' THEN 'transfer'
                ELSE 'other'
            END,
            LEFT(Provider_Code, 3)

        UNION ALL

--  ___  ___ __| |___ 
-- / _ \/ __/ _` / __|
--|  __/ (_| (_| \__ \
-- \___|\___\__,_|___/
--
        SELECT 'ecds' as data_source,
            ec.Der_Financial_Year as fyear,
            -- EC_Department_Type,
            CASE
                WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
                WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
                WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
                ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
            END AS arr_mode,
            Der_Number_AEA_Investigation AS n_invst,
            Der_Number_EC_Investigation AS n_invst_ec,
            Der_AEA_Investigation_All,
            Der_EC_Investigation_All,
            Discharge_Destination_SNOMED_CT AS disdest,
            ref_dis_dest.DischargeDestinationDescription AS disdest_desc,
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
            Provider_Code AS procode,
            COUNT(*) AS n
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
            AND -- EXCLUSIONS MIRRORING SW PREVIOUS WORK (ECDS EQUIVALENTS TO AEA): 
            EC_Department_Type IN ('01')
            -- CAN REMOVE ARRIVAL MODE CLAUSE FOR TIMESERIES WORK:
            -- AND -- ARRIVAL MODE KNOWN (AMBULANCE OR, IN ECDS, VARIOUS NAMED OTHERS) :
            -- (NOT ArrivalModeDescription IS NULL)
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

        GROUP BY ec.Der_Financial_Year,
            EC_Department_Type,
            CASE
                WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
                WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
                WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
                ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
            END,
            Der_Number_AEA_Investigation,
            Der_Number_EC_Investigation,
            Der_AEA_Investigation_All,
            Der_EC_Investigation_All,
            Discharge_Destination_SNOMED_CT,
            ref_dis_dest.DischargeDestinationDescription,
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
            END,
            Provider_Code
    ) TB1
