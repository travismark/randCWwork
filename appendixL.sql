##################### APPENDIX L
## AIR FORCE STANDARD ALGORITHMS
## L.1 The following algorithms were compiled based on DoD Standard P&M Terms 5000.40, AFI 10-602, AFLC P&M
## 2000 Action Plan, MIL-STD-721B, MIL-STD-1388-2B, AFM 66-267, AFMCI 66-15, AFR 57-9, and input from various MDD users

## 1) ABORT AIR
## if DEBRIEF DEVIATION CODE = "AA" OR "AI", then add "1" to AIR ABORT.
# unique sorties that are aborted in the air - group unique sorties and filter by air abort
# 29 out of 739 unique sorties, or about 4%
SELECT 
    COUNT(*) AS qtyAirAborts
FROM
    (SELECT 
        Sortie_Number, Sortie_Date, Deviation_Code
    FROM
        debrief
    WHERE
        Deviation_Code IN ('AA' , 'AI')
    GROUP BY Sortie_Number , Sortie_Date , Deviation_Code) AS uniqueAirAborts;

## 2) ABORT AIR RELATED MAINTENANCE ACTIONS
## if WHEN DISCOVERED CODE = "C" then add units to AIR ABORT RELATED MAINTENANCE ACTIONS.
# not sure what a maintenance action is - taking all rows in the oem data
# 8
SELECT 
    COUNT(*) AS qtyAirAbortMaintenanceActions
FROM
    on_equipment_maintenance
WHERE
    When_Discovered_Code LIKE 'c';
# is there perhaps a better way to classify air abort related maintenance actions? look in discrepancy narrative?


## 3) ABORT AIR RATE
## ( ABORT AIR / SORTIES FLOWN ) x 100
# sorties flown is count of unique sorties with nonzero total flight time field
# 29 air-aborted sorties out of 601 sorties flown is about 5%
SELECT 
    qtySortiesAirAborted / qtySortiesFlown as airAbortRate
FROM
    (SELECT 
        COUNT(*) AS qtySortiesAirAborted
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, Deviation_Code
    FROM
        debrief
    WHERE
        Deviation_Code IN ('AA' , 'AI')
    GROUP BY Sortie_Number , Sortie_Date , Deviation_Code) sortiesAirAborted) qty1,
    (SELECT 
        COUNT(*) AS qtySortiesFlown
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, SUM(flight_duration) fd
    FROM
        debrief
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) sortiesFlown) qty2;
    
## 4) ABORT GROUND
## if DEBRIEF DEVIATION CODE = “GA”, then add “1” to GROUND ABORT.
# unique sorties that are aborted on the ground - group unique sorties and filter by ground abort
# 46 out of 739 unique sorties, or about 6%
SELECT 
    COUNT(*) AS qtyGroundAborts
FROM
    (SELECT 
        Sortie_Number, Sortie_Date, Deviation_Code
    FROM
        debrief
    WHERE
        Deviation_Code like 'GA'
    GROUP BY Sortie_Number , Sortie_Date , Deviation_Code) AS uniqueGroundAborts;
    
## 5) ABORT GROUND RELATED MAINTENANCE ACTIONS
## if WHEN DISCOVERED CODE = “A” then add UNITS to GROUND ABORT RELATED MAINTENANCE ACTIONS.
# not sure what a maintenance action is - taking all rows in the oem data
# 100
SELECT 
    COUNT(*) AS qtyGroundAbortMaintenanceActions
FROM
    on_equipment_maintenance
WHERE
    When_Discovered_Code LIKE 'a';
    
## 6) ABORT GROUND RATE
## ( ABORT GROUND / [SORTIES FLOWN + ABORT GROUND]) x 100
# 46 / (601 + 46) or about 7%
SELECT 
    qtySortiesGroundAborted / (qtySortiesFlown + qtySortiesGroundAborted) AS groundAbortRate
FROM
    (SELECT 
        COUNT(*) AS qtySortiesGroundAborted
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, Deviation_Code
    FROM
        debrief
    WHERE
        Deviation_Code LIKE 'GA'
    GROUP BY Sortie_Number , Sortie_Date , Deviation_Code) sortiesGroundAborted) qty1,
    (SELECT 
        COUNT(*) AS qtySortiesFlown
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, SUM(flight_duration) fd
    FROM
        debrief
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) sortiesFlown) qty2;
    
## 7) ABORT TOTAL RATE
## ( [ABORT AIR + ABORT GROUND] / [SORTIES FLOWN + ABORT GROUND]) x 100
# (29 + 46) / (601 + 46) or about 12%

SELECT 
    (qtySortiesAirAborted + qtySortiesGroundAborted) / (qtySortiesFlown + qtySortiesGroundAborted) AS totalAbortRate
FROM
    (SELECT 
        COUNT(*) AS qtySortiesGroundAborted
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, Deviation_Code
    FROM
        debrief
    WHERE
        Deviation_Code LIKE 'GA'
    GROUP BY Sortie_Number , Sortie_Date , Deviation_Code) sortiesGroundAborted) qty1,
    (SELECT 
        COUNT(*) AS qtySortiesFlown
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, SUM(flight_duration) fd
    FROM
        debrief
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) sortiesFlown) qty2,
    (SELECT 
        COUNT(*) AS qtySortiesAirAborted
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, Deviation_Code
    FROM
        debrief
    WHERE
        Deviation_Code IN ('AA' , 'AI')
    GROUP BY Sortie_Number , Sortie_Date , Deviation_Code) sortiesAirAborted) qty3;
    
## 8) AIRCRAFT AVAILABILITY (AA)
## AVAILABILITY RATE = MC HOURS x 100 TAI HOURS
# can't do this because we don't have the Possession Purpose Codes

## 9) BREAK
## if DEBRIEF LANDINGS STATUS CODE = “3” then add “1” to BREAK.
# not sure if this means sorties that had a break or just rows with breaks (multiple JCNs within one flown sortie)
# I assume here sorties with a break:
# 153
SELECT 
    COUNT(*) AS qtyBreaks
FROM
    (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) breaks;
# this is row count in debrief table
# SELECT COUNT(*) as qtyBreaks FROM debrief WHERE landing_status = 3;
# 2464 or about 41%
    
## 10) BREAK RELATED REPAIRS
## if DEBRIEF LANDINGS STATUS CODE = “3” and
## a related discrepancy record with capability code equal to “3” or “4”
## and for each debriefing discrepancy count the number of
## on and off equipment repairs with the same command
## ON EQUIP RECORD WITH ACTION TAKEN CODE equals “P,” “R,” “G,” “K,” “L,” “V,” “Z”
## or OFF EQUIP RECORD WITH ACTION TAKEN CODE equals “A,” “F,” “G,” “K,” “L,” “M,” “N,” “V,” “Z”
## with the same command code, geographic location indicator and jcn
## then add units to BREAK RELATED REPAIRS.
# 302
SELECT 
    COUNT(oem.Job_Control_Number) AS qtyBreakRelatedRepairs
FROM
    on_equipment_maintenance oem
        JOIN
    (SELECT DISTINCT
        Job_Control_Number, Command, Geographic_Location
    FROM
        debrief
    WHERE
        landing_status = 3
            AND capability_code IN (3 , 4)) AS deb ON deb.job_control_number = oem.Job_Control_Number
        AND deb.command = oem.Command
        AND deb.Geographic_Location = oem.Geographic_Location
WHERE
    (oem.Action_Taken_Code IN ('P' , 'R', 'G', 'K', 'L', 'V', 'Z')
        AND oem.Record_Type LIKE 'ON')
        OR (oem.Action_Taken_Code IN ('A' , 'F', 'G', 'K', 'L', 'M', 'N', 'V', 'Z')
        AND oem.Record_Type LIKE 'OFF');

## 11) BREAK RATE
## ( BREAK / SORTIES FLOWN ) x 100
# 153 / 601 or about 25%
SELECT 
    qtyBreaks / qtySortiesFlown AS breakRate
FROM
    (SELECT 
        COUNT(*) AS qtyBreaks
    FROM
        (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) breaks) qty1,
    (SELECT 
        COUNT(*) AS qtySortiesFlown
    FROM
        (SELECT 
        Sortie_Number, Sortie_Date, SUM(flight_duration) fd
    FROM
        debrief
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) sortiesFlown) qty2;
    
## 12) FIX
## if DEBRIEF LANDINGS STATUS CODE = “3” and there is a related FIX action then add “1” to FIX.
# not sure what a fix action is, but taking any oem row
# not sure if this is JCN's fixed or sorties fixed
# 152 - all but one break
SELECT 
    COUNT(*) AS qtyFixedBrokenFlownSorties
FROM
    (SELECT DISTINCT
        JCNs.sortie_number, JCNs.sortie_date # only those broken sorties flown that are fixed
    FROM
        (SELECT 
        job_control_number
    FROM
        on_equipment_maintenance) AS oem
    JOIN (SELECT DISTINCT
        d1.Sortie_Number, d1.Sortie_Date, d1.Job_Control_Number # all unique JCNs from broken sorties flown
    FROM
        debrief d1
    JOIN (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) AS sortiesFlown ON sortiesFlown.sortie_number = d1.sortie_number
        AND sortiesFlown.sortie_date = d1.sortie_date
    WHERE
        Job_Control_Number IS NOT NULL) AS JCNs ON JCNs.job_control_number = oem.job_control_number) AS fixTable;
        
## 13 4 HOUR FIX RATE
## 4 HOUR FIX RATE = (BREAKS FIXED WITHIN 4 HOURS AFTER LANDING / BREAKS) X 100
# assuming these fixes are the max for that sortie flown, so if some jcns are fixed within four hours but others from the flown sortie are not then it does not count as a 4-hour fix
# 1%
SELECT 
    qtyFourHourFlownSortieBreaksFixed / qtyBreaks AS fourHourFixRate
FROM
    (SELECT 
        COUNT(*) AS qtyFourHourFlownSortieBreaksFixed
    FROM
        (SELECT 
        sortie_number,
            sortie_date,
            MAX(hoursBetweenBreakAndFix) AS jcnMaxHoursBwBreakAndFix,
            COUNT(*) AS qtyJCNs
    FROM
        (SELECT 
        lastJCNFixTimestampTable.sortie_number,
            lastJCNFixTimestampTable.sortie_date,
            lastJCNFixTimestampTable.lastJCNFixTimestamp,
            lastJCNLandTimestampTable.lastJCNLandTimestamp,
            lastJCNLandTimestampTable.job_control_number,
            TIMESTAMPDIFF(HOUR, lastJCNLandTimestampTable.lastJCNLandTimestamp, lastJCNFixTimestampTable.lastJCNFixTimestamp) AS hoursBetweenBreakAndFix
    FROM
        (SELECT 
        sortie_number,
            sortie_date,
            job_control_number,
            MAX(TIMESTAMP(Landing_Date, CONCAT(landing_time, '00'))) AS lastJCNLandTimestamp
    FROM
        debrief
    GROUP BY job_control_number
    HAVING job_control_number IS NOT NULL) lastJCNLandTimestampTable
    JOIN (SELECT 
        JCNs.sortie_number,
            JCNs.sortie_date,
            JCNs.job_control_number,
            MAX(TIMESTAMP(oem.transaction_date, CONCAT(oem.stop_time, '00'))) AS lastJCNFixTimestamp
    FROM
        (SELECT 
        job_control_number, transaction_date, stop_time
    FROM
        on_equipment_maintenance) AS oem
    JOIN (SELECT DISTINCT
        d1.Sortie_Number, d1.Sortie_Date, d1.Job_Control_Number
    FROM
        debrief d1
    JOIN (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) AS sortiesFlown ON sortiesFlown.sortie_number = d1.sortie_number
        AND sortiesFlown.sortie_date = d1.sortie_date
    WHERE
        Job_Control_Number IS NOT NULL) AS JCNs ON JCNs.job_control_number = oem.job_control_number
    GROUP BY JCNs.sortie_number , JCNs.sortie_date , JCNs.job_control_number) lastJCNFixTimestampTable ON lastJCNFixTimestampTable.job_control_number = lastJCNLandTimestampTable.job_control_number
    GROUP BY lastJCNLandTimestampTable.Job_Control_Number) AS jcnFixTimeTable
    GROUP BY Sortie_Number , Sortie_Date) flownSortieBreaksFixedTimeTable
    WHERE
        jcnMaxHoursBwBreakAndFix < 4
            AND jcnMaxHoursBwBreakAndFix >= 0) qty1,
    (SELECT 
        COUNT(*) AS qtyBreaks
    FROM
        (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) breaks) qty2;
    
## 14 8 HOUR FIX RATE
## 8 HOUR FIX RATE = (BREAKS FIXED WITHIN 8 HOURS AFTER LANDING / BREAKS) X 100
# still not sure here
# 3%
SELECT 
    qtyEightHourFlownSortieBreaksFixed / qtyBreaks AS eightHourFixRate
FROM
    (SELECT 
        COUNT(*) AS qtyEightHourFlownSortieBreaksFixed
    FROM
        (SELECT 
        sortie_number,
            sortie_date,
            MAX(hoursBetweenBreakAndFix) AS jcnMaxHoursBwBreakAndFix,
            COUNT(*) AS qtyJCNs
    FROM
        (SELECT 
        lastJCNFixTimestampTable.sortie_number,
            lastJCNFixTimestampTable.sortie_date,
            lastJCNFixTimestampTable.lastJCNFixTimestamp,
            lastJCNLandTimestampTable.lastJCNLandTimestamp,
            lastJCNLandTimestampTable.job_control_number,
            TIMESTAMPDIFF(HOUR, lastJCNLandTimestampTable.lastJCNLandTimestamp, lastJCNFixTimestampTable.lastJCNFixTimestamp) AS hoursBetweenBreakAndFix
    FROM
        (SELECT 
        sortie_number,
            sortie_date,
            job_control_number,
            MAX(TIMESTAMP(Landing_Date, CONCAT(landing_time, '00'))) AS lastJCNLandTimestamp
    FROM
        debrief
    GROUP BY job_control_number
    HAVING job_control_number IS NOT NULL) lastJCNLandTimestampTable
    JOIN (SELECT 
        JCNs.sortie_number,
            JCNs.sortie_date,
            JCNs.job_control_number,
            MAX(TIMESTAMP(oem.transaction_date, CONCAT(oem.stop_time, '00'))) AS lastJCNFixTimestamp
    FROM
        (SELECT 
        job_control_number, transaction_date, stop_time
    FROM
        on_equipment_maintenance) AS oem
    JOIN (SELECT DISTINCT
        d1.Sortie_Number, d1.Sortie_Date, d1.Job_Control_Number
    FROM
        debrief d1
    JOIN (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) AS sortiesFlown ON sortiesFlown.sortie_number = d1.sortie_number
        AND sortiesFlown.sortie_date = d1.sortie_date
    WHERE
        Job_Control_Number IS NOT NULL) AS JCNs ON JCNs.job_control_number = oem.job_control_number
    GROUP BY JCNs.sortie_number , JCNs.sortie_date , JCNs.job_control_number) lastJCNFixTimestampTable ON lastJCNFixTimestampTable.job_control_number = lastJCNLandTimestampTable.job_control_number
    GROUP BY lastJCNLandTimestampTable.Job_Control_Number) AS jcnFixTimeTable
    GROUP BY Sortie_Number , Sortie_Date) flownSortieBreaksFixedTimeTable
    WHERE
        jcnMaxHoursBwBreakAndFix < 8
            AND jcnMaxHoursBwBreakAndFix >= 0) qty1,
    (SELECT 
        COUNT(*) AS qtyBreaks
    FROM
        (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) breaks) qty2;
    
## 15 12 HOUR FIX RATE
## 12 HOUR FIX RATE = (BREAKS FIXED WITHIN 12 HOURS AFTER LANDING / BREAKS) X 100
# not sure here about what a break or fix is -  treating as only if all JCNs within the flown sortie are complete in less than 12 hours
# 5%
SELECT 
    qtyTwelveHourFlownSortieBreaksFixed / qtyBreaks AS twelveHourFixRate
FROM
    (SELECT 
        COUNT(*) AS qtyTwelveHourFlownSortieBreaksFixed
    FROM
        (SELECT 
        sortie_number,
            sortie_date,
            MAX(hoursBetweenBreakAndFix) AS jcnMaxHoursBwBreakAndFix,
            COUNT(*) AS qtyJCNs
    FROM
        (SELECT 
        lastJCNFixTimestampTable.sortie_number,
            lastJCNFixTimestampTable.sortie_date,
            lastJCNFixTimestampTable.lastJCNFixTimestamp,
            lastJCNLandTimestampTable.lastJCNLandTimestamp,
            lastJCNLandTimestampTable.job_control_number,
            TIMESTAMPDIFF(HOUR, lastJCNLandTimestampTable.lastJCNLandTimestamp, lastJCNFixTimestampTable.lastJCNFixTimestamp) AS hoursBetweenBreakAndFix
    FROM
        (SELECT 
        sortie_number,
            sortie_date,
            job_control_number,
            MAX(TIMESTAMP(Landing_Date, CONCAT(landing_time, '00'))) AS lastJCNLandTimestamp
    FROM
        debrief
    GROUP BY job_control_number
    HAVING job_control_number IS NOT NULL) lastJCNLandTimestampTable
    JOIN (SELECT 
        JCNs.sortie_number,
            JCNs.sortie_date,
            JCNs.job_control_number,
            MAX(TIMESTAMP(oem.transaction_date, CONCAT(oem.stop_time, '00'))) AS lastJCNFixTimestamp
    FROM
        (SELECT 
        job_control_number, transaction_date, stop_time
    FROM
        on_equipment_maintenance) AS oem
    JOIN (SELECT DISTINCT
        d1.Sortie_Number, d1.Sortie_Date, d1.Job_Control_Number
    FROM
        debrief d1
    JOIN (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) AS sortiesFlown ON sortiesFlown.sortie_number = d1.sortie_number
        AND sortiesFlown.sortie_date = d1.sortie_date
    WHERE
        Job_Control_Number IS NOT NULL) AS JCNs ON JCNs.job_control_number = oem.job_control_number
    GROUP BY JCNs.sortie_number , JCNs.sortie_date , JCNs.job_control_number) lastJCNFixTimestampTable ON lastJCNFixTimestampTable.job_control_number = lastJCNLandTimestampTable.job_control_number
    GROUP BY lastJCNLandTimestampTable.Job_Control_Number) AS jcnFixTimeTable
    GROUP BY Sortie_Number , Sortie_Date) flownSortieBreaksFixedTimeTable
    WHERE
        jcnMaxHoursBwBreakAndFix < 12
            AND jcnMaxHoursBwBreakAndFix >= 0) qty1,
    (SELECT 
        COUNT(*) AS qtyBreaks
    FROM
        (SELECT 
        Sortie_Number,
            Sortie_Date,
            Landing_Status,
            SUM(flight_duration) fd
    FROM
        debrief
    WHERE
        Landing_Status = 3
    GROUP BY Sortie_Number , Sortie_Date
    HAVING SUM(flight_duration) > 0) breaks) qty2;
    
## 16) IN FLIGHT EMERGENCY
## if DEBRIEF DEVIATION CODE = “FE” OR “AI” then add “1” to IN FLIGHT EMERGENCY.
# 28
SELECT 
    COUNT(*) AS qtyIFEs
FROM
    (SELECT 
        Sortie_Number, Sortie_Date, Deviation_Code
    FROM
        debrief
    WHERE
        Deviation_Code IN ('FE' , 'AI')
    GROUP BY Sortie_Number , Sortie_Date , Deviation_Code) AS uniqueIFE;
    
## 17) BASE LEVEL EVENTS
## if the on-equipment or off-equipment and the first position of the WUC is not equal to “0”
## (or not equal to support general LCNs, reference Appendix I) 
## (the TYPE MAINTENANCE CODE is not equal to “R” and
## the WHEN DISCOVERED CODE is not equal to “S”) then augment BASE LEVEL EVENTS.
# not right b/c our preceding 0's are missing (first line)
# 37845, or 59%
SELECT 
    COUNT(*) AS qtyBaseLevelEvents
FROM
    on_equipment_maintenance
WHERE
    Type_maintenance_code NOT LIKE 'R'
        AND when_discovered_code NOT LIKE 'S'
        AND Work_Unit_Code NOT LIKE '0%'
        AND Work_Unit_Code NOT IN (01000 , 02000,
        03000,
        03100,
        03101,
        03102,
        03107,
        03111,
        03112,
        03113,
        03114,
        03115,
        '0311K',
        '0311L',
        '0311M',
        '0311N',
        '0311P',
        '0311R',
        '0311S',
        '0311T',
        '0311U',
        03121,
        03128,
        03130,
        03142,
        03156,
        03184,
        03200,
        03205,
        03209,
        03210,
        03212,
        03215,
        03220,
        03221,
        03268,
        03300,
        03305,
        03310,
        03311,
        03312,
        03313,
        03314,
        03320,
        03330,
        03336,
        03340,
        03360,
        03370,
        03380,
        03390,
        03395,
        03400,
        '0341A',
        '0341B',
        '0341C',
        '0341D',
        '0341E',
        '0341F',
        '0341G',
        '0341H',
        '0341J',
        '0341K',
        '0341L',
        '0341M',
        '0341N',
        '0341P',
        '0341Q',
        '0341R',
        '0341S',
        '0341T',
        '0341U',
        '0341V',
        '0341W',
        '0341X',
        '0341Y',
        '0341Z',
        '0342A',
        '0342B',
        03510,
        03580,
        03596,
        03597,
        03600,
        03610,
        03700,
        03710,
        03711,
        03712,
        03713,
        03714,
        03720,
        03721,
        03722,
        03723,
        03724,
        03730,
        03731,
        03732,
        03750,
        03755,
        03800,
        03802,
        03803,
        03804,
        03806,
        03900,
        03999,
        04000,
        04100,
        04101,
        04110,
        04111,
        04112,
        04113,
        04114,
        04115,
        04116,
        04117,
        04118,
        04119,
        '0411A',
        '0411B',
        '0411C',
        '0411D',
        '0411E',
        '0411H',
        '0411J',
        '0411K',
        04120,
        04121,
        04122,
        04123,
        04124,
        04125,
        04126,
        04127,
        04128,
        04129,
        '0412A',
        '0412B',
        '0412C',
        '0412D',
        '0412E',
        '0412F',
        '0412G',
        '0412H',
        '0412J',
        '0412H',
        '0412J',
        '0412L',
        '0412M',
        '0412N',
        '0412P',
        '0412Q',
        04130,
        04131,
        04132,
        04133,
        04134,
        04135,
        04136,
        04137,
        04138,
        04139,
        '0413A',
        '0413B',
        '0413C',
        '0413E',
        '0413F',
        '0413H',
        '0413J',
        '0413K',
        '0413L',
        '0413M',
        '0413N',
        '0413P',
        04140,
        04141,
        04142,
        04143,
        04144,
        04145,
        04146,
        04147,
        04148,
        04149,
        04150,
        04151,
        04152,
        '0415A',
        '0415B',
        '0415C',
        04160,
        04161,
        04162,
        04163,
        04170,
        04180,
        04181,
        04182,
        04184,
        04185,
        04186,
        04187,
        04188,
        04189,
        '0418A',
        '0418B',
        '0418C',
        '0418D',
        '0418E',
        '0418F',
        04190,
        04199,
        04200,
        04210,
        04220,
        04221,
        04222,
        04227,
        04228,
        04270,
        04280,
        04310,
        04311,
        04313,
        04314,
        04315,
        04316,
        04317,
        04320,
        04321,
        04322,
        04324,
        04325,
        04326,
        04327,
        04330,
        04340,
        04341,
        04342,
        04343,
        04344,
        04345,
        04346,
        04347,
        04348,
        04349,
        04350,
        04351,
        04352,
        04353,
        04354,
        04355,
        04356,
        04358,
        04359,
        04360,
        04361,
        04362,
        04363,
        04364,
        04365,
        04366,
        04367,
        04370,
        04371,
        04372,
        04373,
        04400,
        04500,
        04510,
        04572,
        04573,
        04574,
        04575,
        04576,
        04577,
        04578,
        04583,
        04584,
        04610,
        04620,
        04630,
        04650,
        04660,
        04999,
        '04MD4',
        05000,
        06000,
        07000,
        08000,
        09000);

## 18) BASE NOT REPAIRABLE THIS STATION (NRTS) 
## if the on-equipment or off-equipment ACTION TAKEN CODE equals
## “0,” “1,” “2,” “3,” “4,” “5,” “6,” “7,” or “8” and
## (the TYPE MAINTENANCE CODE is not equal to “R” and
## the WHEN DISCOVERED CODE is not equal to “S”),
# just 1
SELECT 
    COUNT(*) AS qtyBaseNRTS
FROM
    on_equipment_maintenance
WHERE
    Type_maintenance_code NOT LIKE 'R'
        AND when_discovered_code NOT LIKE 'S'
        AND (Action_Taken_Code IN ('0' , '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '0.0',
        '1.0',
        '2.0',
        '3.0',
        '4.0',
        '5.0',
        '6.0',
        '7.0',
        '8.0'));
        
## 19) DEPOT NRTS
## if the on-equipment or off-equipment ACTION TAKEN CODE equals
## “0,” “1,” “2,” “3,” “4,” “5,” “6,” “7” or “8” or
## (the TYPE MAINTENANCE CODE equals to “R” or
## the WHEN DISCOVERED CODE equals to “S”), then add UNITS to DEPOT NRTS.      
# 13714, or 22%
SELECT 
    COUNT(*) AS qtyDepotNRTS
FROM
    on_equipment_maintenance
WHERE
    Type_maintenance_code LIKE 'R'
        OR when_discovered_code LIKE 'S'
        OR (Action_Taken_Code IN ('0' , '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '0.0',
        '1.0',
        '2.0',
        '3.0',
        '4.0',
        '5.0',
        '6.0',
        '7.0',
        '8.0'));
        
## 20) TOTAL NRTS
## TOTAL NRTS = BASE NRTS + DEPOT NRTS
# 13715
SELECT 
    qtyBaseNRTS + qtyDepotNRTS AS qtyTotalNRTS
FROM
    (SELECT 
        COUNT(*) AS qtyDepotNRTS
    FROM
        on_equipment_maintenance
    WHERE
        Type_maintenance_code LIKE 'R'
            OR when_discovered_code LIKE 'S'
            OR (Action_Taken_Code IN ('0' , '1', '2', '3', '4', '5', '6', '7', '8', '0.0', '1.0', '2.0', '3.0', '4.0', '5.0', '6.0', '7.0', '8.0'))) qty1,
    (SELECT 
        COUNT(*) AS qtyBaseNRTS
    FROM
        on_equipment_maintenance
    WHERE
        Type_maintenance_code NOT LIKE 'R'
            AND when_discovered_code NOT LIKE 'S'
            AND (Action_Taken_Code IN ('0' , '1', '2', '3', '4', '5', '6', '7', '8', '0.0', '1.0', '2.0', '3.0', '4.0', '5.0', '6.0', '7.0', '8.0'))) qty2;
            
## 21) BASE NRTS RATE
## BASE NRTS RATE = (BASE NRTS / [BCS + BASE BCR + BASE NRTS + BASE CONDEMNATIONS] ) X 100
# tbd

## 22) DEPOT NRTS RATE
## DEPOT NRTS RATE = ( DEPOT NRTS / [RTOKs + DEPOT BCR + DEPOT NRTS + DEPOT CONDEMNATIONS - BASE NRTS] ) X 100
# tbd

## 23) TOTAL NRTS RATE
## TOTAL NRTS RATE = ( TOTAL NRTS / [BCS + RTOKs + TOTAL BCR + TOTAL CONDEMNATIONS - TOTAL NRTS] ) X 100
# tbd

## 24) BASE BENCH CHECK REPAIR
## if the on-equipment first position of the WUC is not equal to “0” and
## (or not equal to support general LCNs, reference Appendix I)
## HOW MAL CLASS equals 1 or 2 and the ACTION TAKEN CODE equals “P” or “R” and
## there exists a related off-equipment record whose WUC equals this on-equipment record’s WUC and
## the ACTION TAKEN CODE of the off-equipment record equals “A” or “F”
## (the TYPE MAINTENANCE CODE is not equal to “R” and the WHEN DISCOVERED CODE is not equal to “S”)
# this must be wrong b/c we don't have the off-maintenance actions
# something like this:
SELECT 
    COUNT(*) AS qtyBaseBenchCheckRepair
FROM
    on_equipment_maintenance
WHERE
    Work_Unit_Code NOT LIKE '0%'
        AND action_taken_code IN ('P' , 'R')
        AND How_Malfunction_Class_Ind IN (1 , 2)
        AND Work_Unit_Code NOT IN (01000 , 02000,
        03000,
        03100,
        03101,
        03102,
        03107,
        03111,
        03112,
        03113,
        03114,
        03115,
        '0311K',
        '0311L',
        '0311M',
        '0311N',
        '0311P',
        '0311R',
        '0311S',
        '0311T',
        '0311U',
        03121,
        03128,
        03130,
        03142,
        03156,
        03184,
        03200,
        03205,
        03209,
        03210,
        03212,
        03215,
        03220,
        03221,
        03268,
        03300,
        03305,
        03310,
        03311,
        03312,
        03313,
        03314,
        03320,
        03330,
        03336,
        03340,
        03360,
        03370,
        03380,
        03390,
        03395,
        03400,
        '0341A',
        '0341B',
        '0341C',
        '0341D',
        '0341E',
        '0341F',
        '0341G',
        '0341H',
        '0341J',
        '0341K',
        '0341L',
        '0341M',
        '0341N',
        '0341P',
        '0341Q',
        '0341R',
        '0341S',
        '0341T',
        '0341U',
        '0341V',
        '0341W',
        '0341X',
        '0341Y',
        '0341Z',
        '0342A',
        '0342B',
        03510,
        03580,
        03596,
        03597,
        03600,
        03610,
        03700,
        03710,
        03711,
        03712,
        03713,
        03714,
        03720,
        03721,
        03722,
        03723,
        03724,
        03730,
        03731,
        03732,
        03750,
        03755,
        03800,
        03802,
        03803,
        03804,
        03806,
        03900,
        03999,
        04000,
        04100,
        04101,
        04110,
        04111,
        04112,
        04113,
        04114,
        04115,
        04116,
        04117,
        04118,
        04119,
        '0411A',
        '0411B',
        '0411C',
        '0411D',
        '0411E',
        '0411H',
        '0411J',
        '0411K',
        04120,
        04121,
        04122,
        04123,
        04124,
        04125,
        04126,
        04127,
        04128,
        04129,
        '0412A',
        '0412B',
        '0412C',
        '0412D',
        '0412E',
        '0412F',
        '0412G',
        '0412H',
        '0412J',
        '0412H',
        '0412J',
        '0412L',
        '0412M',
        '0412N',
        '0412P',
        '0412Q',
        04130,
        04131,
        04132,
        04133,
        04134,
        04135,
        04136,
        04137,
        04138,
        04139,
        '0413A',
        '0413B',
        '0413C',
        '0413E',
        '0413F',
        '0413H',
        '0413J',
        '0413K',
        '0413L',
        '0413M',
        '0413N',
        '0413P',
        04140,
        04141,
        04142,
        04143,
        04144,
        04145,
        04146,
        04147,
        04148,
        04149,
        04150,
        04151,
        04152,
        '0415A',
        '0415B',
        '0415C',
        04160,
        04161,
        04162,
        04163,
        04170,
        04180,
        04181,
        04182,
        04184,
        04185,
        04186,
        04187,
        04188,
        04189,
        '0418A',
        '0418B',
        '0418C',
        '0418D',
        '0418E',
        '0418F',
        04190,
        04199,
        04200,
        04210,
        04220,
        04221,
        04222,
        04227,
        04228,
        04270,
        04280,
        04310,
        04311,
        04313,
        04314,
        04315,
        04316,
        04317,
        04320,
        04321,
        04322,
        04324,
        04325,
        04326,
        04327,
        04330,
        04340,
        04341,
        04342,
        04343,
        04344,
        04345,
        04346,
        04347,
        04348,
        04349,
        04350,
        04351,
        04352,
        04353,
        04354,
        04355,
        04356,
        04358,
        04359,
        04360,
        04361,
        04362,
        04363,
        04364,
        04365,
        04366,
        04367,
        04370,
        04371,
        04372,
        04373,
        04400,
        04500,
        04510,
        04572,
        04573,
        04574,
        04575,
        04576,
        04577,
        04578,
        04583,
        04584,
        04610,
        04620,
        04630,
        04650,
        04660,
        04999,
        '04MD4',
        05000,
        06000,
        07000,
        08000,
        09000);
        
## 25) DEPOT BCR
## if the first position of the WUC is not equal to “0” and
## (or not equal to support general LCNs, reference Appendix I)
## the ACTION TAKEN CODE of the off-equipment record equals “A” or “F”
## (the TYPE MAINTENANCE CODE equals “R” and
## the WHEN DISCOVERED CODE equals “S”) then add UNITS to DEPOT BCR.
# not even going to try

## 25) TOTAL BCR
## TOTAL BCR = BASE BCR + DEPOT BCR

## 26) BENCH CHECK SERVICEABLE (BCS)
## if the on-equipment first position of the WUC is not equal to “0” and
## (or not equal to support general LCNs, reference Appendix I)
## the HOW MAL CLASS equals 1 or 2 and the ACTION TAKEN CODE equals “P” or “R” and
## there is a related off-equipment record whose WUC equals this on-equipment record’s WUC and
## the ACTION TAKEN CODE equals “B” and
## (the TYPE MAINTENANCE CODE is not equal to “R” and the WHEN DISCOVERED CODE is not equal to “S”)
# related to off-equipment, not going to try

## 28) BASE BCS RATE
## BASE BCS RATE = ( BCS / [BCS + BASE BCR + BASE NRTS + BASE CONDEMNATIONS] ) X 100

## 29) RTOK
## if the first position of the WUC is not equal to “0” and (or not equal to support general LCNs, reference Appendix I)
## the ACTION TAKEN CODE of the off equipment record equals“B” (the TYPE MAINTENANCE CODE equals “R” or
## the WHEN DISCOVERED CODE equals “S”)
# related to off-equipment, not going to try

## 30) RTOK RATE
## RTOK RATE = ( RTOK / [RTOK + DEPOT BCR + DEPOT CONDEMNATIONS] ) X 100

## 31) CANNIBALIZATIONS (CANNS)
## if the on-equipment the first position of the WUC is not equal to “0” and
## (or not equal to support general LCNs, reference Appendix I) the ACTION TAKEN CODE equals “T”
# 273
SELECT 
    COUNT(*) AS qtyCannibalizations
FROM
    on_equipment_maintenance
WHERE
    action_taken_code LIKE 'T'
        AND Work_Unit_Code NOT LIKE '0%'
        AND Work_Unit_Code NOT IN (01000 , 02000,
        03000,
        03100,
        03101,
        03102,
        03107,
        03111,
        03112,
        03113,
        03114,
        03115,
        '0311K',
        '0311L',
        '0311M',
        '0311N',
        '0311P',
        '0311R',
        '0311S',
        '0311T',
        '0311U',
        03121,
        03128,
        03130,
        03142,
        03156,
        03184,
        03200,
        03205,
        03209,
        03210,
        03212,
        03215,
        03220,
        03221,
        03268,
        03300,
        03305,
        03310,
        03311,
        03312,
        03313,
        03314,
        03320,
        03330,
        03336,
        03340,
        03360,
        03370,
        03380,
        03390,
        03395,
        03400,
        '0341A',
        '0341B',
        '0341C',
        '0341D',
        '0341E',
        '0341F',
        '0341G',
        '0341H',
        '0341J',
        '0341K',
        '0341L',
        '0341M',
        '0341N',
        '0341P',
        '0341Q',
        '0341R',
        '0341S',
        '0341T',
        '0341U',
        '0341V',
        '0341W',
        '0341X',
        '0341Y',
        '0341Z',
        '0342A',
        '0342B',
        03510,
        03580,
        03596,
        03597,
        03600,
        03610,
        03700,
        03710,
        03711,
        03712,
        03713,
        03714,
        03720,
        03721,
        03722,
        03723,
        03724,
        03730,
        03731,
        03732,
        03750,
        03755,
        03800,
        03802,
        03803,
        03804,
        03806,
        03900,
        03999,
        04000,
        04100,
        04101,
        04110,
        04111,
        04112,
        04113,
        04114,
        04115,
        04116,
        04117,
        04118,
        04119,
        '0411A',
        '0411B',
        '0411C',
        '0411D',
        '0411E',
        '0411H',
        '0411J',
        '0411K',
        04120,
        04121,
        04122,
        04123,
        04124,
        04125,
        04126,
        04127,
        04128,
        04129,
        '0412A',
        '0412B',
        '0412C',
        '0412D',
        '0412E',
        '0412F',
        '0412G',
        '0412H',
        '0412J',
        '0412H',
        '0412J',
        '0412L',
        '0412M',
        '0412N',
        '0412P',
        '0412Q',
        04130,
        04131,
        04132,
        04133,
        04134,
        04135,
        04136,
        04137,
        04138,
        04139,
        '0413A',
        '0413B',
        '0413C',
        '0413E',
        '0413F',
        '0413H',
        '0413J',
        '0413K',
        '0413L',
        '0413M',
        '0413N',
        '0413P',
        04140,
        04141,
        04142,
        04143,
        04144,
        04145,
        04146,
        04147,
        04148,
        04149,
        04150,
        04151,
        04152,
        '0415A',
        '0415B',
        '0415C',
        04160,
        04161,
        04162,
        04163,
        04170,
        04180,
        04181,
        04182,
        04184,
        04185,
        04186,
        04187,
        04188,
        04189,
        '0418A',
        '0418B',
        '0418C',
        '0418D',
        '0418E',
        '0418F',
        04190,
        04199,
        04200,
        04210,
        04220,
        04221,
        04222,
        04227,
        04228,
        04270,
        04280,
        04310,
        04311,
        04313,
        04314,
        04315,
        04316,
        04317,
        04320,
        04321,
        04322,
        04324,
        04325,
        04326,
        04327,
        04330,
        04340,
        04341,
        04342,
        04343,
        04344,
        04345,
        04346,
        04347,
        04348,
        04349,
        04350,
        04351,
        04352,
        04353,
        04354,
        04355,
        04356,
        04358,
        04359,
        04360,
        04361,
        04362,
        04363,
        04364,
        04365,
        04366,
        04367,
        04370,
        04371,
        04372,
        04373,
        04400,
        04500,
        04510,
        04572,
        04573,
        04574,
        04575,
        04576,
        04577,
        04578,
        04583,
        04584,
        04610,
        04620,
        04630,
        04650,
        04660,
        04999,
        '04MD4',
        05000,
        06000,
        07000,
        08000,
        09000);
        
## 32) CANNIBALIZATIONS (CANNS) HOURS
## if the on-equipment the first position of the WUC is not equal to“0” and
## (or not equal to support general LCNs, reference Appendix I)
## the ACTION TAKEN CODE equals “T” or“U”
## then add LABOR MANHOURS to CANNIBALIZATION HOURS.
# 4000 hrs
SELECT 
    sum(Labor_Manhours) AS qtyCannibalizations
FROM
    on_equipment_maintenance
WHERE
    action_taken_code IN ('U','T')
        AND Work_Unit_Code NOT LIKE '0%'
        AND Work_Unit_Code NOT IN (01000 , 02000,
        03000,
        03100,
        03101,
        03102,
        03107,
        03111,
        03112,
        03113,
        03114,
        03115,
        '0311K',
        '0311L',
        '0311M',
        '0311N',
        '0311P',
        '0311R',
        '0311S',
        '0311T',
        '0311U',
        03121,
        03128,
        03130,
        03142,
        03156,
        03184,
        03200,
        03205,
        03209,
        03210,
        03212,
        03215,
        03220,
        03221,
        03268,
        03300,
        03305,
        03310,
        03311,
        03312,
        03313,
        03314,
        03320,
        03330,
        03336,
        03340,
        03360,
        03370,
        03380,
        03390,
        03395,
        03400,
        '0341A',
        '0341B',
        '0341C',
        '0341D',
        '0341E',
        '0341F',
        '0341G',
        '0341H',
        '0341J',
        '0341K',
        '0341L',
        '0341M',
        '0341N',
        '0341P',
        '0341Q',
        '0341R',
        '0341S',
        '0341T',
        '0341U',
        '0341V',
        '0341W',
        '0341X',
        '0341Y',
        '0341Z',
        '0342A',
        '0342B',
        03510,
        03580,
        03596,
        03597,
        03600,
        03610,
        03700,
        03710,
        03711,
        03712,
        03713,
        03714,
        03720,
        03721,
        03722,
        03723,
        03724,
        03730,
        03731,
        03732,
        03750,
        03755,
        03800,
        03802,
        03803,
        03804,
        03806,
        03900,
        03999,
        04000,
        04100,
        04101,
        04110,
        04111,
        04112,
        04113,
        04114,
        04115,
        04116,
        04117,
        04118,
        04119,
        '0411A',
        '0411B',
        '0411C',
        '0411D',
        '0411E',
        '0411H',
        '0411J',
        '0411K',
        04120,
        04121,
        04122,
        04123,
        04124,
        04125,
        04126,
        04127,
        04128,
        04129,
        '0412A',
        '0412B',
        '0412C',
        '0412D',
        '0412E',
        '0412F',
        '0412G',
        '0412H',
        '0412J',
        '0412H',
        '0412J',
        '0412L',
        '0412M',
        '0412N',
        '0412P',
        '0412Q',
        04130,
        04131,
        04132,
        04133,
        04134,
        04135,
        04136,
        04137,
        04138,
        04139,
        '0413A',
        '0413B',
        '0413C',
        '0413E',
        '0413F',
        '0413H',
        '0413J',
        '0413K',
        '0413L',
        '0413M',
        '0413N',
        '0413P',
        04140,
        04141,
        04142,
        04143,
        04144,
        04145,
        04146,
        04147,
        04148,
        04149,
        04150,
        04151,
        04152,
        '0415A',
        '0415B',
        '0415C',
        04160,
        04161,
        04162,
        04163,
        04170,
        04180,
        04181,
        04182,
        04184,
        04185,
        04186,
        04187,
        04188,
        04189,
        '0418A',
        '0418B',
        '0418C',
        '0418D',
        '0418E',
        '0418F',
        04190,
        04199,
        04200,
        04210,
        04220,
        04221,
        04222,
        04227,
        04228,
        04270,
        04280,
        04310,
        04311,
        04313,
        04314,
        04315,
        04316,
        04317,
        04320,
        04321,
        04322,
        04324,
        04325,
        04326,
        04327,
        04330,
        04340,
        04341,
        04342,
        04343,
        04344,
        04345,
        04346,
        04347,
        04348,
        04349,
        04350,
        04351,
        04352,
        04353,
        04354,
        04355,
        04356,
        04358,
        04359,
        04360,
        04361,
        04362,
        04363,
        04364,
        04365,
        04366,
        04367,
        04370,
        04371,
        04372,
        04373,
        04400,
        04500,
        04510,
        04572,
        04573,
        04574,
        04575,
        04576,
        04577,
        04578,
        04583,
        04584,
        04610,
        04620,
        04630,
        04650,
        04660,
        04999,
        '04MD4',
        05000,
        06000,
        07000,
        08000,
        09000);
        
## 33) CANNIBALIZATION RATE
## if TYPE EQUIPMENT equals “A”or “E” then
## CANNIBALIZATION RATE = ( CANNIBALIZATIONS / SORTIES FLOWN ) X 100


## if TYPE EQUIPMENT not equal “A” or “E” then
## CANNIBALIZATION RATE = ( CANNIBALIZATIONS / action taken "P," "R," or "T" ) X 100
