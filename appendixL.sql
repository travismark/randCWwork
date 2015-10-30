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
    HAVING SUM(flight_duration) > 0) breaks) qty2