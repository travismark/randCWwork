#### Downtime
## awp

## awm

###### Mission Impacts
##### aborts
### Air Aborts
## air by debrief subsystem WUC (wuc_description is often "description not found", 
##   which sounds like they don't know what's wrong yet.  look in OEM data for those)
##  air aborts are only either AI or AA, not both
# subsystem all
SELECT 
    d.Sortie_Number,
    d.Sortie_Date,
    d.Deviation_Code,
    d.Subsystem_Work_Unit_Code,
    d.Subsystem_WUC_Description
FROM
    debrief d
WHERE
    d.Deviation_Code IN ('AA' , 'AI')
GROUP BY d.Sortie_Number , d.Sortie_Date , deviation_code , Subsystem_Work_Unit_Code , Subsystem_WUC_Description;

# subsystem pareto
SELECT 
    Subsystem_Work_Unit_Code,
    Subsystem_WUC_Description,
    COUNT(*) AS airAborts
FROM
    (SELECT 
    d.Sortie_Number,
    d.Sortie_Date,
    d.Deviation_Code,
    d.Subsystem_Work_Unit_Code,
    d.Subsystem_WUC_Description
FROM
    debrief d
WHERE
    d.Deviation_Code IN ('AA' , 'AI')
GROUP BY d.Sortie_Number , d.Sortie_Date , deviation_code , Subsystem_Work_Unit_Code , Subsystem_WUC_Description) AS allAirAbortSubWucs
GROUP BY Subsystem_Work_Unit_Code , Subsystem_WUC_Description
ORDER BY COUNT(*) DESC;

## air by debrief System WUC
# wuc all - 209 rows returned - many more than the 29 air aborts
SELECT 
    d.Sortie_Number,
    d.Sortie_Date,
    d.Deviation_Code,
    d.Work_Unit_Code,
    d.WUC_Description
FROM
    debrief d
WHERE
    d.Deviation_Code IN ('AA' , 'AI')
GROUP BY d.Sortie_Number , d.Sortie_Date , deviation_code , Work_Unit_Code , WUC_Description;

# subsystem pareto (excluding description not found)
SELECT 
    Work_Unit_Code,
    WUC_Description,
    COUNT(*) AS airAborts
FROM
    (SELECT 
        d.Sortie_Number,
            d.Sortie_Date,
            d.Deviation_Code,
            d.Work_Unit_Code,
            d.WUC_Description
    FROM
        debrief d
    WHERE
        d.Deviation_Code IN ('AA' , 'AI')
    GROUP BY d.Sortie_Number , d.Sortie_Date , deviation_code , Work_Unit_Code , WUC_Description) AS allAirAbortWucs
WHERE
    WUC_Description NOT LIKE 'DESCRIPTION NOT FOUND'
GROUP BY Work_Unit_Code , WUC_Description
ORDER BY COUNT(*) DESC;

## air by oem WUC
SELECT 
    d.Sortie_Number,
    d.Sortie_Date,
    d.Deviation_Code,
    d.Job_Control_Number,
    oem.work_unit_code,
    oem.WUC_Narrative
FROM
    debrief d
        JOIN
    on_equipment_maintenance oem ON d.Job_Control_Number = oem.Job_Control_Number
WHERE
    d.Deviation_Code IN ('AA' , 'AI')
GROUP BY d.Sortie_Number , d.Sortie_Date , d.Deviation_Code , d.Job_Control_Number, oem.work_unit_code, oem.WUC_Narrative;

## missed flight hours

#### High Cost Drivers
## man hours
# PN - by total and by average
SELECT 
    On_Component_Part_Number,
    SUM(labor_manhours) AS totalLaborHrs, avg(labor_manhours) as avgLaborHrs
FROM
    on_equipment_maintenance
WHERE
    on_component_part_number IS NOT NULL
GROUP BY On_Component_Part_Number
ORDER BY SUM(labor_manhours) DESC;
# WUC
SELECT 
    Work_Unit_Code, WUC_Narrative,
    SUM(labor_manhours) AS totalLaborHrs, avg(labor_manhours) as avgLaborHrs
FROM
    on_equipment_maintenance
#WHERE
 #   on_component_part_number IS NOT NULL
GROUP BY work_unit_code, WUC_Narrative
ORDER BY SUM(labor_manhours) DESC;


## estimated cost
