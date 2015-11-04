### one way
# records per tail in a sortie
SELECT 
        Sortie_Date, Serial_Number, Sortie_Number, COUNT(*) AS c
    FROM
        debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number;
# count of tails in a sortie that have a certain nubmer of rows
SELECT 
    c AS recordsPerSortie, COUNT(*) AS rowsPerSortie
FROM
    (SELECT 
        Sortie_Date, Serial_Number, Sortie_Number, COUNT(*) AS c
    FROM
        debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number) a
GROUP BY c;

### second way
# unique Date/SortieNumber/GeoLoc
SELECT 
        Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location, avg(Flight_Duration)
    FROM
        debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location
    having avg(flight_duration) > 0
    ORDER BY Sortie_Number, Sortie_Date;

# SNs per Date/SortieNumber/GeoLoc
SELECT 
    Sortie_Date, Sortie_Number, Geographic_Location, COUNT(*) AS SNsPerSortie
FROM
    (SELECT 
        Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location
    FROM
        debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location) a
GROUP BY Sortie_Date, Sortie_Number, Geographic_Location;

select * from debrief where sortie_number = 702 and sortie_date like '2014-06-02'; # 2 aircraft used, 1 has no flight and requests a spare (Deviation SP
select * from debrief where sortie_number = 711 and sortie_date like '2014-07-01'; # 2 aircraft used, 1 has no flight and requests a flight interchange (Deviation TS)
select * from debrief where sortie_number = 703 and sortie_date like '2014-06-02'; # 3 aircraft used, 1 has no flight and requetss a spare, a second has no flight and requests interchange
select * from debrief where sortie_number = 301 and sortie_date like '2014-07-14'; # 2 aircraft used, 1 has some fligth (missing deviation though)
select * from debrief where sortie_number = 302 and sortie_date like '2014-07-14'; # 2 aircraft used, 1 has some fligth (missing deviation though)

# Count of Sorties that have a certain number of Aircraft SNs
select SNsPerSortie, count(*)
FROM
(SELECT 
    Sortie_Date, Sortie_Number, Geographic_Location, COUNT(*) AS SNsPerSortie
FROM
    (SELECT 
        Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location
    FROM
        debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location) a
GROUP BY Sortie_Date, Sortie_Number, Geographic_Location) a
group by SNsPerSortie;

# sorties that require more than one SN
SELECT Sortie_Date, Sortie_Number, Geographic_Location, count(*) as SNs FROM
(SELECT Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location
    FROM debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location) a
GROUP BY Sortie_Date, Sortie_Number, Geographic_Location 
HAVING count(*) > 1;
    
# sorties that require more than one SN and more than one SN accrue some flying time
SELECT Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location, avg(Flight_Duration) as duration
    FROM debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location;
    
SELECT Sortie_Date, Sortie_Number, Geographic_Location, count(*) as SNs FROM
(SELECT Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location, avg(Flight_Duration) as duration
    FROM debrief
    GROUP BY Sortie_Date, Serial_Number, Sortie_Number, Geographic_Location) a
WHERE duration > 0
GROUP BY Sortie_Date, Sortie_Number, Geographic_Location 
HAVING count(*) > 1;

select * from debrief where sortie_number = 601 and sortie_date like '2014-06-09'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - probably bad data
select * from debrief where sortie_number = 602 and sortie_date like '2014-06-09'; # 3 aircraft used, 1 is swapped with zero flight_duration, 1 has some flight (missing deviation though) but it's a different mission code - probably bad data
select * from debrief where sortie_number = 603 and sortie_date like '2014-06-14'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - maybe bad data but at least its the same mission class (name)
select * from debrief where sortie_number = 602 and sortie_date like '2014-06-16'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - probably bad data
select * from debrief where sortie_number = 601 and sortie_date like '2014-06-30'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - probably bad data
select * from debrief where sortie_number = 602 and sortie_date like '2014-06-30'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - probably bad data
select * from debrief where sortie_number = 603 and sortie_date like '2014-07-04'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - maybe bad data but at least its the same mission class (name)
select * from debrief where sortie_number = 603 and sortie_date like '2014-07-05'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - maybe bad data but at least its the same mission class (name) - maybe good data?
select * from debrief where sortie_number = 603 and sortie_date like '2014-07-05'; # 2 aircraft used, 1 has some flight (missing deviation though) but it's a different mission code - maybe bad data but at least its the same mission class (name)
select * from debrief where sortie_number = 601 and sortie_date like '2014-07-08'; ### FOUND ONE # 2 aircraft used, 1 has some flight (missing deviation though)
select * from debrief where sortie_number = 602 and sortie_date like '2014-07-25'; ### FOUND ONE # 2 aircraft used, 1 has some flight (AI air abort)
select * from debrief where sortie_number = 602 and sortie_date like '2014-07-28'; ### FOUND ONE # 2 aircraft used, 1 has some flight (AI air abort)

