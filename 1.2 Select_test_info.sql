-- A script to add in test information for anaemia, white blood cells and platelets -- 
drop table if exists  freya.tests;

-- Getting a table of anaemia, wbc and platelet codes
create table freya.tests
select d.e_patid, d.eventdate as test_date, d.enttype, d.medcode, d.data1, d.data2, d.data3, d.data4, d.data5, d.data6, d.data7, d.data8
from 18_299_Lyratzopoulos_e2.cprd_test d
where d.eventdate >= makedate(2005, 1) AND d.eventdate <= makedate(2017, 365)
and (enttype = 173 or (enttype = 288 and medcode in (4, 10404, 35749, 3942, 26910, 26909, 26272, 26913, 41531, 26908, 26912, 2405)))
or enttype IN (189,207)
;

-- Join in the medcode descriptions
set sql_safe_updates=0;

-- Add indexes
create index enttype on freya.tests (enttype);
create index e_patid on freya.tests (e_patid);
create index medcode on freya.tests (medcode);
create index data1 on freya.tests (data1);
create index data3 on freya.tests (data3);
create index data4 on freya.tests (data4);

drop table if exists  freya.tests_desc;
create table freya.tests_desc
select ta.e_patid,ta.test_date,ta.enttype,ta.medcode, l.descc, opr.operator,ta.data2 as value,sum.specimenunitofmeasure,tqu.testqualifier,ta.data5 as rangefrom,ta.data6 as rangeto
from freya.tests ta
left join lookup_tables.lookup_medical l on l.medcode = ta.medcode
left join lookup_tables.lookup_txtfiles_opr opr on opr.code = ta.data1
left join lookup_tables.lookup_txtfiles_sum sum on sum.code = ta.data3
left join lookup_tables.lookup_txtfiles_tqu tqu on tqu.code = ta.data4
;

-- Add indexes
create index e_patid on freya.tests_desc (e_patid);
create index medcode on freya.tests_desc (medcode);
create index e_patid on freya.cohortv1 (e_patid);
create index medcode on freya.cohortv1 (medcode);

-- Joining in gender information
-- drop table if exists  freya.tests_desc_gen;
-- create table freya.tests_anaemia_desc_gen
-- select ta.*,pt.gender
-- from freya.tests_anaemia_desc ta
-- inner join 18_299_Lyratzopoulos_e2.cprd_patient pt on pt.e_patid = ta .e_patid
-- ;

-- All events occurring 6 mo before symptom presentation
drop table if exists  freya.cohortv1_1;
create table freya.cohortv1_1
(
select l.*, d.enttype, d.medcode as test_medcode, d.test_date,d.descc,d.operator,d.value,d.specimenunitofmeasure,d.testqualifier,d.rangefrom,d.rangeto
from freya.cohortv1 l
left join freya.tests_desc d on d.e_patid = l.e_patid 
and d.test_date <= l.eventdate
and d.test_date >= date_sub(l.eventdate, interval 6 month)
)
;

-- go to R script 1.2.1