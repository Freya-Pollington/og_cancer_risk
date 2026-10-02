-- Create table of patients with one of the disease outcomes of interest
-- First for CPRD
DROP TABLE IF EXISTS freya.disease_index_cprd;
Create table freya.disease_index_cprd
select cl.e_patid, cl.eventdate, cl.medcode,cp.disease_number,cp.disease_name,cp.readterm
from 18_299_Lyratzopoulos_e2.cprd_clinical cl
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_random_sample rs on rs.e_patid = cl.e_patid
LEFT JOIN lookup_tables.lookup_cal_cprd cp on cp.medcode = cl.medcode
WHERE cl.eventdate >= makedate(2005, 1) AND cl.eventdate <= makedate(2018, 183)
AND cp.disease_number IN (162,101,34,106,97,162)   
; 

DROP TABLE IF EXISTS freya.disease_index_cprd_elig;
CREATE TABLE freya.disease_index_cprd_elig
SELECT v.*, pr.lcd, pr.uts, pt.deathdate, pt.yob, pt.mob, pt.crd, pt.gender, pt.tod
FROM freya.disease_index_cprd v
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold eg ON v.e_patid = eg.e_patid
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_practice pr on eg.e_pracid = pr.e_pracid
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_patient pt on pt.e_patid = v.e_patid
;

-- Now HES 
DROP TABLE IF EXISTS freya.disease_index_hes;
Create table freya.disease_index_hes
select cl.e_patid, cl.epistart, cl.icd, rc.icd10codedescr,rc.disease_number,rc.disease_name
from 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_epi cl
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_random_sample rs on rs.e_patid = cl.e_patid
LEFT JOIN lookup_tables.lookup_cal_hes_icd rc on cl.icd = rc.icd10code
WHERE cl.epistart >= makedate(2005, 1) AND cl.epistart <= makedate(2018, 183)
AND (rc.disease_number IN (162,101,34,107,97,162)) OR (cl.icd LIKE "K29%") or (cl.icd LIKE("K44%"))
; 

DROP TABLE IF EXISTS freya.disease_index_hes_elig;
CREATE TABLE freya.disease_index_hes_elig
SELECT v.e_patid,v.epistart,v.disease_name,v.disease_number,v.icd10codedescr, v.icd, pr.lcd, pr.uts, pt.deathdate, pt.yob, pt.mob, pt.crd, pt.gender, pt.tod
FROM freya.disease_index_hes v
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold eg ON v.e_patid = eg.e_patid
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_practice pr on eg.e_pracid = pr.e_pracid
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_patient pt on pt.e_patid = v.e_patid
;

-- union the two tables together with a column indicating the source
drop table if exists  freya.disease_index_all;
create table freya.disease_index_all
Select *
from

(
select h.e_patid, h.epistart as eventdate, h.disease_name, h.icd10codedescr as code_desc,h.lcd, h.uts, h.deathdate, h.yob, h.mob, h.gender, h.crd, h.tod,
case when e_patid is not null then "hes" else null end as data_source, 
icd as code_number
from freya.disease_index_hes_elig h

UNION ALL

select d.e_patid, d.eventdate, d.disease_name, d.readterm, d.lcd, d.uts, d.deathdate, d.yob, d.mob, d.gender, d.crd, d.tod,
case when e_patid is not null then "cprd" else null end as data_source,
medcode as code_number
from freya.disease_index_cprd_elig d
) as tablename;


-- adding in 'reflux' patients from stomach disorders characterisation
DROP TABLE IF EXISTS freya.disease_index_reflux;
CREATE TABLE freya.disease_index_reflux
SELECT * FROM freya.cohortv1_1_2
WHERE readcode_desc LIKE "%reflux%"
AND symptom_desc = "Stomach disorders";

-- Go to R script 3.1