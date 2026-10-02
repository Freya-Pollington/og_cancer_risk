-- Create table of patients with symptoms of interest
DROP TABLE IF EXISTS freya.cohortv0_1;
Create table freya.cohortv0_1
select cl.e_patid, cl.eventdate, cl.medcode, cl.d_cprdclin_key
from 18_299_Lyratzopoulos_e2.cprd_clinical cl
WHERE cl.eventdate >= makedate(2005, 1) AND cl.eventdate <= makedate(2017, 365)
;
show processlist;
CREATE INDEX `medcode` ON freya.cohortv0_1(`medcode`);

DROP TABLE IF EXISTS freya.cohortv0_2;
CREATE TABLE freya.cohortv0_2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
SELECT v0.*, rc.readcode_desc,rc.symptom_desc, rc.symptom_number
FROM freya.cohortv0_1 v0
INNER JOIN lookup_tables.lookup_echo_cohort_readcode rc on v0.medcode = rc.medcode
; 

CREATE INDEX `symptom_number` ON freya.cohortv0_2(`symptom_number`);
CREATE INDEX `medcode` ON freya.cohortv0_2(`medcode`);

-- ******************************
-- I originally took more symptoms than I ended up being interested in, now filtering that
DROP TABLE IF EXISTS freya.cohortv0_21;

CREATE TABLE freya.cohortv0_21
SELECT v0.*
FROM freya.cohortv0_2 v0
WHERE ((v0.symptom_number = 1 AND v0.medcode NOT IN (421,1181,2982,11647,16806,16868,17223,21583,35876,56085,20827,15288,7300,20475,9061,19360,22608,50662,1239,2056,7812,23756)) -- abdominal pain minus iliac, loin, suprapubic, 'lower', 'colicky' pain
or (v0.symptom_number = 5  AND v0.medcode NOT IN (592,2535,5283,14760,15579,16450,16605,16605,34836,35037,984,2281)) -- dyspepsia minus GORD and oesoph_ulc codes (some of which overlap with eachother)
or v0.symptom_number = 6 -- dysphagia
or (v0.symptom_number = 21) -- stomach disorders
or (v0.symptom_number = 18) -- AND v0.readcode IN (3628,1160,19470,18907,47809,7707,1273,11109,54079,29693,16717,3151,292,15483,4070,92,3628,3068,4931,4836,43795,15064,65031,29318)) -- cough minus viral, sputum/productive, fracture,whooping, bovine,barking-removed the edit to be inclusive
or (v0.symptom_number = 8  ) -- fatigue 
or v0.symptom_number = 15 -- weight loss
);

--

DROP TABLE IF EXISTS freya.cohortv0_3;
CREATE TABLE freya.cohortv0_3
SELECT v.*, pr.lcd, pr.uts, pt.deathdate, pt.yob, pt.mob, pt.crd, pt.gender, pt.tod
FROM freya.cohortv0_21 v
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold eg ON v.e_patid = eg.e_patid
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_practice pr on eg.e_pracid = pr.e_pracid
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_patient pt on pt.e_patid = v.e_patid
;

-- Go to R for eligible and potentially eligible flags
show processlist;
kill 326