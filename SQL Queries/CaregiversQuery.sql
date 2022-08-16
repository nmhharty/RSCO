SELECT distinct([Enrollment Consumer ID]), [Enrollment Consumer], [Enrollment Active?], [Enrollment Staff], EnrollmentStart, 
EnrollmentEnd, [Intake?], [Earliest Intake Date], [Data_Entry_Date], [date_start], 
[relationship_type_Value] as [Relationship to ConsumerID], CONCAT([name_manual],[name_sql]) as [Caregiver Name], 
race_ethnicity_Value as Caregiver_RaceEthnicity
FROM [AvatarDW].[MHCD].[rsc_referral] av
RIGHT JOIN Construct.dbo.[Right Start Grant Dashboard] e on av.PATID = [Enrollment Consumer ID]
WHERE [workflow_type_Value] = 'New Person'