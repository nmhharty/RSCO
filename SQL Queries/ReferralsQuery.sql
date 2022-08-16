SELECT distinct([Enrollment Consumer ID]), [Enrollment Consumer], [Enrollment Active?], [Enrollment Staff], EnrollmentStart, EnrollmentEnd, [Intake?], 
[Earliest Intake Date], [Data_Entry_Date],
[date_start], [relationship_type_Value] as [Relationship], CONCAT([name_manual],[name_sql]) as [CaregiverName], 
[referral_type_Value] as [ReferralType], [date_referral], [date_follow_up], 
[external_details] as [External_Referral_Name], [mhcd_referral] as [MHCD_Referral_Number],[mhcd_referral_Value] as [MHCD_Referral_Team],
[referral_outcome_Value] as [Outcome], [relationship_status_Value] as [RelationshipActive], 
[workflow_type_Value] as [Entry_Type]
FROM [AvatarDW].[MHCD].[rsc_referral] av
RIGHT JOIN	Construct.dbo.[Right Start Grant Dashboard] e on av.PATID = [Enrollment Consumer ID]
WHERE [referral_type_Value] IS NOT NULL