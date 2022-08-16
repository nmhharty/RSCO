SELECT [Master Consumer ID], [Most Recent BHA Date], [Enrollment Active?], EnrollmentStart, [Intake?], [Receive Services?], 
[Date of Birth], [Gender],  [Race and Ethnicity Summary], [Primary Language], [Service Language]
FROM
(SELECT Coalesce([BHA Consumer ID], [Enrollment Consumer ID]) as [Master Consumer ID], [Most Recent BHA Date], [Enrollment Active?],
  [EnrollmentStart], [Intake?], [Receive Services?]
  FROM Construct.dbo.[Right Start Grant Dashboard]) x
LEFT JOIN Reporting.dbo.[Consumer Summary] cs 
on cs.[Consumer ID] = [Master Consumer ID]