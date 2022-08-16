SELECT [Evidence Based Practice], [Federal Fiscal Year], minQtr, Staff, COUNT(*) as numConsumersFYdistinct
FROM (SELECT 
      t.[Enrollment Consumer ID], [Evidence Based Practice], [Federal Fiscal Year], MIN([Federal Fiscal Quarter]) as minQtr, Staff
      FROM Construct.[dbo].[Right Start Grant Dashboard] t 
      LEFT JOIN Reporting.dbo.Services s on t.[Enrollment Consumer ID] = S.[Consumer ID]
      LEFT JOIN Reporting.dbo.Calendar c on s.[Date] = c.Date
      WHERE [Evidence Based Practice] IS NOT NULL and s.Date > '09-30-2018' and s.[Service Program] = '449 - Right Start for CO Grant'
      GROUP BY t.[Enrollment Consumer ID], [Federal Fiscal Year], [Evidence Based Practice], Staff
) x
GROUP BY [Evidence Based Practice], [Federal Fiscal Year], minQtr, Staff