
SELECT *
INTO #temp
FROM
(
	SELECT 
		[RunDataId],
		[Name],
		[TargetName],
		[Result]
	FROM [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
	WHERE [TargetName] LIKE 'Influenza A H3' AND [TargetResult] LIKE 'Positive' 
	--WHERE [TargetName] LIKE 'Influenza A' AND [TargetResult] LIKE 'Negative' 
) P
PIVOT
(
	 MAX([Result])
	 FOR [Name]
	 IN
	 (
		[FluA-H3],
		[FluA-H1-2009],
		[FluA-H1-pan],
		[FluA-pan1],
		[FluA-pan2]
	)
)PIV
ORDER BY [RunDataId]

/*
SELECT *
FROM #temp 
WHERE NOT (([FluA-H1-2009] = 'Negative' AND [FluA-H1-pan] = 'Positive' AND [FluA-H3] = 'Positive' AND [FluA-pan1] = 'Positive' AND [FluA-pan2] = 'Negative')
		OR ([FluA-H1-2009] = 'Negative' AND [FluA-H1-pan] = 'Positive' AND [FluA-H3] = 'Negative' AND [FluA-pan1] = 'Positive' AND [FluA-pan2] = 'Negative')
		OR ([FluA-H1-2009] = 'Negative' AND [FluA-H1-pan] = 'Positive' AND [FluA-H3] = 'Negative' AND [FluA-pan1] = 'Positive' AND [FluA-pan2] = 'Positive'))
*/

SELECT DISTINCT
	[TargetName],		
	[FluA-H3],
	[FluA-H1-2009],
	[FluA-H1-pan],
	[FluA-pan1],
	[FluA-pan2]
FROM #temp
ORDER BY [FluA-H3], [FluA-H1-2009], [FluA-H1-pan], [FluA-pan1], [FluA-pan2]
DROP TABLE #temp
