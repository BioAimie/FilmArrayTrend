SET NOCOUNT ON

SELECT 
	[RunDataId]
INTO #keepRuns
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK)
WHERE R.[RunStatus] LIKE 'Completed' AND 
(
	(R.[PositiveAssays] BETWEEN 1 AND 3 AND R.[PouchTitle] LIKE '%Resp%') OR
	(R.[PositiveAssays] BETWEEN 1 AND 3 AND R.[PouchTitle] LIKE '%BCID%') OR
	(R.[PositiveAssays] BETWEEN 1 AND 3 AND R.[PouchTitle] LIKE '%ME%') OR
	(R.[PositiveAssays] BETWEEN 1 AND 5 AND R.[PouchTitle] LIKE '%GI%')
)

SELECT 
	[RunDataId],
	[TargetName] AS [Target],
	[Name] AS [Assay],
	IIF([Cp] IS NULL OR [Cp] = 0, 30, [Cp]) AS [Cp]
FROM [FADataWarehouse].[dbo].[WellData] WITH(NOLOCK)
WHERE [RunDataId] IN (SELECT [RunDataId] FROM #keepRuns) AND [IsHidden] = 0 AND [TargetResult] LIKE 'Positive' AND [Result] LIKE 'Positive'

DROP TABLE #keepRuns