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
	[Interpretation] AS [Target],
	[ResultType]
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] WITH(NOLOCK)
WHERE [RunDataId] IN (SELECT [RunDataId] FROM #keepRuns)

DROP TABLE #keepRuns