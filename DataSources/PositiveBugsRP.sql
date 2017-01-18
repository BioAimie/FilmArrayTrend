SET NOCOUNT ON

SELECT
	R.[RunDataId]
INTO #rpRuns
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK)
WHERE R.[RunStatus] LIKE 'Completed' AND R.[SuppressState] LIKE 'Trendable' AND R.[PouchTitle] LIKE '%Resp%'

SELECT
	[RunDataId],
	[Interpretation] AS [BugPositive]
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] WITH(NOLOCK)
WHERE [ResultType] NOT LIKE 'Control' AND [RunDataId] IN (SELECT [RunDataId] FROM #rpRuns)

DROP TABLE #rpRuns