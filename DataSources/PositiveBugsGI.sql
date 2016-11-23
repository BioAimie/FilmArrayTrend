SET NOCOUNT ON

SELECT
	R.[RunDataId]
INTO #giRuns
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK)
WHERE R.[RunStatus] LIKE 'Completed' AND R.[SuppressState] LIKE 'Trendable' AND R.[PouchTitle] LIKE '%GI%'

SELECT
	[RunDataId],
	[ResultType],
	[Interpretation] AS [BugPositive]
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] S WITH(NOLOCK)
WHERE [ResultType] NOT LIKE 'Control' AND [RunDataId] IN (SELECT [RunDataId] FROM #giRuns)

DROP TABLE #giRuns