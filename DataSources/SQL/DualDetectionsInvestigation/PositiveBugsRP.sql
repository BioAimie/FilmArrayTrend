SET NOCOUNT ON

SELECT
	R.[RunDataId],
	R.[PositiveAssays]
INTO #rpRuns
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK)
WHERE R.[RunStatus] LIKE 'Completed' AND R.[PouchTitle] LIKE '%Resp%' AND R.[PositiveAssays] BETWEEN 0 AND 4

SELECT
	[RunDataId],
	IIF([Interpretation] = 'Influenza A (no subtype detected)', 'Influenza A', [Interpretation]) AS [BugPositive]
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] WITH(NOLOCK)
WHERE [ResultType] NOT LIKE 'Control' AND [RunDataId] IN (SELECT [RunDataId] FROM #rpRuns)

DROP TABLE #rpRuns