SET NOCOUNT ON

SELECT 
	R.[RunDataId],
	L.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PositiveAssays]
INTO #allRuns
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L
	ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
WHERE  R.[RunStatus] = 'Completed' AND R.[PouchTitle] LIKE '%Resp%'

SELECT 
	S.[RunDataId],
	S.[Interpretation],
	I.[ShortName]
INTO #bugs
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] S WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[InterpretationDefinitions] I WITH(NOLOCK)
	ON S.[Interpretation] = I.[Interpretation] 
WHERE S.[Interpretation] NOT LIKE '% Control%' AND [RunDataId] IN (SELECT [RunDataId] FROM #allRuns)

SELECT 
	R.[RunDataId],
	R.[CustomerSiteId],
	R.[Date],
	R.[PositiveAssays],
	IIF(B.[Interpretation] IS NULL, 'Negative', B.[Interpretation]) AS [Interpretation],
	IIF(B.[ShortName] IS NULL, 'Neg', B.[ShortName]) AS [ShortName]
FROM #allRuns R LEFT JOIN #bugs B
	ON R.[RunDataId] = B.[RunDataId]

DROP TABLE #allRuns, #bugs