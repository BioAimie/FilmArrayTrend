SET NOCOUNT ON

SELECT
	R.[RunDataId],
	R.[PositiveAssays],
	CAST(R.[StartTime] AS DATE) AS [Date],
	L.[CustomerSiteId],
	C.[Name],
	C.[Province] AS [State],
	C.[City],
	C.[ZipCode]
INTO #rpRuns
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L
	ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId] INNER JOIN [FADataWarehouse].[dbo].[CustomerSites] S
		ON L.[CustomerSiteId] = S.[CustomerSiteId] INNER JOIN [FADataWarehouse].[dbo].[Customers] C
			ON S.[CustomerId] = C.[CustomerId]
WHERE R.[PouchTitle] LIKE 'Resp%' AND
	  R.[PouchResult] <> 'Fail' AND 
	  R.[PositiveAssays] BETWEEN 0 AND 4 

SELECT 
	R.[RunDataId],
	R.[Date],
	R.[CustomerSiteId],
	R.[Name],
	R.[State],
	R.[City],
	R.[ZipCode],
	IIF(R.[PositiveAssays] = 0, 'Negative', 'Positive') AS [TestResult],
	IIF(P.[Interpretation] LIKE '%Control%', 'Control', P.[Interpretation]) AS [TargetPositives]
INTO #master
FROM #rpRuns R LEFT JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId]

SELECT 
	R.[RunDataId],
	R.[Date],
	R.[CustomerSiteId],
	R.[Name],
	R.[State],
	R.[City],
	R.[ZipCode],
	R.[TestResult],
	IIF(R.[TargetPositives] = 'Control', '', R.[TargetPositives]) AS [TargetPositives]
FROM #master R
GROUP BY
	R.[RunDataId],
	R.[Date],
	R.[CustomerSiteId],
	R.[Name],
	R.[State],
	R.[City],
	R.[ZipCode],
	R.[TestResult],
	R.[TargetPositives]
ORDER BY [RunDataId]	
	
DROP TABLE #rpRuns, #master