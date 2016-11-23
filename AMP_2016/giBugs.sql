USE [FADataWarehouse]
GO

SELECT
	R.[RunDataId],
	L.[CustomerSiteId],
	R.[StartTime], 
	R.[PositiveAssays],
	B.[Interpretation] AS [Bug],
	1 AS [Record]
FROM [dbo].[RunData] R WITH(NOLOCK) INNER JOIN [dbo].[ConnectorLaptops] L WITH(NOLOCK)
	ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId] INNER JOIN [dbo].[SummarizedPositiveAssayResults] B WITH(NOLOCK)
		ON R.[RunDataId] = B.[RunDataId]
WHERE [PouchTitle] LIKE '%GI%' AND R.[SuppressState] LIKE 'Trendable' AND B.[ResultType] NOT LIKE 'Control'
ORDER BY R.[RunDataId]