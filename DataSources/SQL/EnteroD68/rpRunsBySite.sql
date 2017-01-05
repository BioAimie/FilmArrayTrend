SET NOCOUNT ON

SELECT
	R.[RunDataId],
	P.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	1 AS [Run]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId]
WHERE R.[PouchTitle] = 'Respiratory Panel v1.7' AND R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 0 AND 4