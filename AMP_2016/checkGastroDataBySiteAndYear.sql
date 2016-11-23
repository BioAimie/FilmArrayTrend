SELECT 
	YEAR([StartTime]) AS [Year],
	MONTH([StartTime]) AS [Month],
	[CustomerSiteId],
	COUNT(DISTINCT [RunDataId]) AS [Runs]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L WITH(NOLOCK)
	ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
WHERE [PouchTitle] LIKE 'GI %' AND R.[ExperimentStatus] LIKE 'Completed' AND R.[SuppressState] LIKE 'Trendable' AND YEAR([StartTime]) > 2014
GROUP BY YEAR([StartTime]), MONTH([StartTime]), [CustomerSiteId]
ORDER BY YEAR([StartTime]), MONTH([StartTime]), [CustomerSiteId]