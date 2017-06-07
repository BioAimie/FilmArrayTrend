SELECT
	S.[CustomerSiteId],
	C.[Name],
	S.[Note],
	R.[CDCRegion] AS [hhsRegion],
	R.[CensusRegionNational],
	R.[CensusRegionLocal],
	C.[Province] AS [State]--,
	--C.[City],
	--1 AS [Record]
	--C.[ContactEmail]
--INTO #master
FROM [FADataWarehouse].[dbo].[CustomerSites] S INNER JOIN [FADataWarehouse].[dbo].[Customers] C
	ON S.[CustomerId] = C.[CustomerId] INNER JOIN [FADataWarehouse].[dbo].[EpiRegions] R
		ON C.[Province] = R.[StateAbv]
ORDER BY S.[CustomerSiteId]
/*
SELECT 
	[CensusRegionNational],
	SUM([Record])
FROM #master
GROUP BY [CensusRegionNational]

DROP TABLE #master
*/