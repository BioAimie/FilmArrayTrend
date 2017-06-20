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
ORDER BY S.[CustomerSiteId] --R.[CDCRegion], R.[CensusRegionNational] --S.[CustomerSiteId]
/*
SELECT
	--[CensusRegionNational],
	[hhsRegion],
	SUM([Record]) AS [SitesInArea]
FROM #master
GROUP BY [hhsRegion]--[CensusRegionNational]

DROP TABLE #master
*/