SELECT
	S.[CustomerSiteId],
	C.[Name],
	S.[Note],
	R.[CensusRegionNational],
	R.[CensusRegionLocal],
	C.[Province] AS [State],
	C.[City]
	--C.[ContactEmail]
FROM [FADataWarehouse].[dbo].[CustomerSites] S INNER JOIN [FADataWarehouse].[dbo].[Customers] C
	ON S.[CustomerId] = C.[CustomerId] INNER JOIN [FADataWarehouse].[dbo].[EpiRegions] R
		ON C.[Province] = R.[StateAbv]
ORDER BY S.[CustomerSiteId]