USE [FADataWarehouse]
GO

SELECT DISTINCT
	[CustomerSiteId]
INTO #master
FROM [dbo].[SitePercentDetectionRP] WITH(NOLOCK)

SELECT *
INTO #regionTable
FROM [dbo].[EpiRegions] WITH(NOLOCK)

-- Add location information
SELECT 
	M.[CustomerSiteId],
	[StateAbv],
	[CensusRegionLocal] AS [Region]
INTO #metaTable
FROM #master M INNER JOIN [FADataWarehouse].[dbo].[CustomerSites] CS WITH(NOLOCK) 
	ON M.[CustomerSiteId] = CS.[CustomerSiteId] INNER JOIN [Customers] C WITH(NOLOCK)
		ON CS.[CustomerId] = C.[CustomerId] INNER JOIN  #regionTable R
			ON R.[StateAbv] = C.[Province]

-- Bin the number of customer sites
SELECT 
	[Region],
	IIF([RegCountCustSiteId] IS NULL, 1, 0) AS [NoSites],
	IIF(([RegCountCustSiteId] > 0 AND [RegCountCustSiteId] < 5), [RegCountCustSiteId], 0) AS [1-5_Sites],
	IIF(([RegCountCustSiteId] >= 5 AND [RegCountCustSiteId] < 10), [RegCountCustSiteId], 0) AS [5-10_Sites],
	IIF([RegCountCustSiteId] >= 10, [RegCountCustSiteId], 0) AS [10+_Sites]
FROM 
(
	-- Count customer sites in each region
	SELECT 
		COUNT(DISTINCT [CustomerSiteId]) AS [RegCountCustSiteId],
		M.[Region]
	FROM #metaTable M
	GROUP BY [Region]
) A
ORDER BY [Region]

DROP TABLE #master, #regionTable, #metaTable
