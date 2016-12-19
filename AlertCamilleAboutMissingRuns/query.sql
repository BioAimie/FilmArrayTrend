SET NOCOUNT ON

SELECT
	S.[Note],
	MAX(R.[StartTime]) AS [LastRunStart],
	MAX(R.[DateImported]) AS [LastTimeImported],
	DATEPART(mi, CAST(GETDATE() - MAX(R.[DateImported]) AS FLOAT)) AS [MinutesBetweenLastImportAndCurrentTime]
INTO #lastImportMoreThanTwoDaysAgo
FROM [FADataWarehouse].[dbo].[RunData] R INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] C 
	ON R.[ConnectorLaptopId] = C.[ConnectorLaptopId] INNER JOIN [FADataWarehouse].[dbo].[CustomerSites] S
		ON C.[CustomerSiteId] = S.[CustomerSiteId]
GROUP BY S.[Note]
HAVING (DATEDIFF(dd, MAX(R.[DateImported]), GETDATE()) >= 2)
ORDER BY [Note], [MinutesBetweenLastImportAndCurrentTime]

SELECT
	S.[Note],
	SUM(IIF(R.[HasBeenCollected] = 0, 1, 0)) AS [RunsNotYetCollected]
INTO #runsNotYetCollected
FROM [FADataWarehouse].[dbo].[RunData] R INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] C 
	ON R.[ConnectorLaptopId] = C.[ConnectorLaptopId] INNER JOIN [FADataWarehouse].[dbo].[CustomerSites] S
		ON C.[CustomerSiteId] = S.[CustomerSiteId]
WHERE R.[StartTime] > GETDATE() - 30
GROUP BY S.[Note]

SELECT 
	R.[Note] AS [Site],
	R.[RunsNotYetCollected] AS [RunsNotYetCollectedLast30Days],
	L.[LastRunStart],
	L.[LastTimeImported],
	L.[MinutesBetweenLastImportAndCurrentTime] AS [MinutesBetweenLastImportAndNowForSitesWithMostRecentExportTwoOrMoreDaysAgo]
FROM #runsNotYetCollected R LEFT JOIN #lastImportMoreThanTwoDaysAgo L
	ON R.[Note] = L.[Note]
ORDER BY R.[RunsNotYetCollected] DESC

DROP TABLE #lastImportMoreThanTwoDaysAgo, #runsNotYetCollected