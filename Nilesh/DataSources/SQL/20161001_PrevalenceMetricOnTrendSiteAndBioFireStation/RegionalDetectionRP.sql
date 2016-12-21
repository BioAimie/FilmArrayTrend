USE [FADataWarehouse]
GO

SELECT 
	[CustomerSiteId],
	[Year],
	[Week],
	[Interpretation],
	IIF([CenteredRuns] < 10, 0, [CenteredRuns]) AS [CenteredRuns],
	IIF([CenteredRuns] < 10, 0, [CenteredPositives]) AS [CenteredPositives],
	IIF([CenteredRuns] < 10, 0, [CenteredRate]) AS [CenteredRate]
INTO #master
FROM [dbo].[SitePercentDetectionRP] WITH(NOLOCK) 

SELECT *
INTO #regionTable
FROM [dbo].[EpiRegions] WITH(NOLOCK) 

-- Create table #gapFinder
SELECT DISTINCT
	[CustomerSiteId],
    [Year],
    [Week],
    [CenteredRuns]
INTO #gapFinder
FROM #master

-- Find most recent gap date for each customer
SELECT 
    [CustomerSiteId],
    LEAD(IIF([Week] > 9, CONCAT([Year], [Week]), CONCAT([Year], 0, [Week])), 1) OVER(ORDER BY [CustomerSiteId]) AS [ConcatDate],
    [Gap]
INTO #gapFlagged
FROM
(
    SELECT 
        [CustomerSiteId],
        [Year],
        [Week],
        IIF([Total] = 0, 1, 0) AS [Gap]
    FROM
    (
        SELECT *,
            ([CenteredRuns] + [LagCenteredRuns] + [LeadCenteredRuns]) AS [Total]
        FROM
        (
            SELECT 
				[CustomerSiteId], 
				[Year], 
				[Week], 
				[CenteredRuns],
                IIF([LagCenteredRuns1] IS NULL, 0, [LagCenteredRuns1]) AS [LagCenteredRuns],
                IIF([LeadCenteredRuns1] IS NULL, 0, [LeadCenteredRuns1]) AS [LeadCenteredRuns]
            FROM
            (
                SELECT *,
                    LAG([CenteredRuns]) OVER(ORDER BY [CustomerSiteId], [Year], [Week]) AS [LagCenteredRuns1],
                    LEAD([CenteredRuns]) OVER(ORDER BY [CustomerSiteId], [Year], [Week]) AS [LeadCenteredRuns1]
                FROM #gapFinder
            )T1
        )T2
    )T3
)T4

-- Start date for [CustomerSiteId] that have a gap
SELECT 
    [CustomerSiteId],
	MAX([ConcatDate]) AS [SiteStartDate]
INTO #gappedSites
FROM #gapFlagged
WHERE [Gap] = 1
GROUP BY [CustomerSiteId]

-- Start date for [CustomerSiteId] that do not have a gap
SELECT 
    [CustomerSiteId],
    MIN([ConcatDate]) AS [SiteStartDate]
INTO #nongappedSites
FROM #gapFlagged
WHERE [CustomerSiteId] NOT IN (SELECT [CustomerSiteId] FROM #gappedSites)
GROUP BY [CustomerSiteId]

-- Combine the above two tables to get start date for all sites 'with' and 'without gap'
SELECT *
INTO #siteStartDates
FROM
(
    SELECT *
    FROM #gappedSites
    UNION 
    SELECT *
    FROM #nongappedSites
) T

-- Join the above start dates table '#sitestartdates' to '#master'. This table filters out all gaps to get clean data
SELECT 
    M.[CustomerSiteId],
    M.[ConcatDate],
    M.[Interpretation],
	[CenteredRate]
INTO #usableData
FROM
(
    SELECT 
        [CustomerSiteId],
		IIF([Week] > 9, CONCAT([Year], [Week]), CONCAT([Year], 0, [Week])) AS [ConcatDate],
		[Interpretation],
        [CenteredRate]
    FROM #master 
) M LEFT JOIN #siteStartDates S
       ON M.[CustomerSiteId] = S.[CustomerSiteId]
WHERE M.[ConcatDate] >= S.[SiteStartDate]

-- Join #usableData to get census region information
SELECT 
	U.[CustomerSiteId],
	U.[ConcatDate],
    U.[Interpretation],
	U.[CenteredRate],
	[CensusRegionLocal] AS [Region]
INTO #metaTable
FROM #usableData U INNER JOIN [FADataWarehouse].[dbo].[CustomerSites] cs WITH(NOLOCK) 
	ON U.[CustomerSiteId] = cs.[CustomerSiteId] INNER JOIN [Customers] c WITH(NOLOCK)
		ON cs.[CustomerId] = c.[CustomerId] INNER JOIN #regionTable rt
			ON rt.[StateAbv] = c.[Province]

-- Calculate average centered runs
SELECT 
	[Region],
	CAST(SUBSTRING(CAST([ConcatDate] AS VARCHAR(20)), 1, 4) AS INT) AS [Year],
	CAST(SUBSTRING(CAST([ConcatDate] AS VARCHAR(20)), 5, 6) AS INT) AS [Week],
	[Interpretation],
	AVG([CenteredRate]) AS [RegionalAvgCenteredRate]
FROM
(
	SELECT 
		M.[CustomerSiteId],
		M.[ConcatDate],
		M.[Interpretation],
		M.[CenteredRate],
		M.[Region],
		T2.[RegionStartDate]
	FROM #metaTable M INNER JOIN
	(
		-- Find the region start date as minimum of [ConcatDate]
		SELECT 
			[Region],
			MIN([ConcatDate]) AS [RegionStartDate]
		FROM
		(
			-- Filter out data where sites less than 3 in each region by counting sites for each region and [ConcatDate]
			SELECT
				[Region],
				[ConcatDate],
				COUNT(DISTINCT [CustomerSiteId]) AS [ParticipatingSites]
			FROM #metaTable
			WHERE [ConcatDate] IS NOT NULL
			GROUP BY [Region], [ConcatDate]
			HAVING COUNT(DISTINCT [CustomerSiteId]) >= 3
		) T
		GROUP BY [Region]
	)T2 ON M.[Region] = T2.[Region]
)T3
WHERE [ConcatDate] >= [RegionStartDate]
GROUP BY [Region], [Interpretation], [ConcatDate]
ORDER BY [Region], [Interpretation], [Year], [Week]

DROP TABLE #regionTable, #master, #gapFinder, #gappedSites, #gapFlagged, #siteStartDates, #nongappedSites, #metaTable, #usableData