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
	M.[CenteredRate]
INTO #usableData
FROM
(
	SELECT 
		[CustomerSiteId],
		IIF([Week] > 9, CONCAT([Year], [Week]), CONCAT([Year], 0, [Week])) AS [ConcatDate],
		[Interpretation],
		IIF([CenteredRuns] = 0, NULL, [CenteredRate]) AS [CenteredRate] -- added by Aimie
	FROM #master 
) M LEFT JOIN #siteStartDates S
       ON M.[CustomerSiteId] = S.[CustomerSiteId]
WHERE M.[ConcatDate] >= S.[SiteStartDate]

-- Filter out data with less than '3' sites
SELECT 
    U.[CustomerSiteId],
    U.[ConcatDate],
    U.[Interpretation],
    U.[CenteredRate]
INTO #masterPrime
FROM #usableData U LEFT JOIN
(
	-- Count [CustomerSiteId] at each [ConcatDate]
	SELECT 
		[ConcatDate],
		COUNT(DISTINCT [CustomerSiteId]) AS [ParticipatingSites]
	FROM #usableData
	GROUP BY [ConcatDate]
	HAVING COUNT(DISTINCT [CustomerSiteId]) >= 3
) S
	ON  U.[ConcatDate] = S.[ConcatDate]
WHERE S.[ConcatDate] IS NOT NULL AND S.[ParticipatingSites] >= 3

-- Calculate the national average centered rate.
SELECT
	[Interpretation],
	CAST(SUBSTRING(CAST([ConcatDate] AS VARCHAR(10)), 1, 4) AS INT) AS [Year],
	CAST(SUBSTRING(CAST([ConcatDate] AS VARCHAR(10)), 5, 6) AS INT) AS [Week],
	AVG([CenteredRate]) AS [NationalCenteredRate]
FROM #masterPrime
GROUP BY [Interpretation], [ConcatDate]
ORDER BY [Interpretation], [ConcatDate]

DROP TABLE #master, #gapFinder, #gappedSites, #gapFlagged, #siteStartDates, #nongappedSites, #masterprime, #usableData