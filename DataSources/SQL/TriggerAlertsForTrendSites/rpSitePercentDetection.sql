SELECT
	S.[Year],
	S.[Week],
	S.[CustomerSiteId],
	S.[AssayName],
	S.[CenteredRuns] AS [Runs],
	S.[CenteredPositives] AS [Positives],
	[CenteredRate] AS [PercentDetection],
	1 AS [Record]
FROM [FADataWarehouse].[dbo].[SitePercentDetectionRP] S WITH(NOLOCK)