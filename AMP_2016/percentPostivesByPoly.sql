SET NOCOUNT ON

SELECT 
	[RunDataId],
	[PositiveAssays],
	1 AS [Record]
INTO #trendruns
FROM [FADataWarehouse].[dbo].[RunData] WITH(NOLOCK)
WHERE [PositiveAssays] < 5 AND [PouchTitle] LIKE '%GI%' AND YEAR([StartTime]) = 2016

DECLARE @runCount INT; 
SELECT @runCount = COUNT([RunDataId]) FROM #trendruns

SELECT 
	[PositiveAssays],	
	SUM([Record]) AS [Count],
	@runCount AS [TotalRuns]
FROM #trendruns
GROUP BY [PositiveAssays]
ORDER BY [PositiveAssays]

DROP TABLE #trendruns