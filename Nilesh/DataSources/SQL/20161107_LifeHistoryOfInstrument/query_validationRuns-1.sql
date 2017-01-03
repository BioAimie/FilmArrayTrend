SET NOCOUNT ON

SELECT
	[RunDataId],
	[StartTime],
	[PouchTitle],
	[PositiveAssays],
	[SuppressState]
INTO #runData
FROM [FADataWarehouse].[dbo].[RunData] WITH(NOLOCK)
WHERE [InstrumentSerialNumber] LIKE 'FA2127'
	AND [RunStatus] LIKE 'Completed' AND [PouchResult] LIKE 'Pass' AND [SuppressState] LIKE 'Suppress'
ORDER BY [StartTime] 

SELECT DISTINCT
	'validate' AS [Type],
	CAST([StartTime] AS DATE) AS [ValidationDate]
FROM #runData
ORDER BY [ValidationDate]

DROP TABLE #runData


