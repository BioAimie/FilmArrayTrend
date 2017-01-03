SET NOCOUNT ON

SELECT
	[RunDataId],
	[StartTime],
	[PouchTitle],
	[PositiveAssays],
	[SuppressState]
INTO #runData
FROM [FADataWarehouse].[dbo].[RunData]
WHERE [InstrumentSerialNumber] LIKE 'FA2127'
	AND [RunStatus] LIKE 'Completed' AND [PouchResult] LIKE 'Pass' AND [SuppressState] LIKE 'Trendable'
ORDER BY [StartTime] 

SELECT 
	[Interpretation],
	[ResultType],
	[RunDataId]
INTO #assayPositives
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] 
WHERE [RunDataId] IN (SELECT [RunDataId] FROM #runData) AND [ResultType] NOT LIKE 'Control'
ORDER BY [RunDataId] 

SELECT DISTINCT
	'patient run' AS [Type],
	CAST(R.[StartTime] AS DATE) AS [PatientSampleRunDate]
FROM #runData R INNER JOIN #assayPositives A 
	ON R.[RunDataId] = A.[RunDataId]
ORDER BY [PatientSampleRunDate]

DROP TABLE #runData, #assayPositives