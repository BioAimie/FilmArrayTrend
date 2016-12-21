SET NOCOUNT ON

DECLARE @DateOfManufacture DATE, @DateTo DATE;
SET @DateOfManufacture = CONVERT(DATE, '2012-06-20');
SET @DateTo = CAST(GETDATE() AS DATE);
WITH T(date)
AS
(
    SELECT @DateOfManufacture
    UNION ALL
    SELECT DATEADD(day, 1, T.date) FROM T WHERE T.date < @DateTo
)
SELECT 
    date AS [Date]
INTO #calendar
FROM T
GROUP BY date, YEAR(date), DATEPART(dy,date)
OPTION(MAXRECURSION 32767);

SELECT 
	[InstrumentSerialNumber] AS [SerialNo],
	IIF([RunStatus] LIKE '%Error%', 'Error', 
		IIF([SuppressState] LIKE 'Suppress', 'Validation',
		IIF([RunStatus] LIKE 'Completed', 'Completed', NULL))) AS [Status],
	CAST([StartTime] AS DATE) AS [Date],
	[PouchTitle],
	1 AS [Record]
INTO #runDataRaw
FROM [dbo].[RunData] R WITH(NOLOCK) INNER JOIN [dbo].[ConnectorLaptops] L WITH(NOLOCK)
       ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
WHERE [InstrumentSerialNumber] LIKE 'FA2127' AND [RunStatus] <> 'Aborted'


SELECT 
	[SerialNo],
	[Status],
	IIF([Status] LIKE 'Completed', 'PatientRun', NULL) AS [PatientRuns],
	CAST(C.[Date] AS DATE) AS [Date],
	IIF(T.[Runs] IS NULL, 0, T.[Runs]) AS [Runs]
FROM #calendar C LEFT JOIN
(
	SELECT
		[SerialNo],
		[Status],
		[Date],
		SUM([Record]) AS [Runs]
	FROM #runDataRaw
	GROUP BY [SerialNo], [Status], [Date]
)T ON C.[Date] = T.[Date]
WHERE [Status] IS NOT NULL
ORDER BY [Date], [Status]

DROP TABLE #calendar, #runDataRaw