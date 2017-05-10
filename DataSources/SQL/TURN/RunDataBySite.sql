SET NOCOUNT ON

SELECT
	R.[RunDataId],
	R.[PouchTitle],
	P.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[InstrumentSerialNumber] AS [SerialNo],
	1 AS [Run]
INTO #master
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) LEFT JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId]
WHERE R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 1 AND 4 AND R.[PouchResult] <> 'Fail'
GROUP BY
	R.[RunDataId],
	R.[PouchTitle],
	P.[CustomerSiteId],
	R.[StartTime],
	R.[InstrumentSerialNumber]

SELECT
	[RunDataId],
	CASE 
		WHEN [PouchTitle] LIKE 'BCID %' THEN 'BCID'
		WHEN [PouchTitle] LIKE 'ME %' THEN 'ME'
		WHEN [PouchTitle] LIKE 'GI %' THEN 'GI'
		ELSE 'RP'
	END AS [Panel],
	[CustomerSiteId],
	[Date],
	[SerialNo],
	[Run]
FROM #master M

DROP TABLE #master