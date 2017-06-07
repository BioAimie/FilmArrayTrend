SET NOCOUNT ON

SELECT
	R.[RunDataId],
	P.[CustomerSiteId],
	R.[PouchTitle],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[InstrumentSerialNumber] AS [SerialNo],
	--R.[PositiveAssays],
	--R.[PositiveGenes],
	(R.[PositiveAssays] + R.[PositiveGenes]) AS [PositiveTargets],
	1 AS [Run]
INTO #master
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) LEFT JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId]
WHERE R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 0 AND 4 AND R.[PouchResult] = 'Pass'
GROUP BY
	R.[RunDataId],
	R.[PouchTitle],
	P.[CustomerSiteId],
	R.[StartTime],
	R.[InstrumentSerialNumber],
	R.[PositiveAssays],
	R.[PositiveGenes]

SELECT
	[RunDataId],
	[CustomerSiteId],
	[Date],
	CASE 
		WHEN [PouchTitle] LIKE 'BCID %' THEN 'BCID'
		WHEN [PouchTitle] LIKE 'ME %' THEN 'ME'
		WHEN [PouchTitle] LIKE 'GI %' THEN 'GI'
		ELSE 'RP'
	END AS [Panel],
	[SerialNo],
	[Run],
	IIF([PositiveTargets] > 0, 1, 0) AS [Positive]
FROM #master M
ORDER BY [Date], [RunDataId]

DROP TABLE #master