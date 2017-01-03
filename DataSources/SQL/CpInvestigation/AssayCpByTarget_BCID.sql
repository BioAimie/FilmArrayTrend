SET NOCOUNT ON

SELECT
	R.[RunDataId],
	L.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [SerialNo],
	R.[PositiveAssays],
	R.[PositiveGenes],
	W.[TargetName],
	W.[WellName],
	W.[Name] AS [AssayName],
	IIF(W.[Cp] IS NULL OR W.[Cp] = 0, 30, W.[Cp]) AS [Cp],
	IIF(W.[Name] IN ('KPC','mecA','vanA/B'), 'Gene', 
		IIF(W.[ResultType] = 'control', 'Control', 'Organism')) AS [AssayType]
INTO #master
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
	ON R.[RunDataId] = W.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L WITH(NOLOCK)
		ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
WHERE R.[PouchTitle] LIKE '%BCID%' AND R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 1 AND 3 AND W.[TargetResult] IN ('Positive','Pass') AND W.[Result] IN ('Positive','Pass')

SELECT 
	M.*
FROM #master M LEFT JOIN
(
	SELECT 
		[RunDataId],
		[TargetName],
		[AssayName],
		COUNT([Cp]) AS [PositiveWells]
	FROM #master
	GROUP BY [RunDataId], [TargetName], [AssayName]
) C
	ON C.[RunDataId] = M.[RunDataId] AND C.[TargetName] = M.[TargetName] AND C.[AssayName] = M.[AssayName]
WHERE C.[PositiveWells] > 1

DROP TABLE #master