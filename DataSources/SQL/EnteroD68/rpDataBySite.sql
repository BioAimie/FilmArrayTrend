SET NOCOUNT ON

SELECT
	R.[RunDataId],
	P.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [SerialNo],
	R.[PositiveAssays],
	R.[PositiveGenes],
	W.[TargetName],
	W.[Name] AS [AssayName],
	IIF(W.[Cp] IS NULL OR W.[Cp] = 0, 30, W.[Cp]) AS [Cp],
	IIF(P.[ResultType] = 'control', 'Control', 'Organism') AS [AssayType]
INTO #master
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
		ON R.[RunDataId] = W.[RunDataId] AND P.[Interpretation] = W.[TargetName] AND P.[ResultType] = W.[ResultType]
WHERE R.[PouchTitle] = 'Respiratory Panel v1.7' AND R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 1 AND 4 AND W.[Result] IN ('Positive','Pass') AND W.[IsHidden] = 0 AND P.[CustomerSiteId] = SITE_INDEX

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
ORDER BY [RunDataId], [AssayName]

DROP TABLE #master