SET NOCOUNT ON

/*
SELECT DISTINCT
	L.[CustomerSiteId],
	R.[RunDataId],
	W.[WellName],
	R.[EndTime],
	CAST(R.[EndTime] AS DATE) AS [EndDate],
	R.[PositiveAssays],
	W.[ResultType],
	W.[Name] AS [AssayName],
	W.[Result] AS [AssayResult],
	IIF(W.[Cp] IS NULL OR W.[Cp] = 0, 30, [Cp]) AS [Cp]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L WITH(NOLOCK)
		ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId] INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
				ON R.[RunDataId] = W.[RunDataId]

SELECT DISTINCT
	--R.[RunDataId],
	W.[WellName],
	W.[TargetName],
	--W.[TargetResult],
	W.[Name],
	W.[ResultType] --,
	--W.[Result],
	--W.[Cp]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
	ON R.[RunDataId] = W.[RunDataId]
WHERE R.[PouchResult] = 'Pass' AND R.[RunStatus] = 'Completed' AND R.[PouchTitle] LIKE '%BCID%'
ORDER BY [Name], [TargetName], [WellName]


SELECT
	R.[RunDataId],
	P.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [SerialNo],
	R.[PositiveAssays],
	R.[PositiveGenes],
	P.[Interpretation],
	P.[ResultType],
	I.[ShortName],
	W.[WellName],
	W.[TargetName],
	W.[Name] AS [AssayName],
	W.[Result] AS [AssayResult]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[InterpretationDefinitions] I WITH(NOLOCK)
		ON P.[Interpretation] = I.[Interpretation] INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
			ON R.[RunDataId] = W.[RunDataId] --AND P.[Interpretation] = W.[TargetName] AND P.[ResultType] = W.[ResultType]
WHERE P.[ResultType] <> 'control' AND R.[PouchTitle] LIKE '%BCID%' /*AND R.[SuppressState] = 'Trendable'*/ AND R.[PositiveAssays] BETWEEN 2 AND 4 AND R.[RunDataId] = 847
ORDER BY R.[RunDataId], W.[TargetName], W.[Name], W.[WellName]

SELECT *
FROM [FADataWarehouse].[dbo].[RunData] W WITH(NOLOCK)
WHERE [RunDataId] = 16281 

SELECT DISTINCT 
	[Interpretation]
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] W WITH(NOLOCK)
WHERE [ResultType] = 'Gene'
*/

SELECT
	R.[RunDataId],
	L.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [SerialNo],
	R.[PositiveAssays],
	R.[PositiveGenes],
	W.[TargetName],
	W.[Name] AS [AssayName],
	IIF(W.[Cp] IS NULL OR W.[Cp] = 0, 30, W.[Cp]) AS [Cp], 
	IIF(W.[Name] IN ('KPC','mecA','vanA/B'), 'Gene', 'Organism')  AS [AssayType]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
	ON R.[RunDataId] = W.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L WITH(NOLOCK)
		ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
WHERE W.[ResultType] <> 'control' AND R.[PouchTitle] LIKE '%BCID%' AND R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 1 AND 3 AND W.[TargetResult] = 'Positive' AND W.[Result] = 'Positive'
GROUP BY 
	R.[RunDataId],
	L.[CustomerSiteId],
	R.[StartTime],
	R.[PouchLotNumber],
	R.[InstrumentSerialNumber],
	R.[PositiveAssays],
	R.[PositiveGenes],
	W.[TargetName],
	W.[Name],
	W.[Cp]
