SET NOCOUNT ON

SELECT 
	[RunDataId]
INTO #positives
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
WHERE P.[Interpretation] = 'Human Rhinovirus/Enterovirus'

SELECT
	R.[RunDataId],
	R.[PouchSerialNumber],
	P.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [SerialNo],
	R.[PositiveAssays],
	R.[PositiveGenes],
	W.[TargetName],
	W.[Name] AS [AssayName],
	IIF(W.[MeltDetectorCall] = 'Positive' AND (W.[Cp] IS NULL OR W.[Cp] = 0), 30, 
		IIF(W.[MeltDetectorCall] = 'Positive', W.[Cp], 40)) AS [Cp],
	W.[Tm1] AS [Tm],
	W.[MaxFluor],
	IIF(P.[ResultType] = 'control', 'Control', 'Organism') AS [AssayType]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
		ON R.[RunDataId] = W.[RunDataId] AND W.[TargetName] = P.[Interpretation]
WHERE R.[RunDataId] IN (SELECT [RunDataId] FROM #positives)

DROP TABLE #positives