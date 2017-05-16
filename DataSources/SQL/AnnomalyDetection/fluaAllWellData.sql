SET NOCOUNT ON

SELECT
	[RunDataId]
INTO #positives
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
WHERE P.[Interpretation] LIKE 'Influenza A%'

SELECT
	R.[RunDataId],
	R.[ExperimentRunId] AS [Id],
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
		IIF(W.[MeltDetectorCall] = 'Positive', W.[Cp], 40)) AS [Cp], -- if an assay is positive by Tm with no Cp, change to 30... if negative change to 40
	ISNULL(W.[Tm1], 0) AS [Tm], -- if an assay is negative, then change the Tm to zero
	W.[MaxFluor], -- every well should have a fluor value
	IIF(P.[ResultType] = 'control', 'Control', 'Organism') AS [AssayType]
INTO #master
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
		ON R.[RunDataId] = W.[RunDataId] AND W.[TargetName] = P.[Interpretation]
WHERE R.[RunDataId] IN (SELECT [RunDataId] FROM #positives) AND 
	  (W.[TargetName] LIKE 'Influenza A%' OR W.[Name] = 'yeastRNA') AND 
	  R.[PouchResult] <> 'Fail' AND 
	  (R.[PositiveAssays] BETWEEN 1 AND 4) AND
	  P.[CustomerSiteId] = SITE_INDEX

SELECT 
	[RunDataId],
	[Id],
	[PouchSerialNumber],
	[CustomerSiteId],
	[Date],
	[LotNo],
	[SerialNo],
	[PositiveAssays],
	[PositiveGenes],
	'Influenza A' AS [TargetName],
	[AssayName],
	[Cp], 
	[Tm],
	[MaxFluor],
	[AssayType]
INTO #masterTrim
FROM #master
GROUP BY 
	[RunDataId],
	[Id],
	[PouchSerialNumber],
	[CustomerSiteId],
	[Date],
	[LotNo],
	[SerialNo],
	[PositiveAssays],
	[PositiveGenes],
	[AssayName],
	[Cp], 
	[Tm],
	[MaxFluor],
	[AssayType]

SELECT
	M.*
FROM #masterTrim M INNER JOIN 
(
	SELECT
		[RunDataId],
		COUNT([Cp]) AS [Wells]
	FROM #masterTrim
	GROUP BY [RunDataId]
) T
	ON M.[RunDataId] = T.[RunDataId]
WHERE [Wells] = 18

DROP TABLE #positives, #master, #masterTrim