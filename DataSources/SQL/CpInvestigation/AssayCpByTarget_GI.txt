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
	'Organism'  AS [AssayType]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
			ON R.[RunDataId] = W.[RunDataId] AND P.[Interpretation] = W.[TargetName] AND P.[ResultType] = W.[ResultType]
WHERE P.[ResultType] <> 'control' AND R.[PouchTitle] LIKE '%GI%' AND R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 1 AND 4 AND W.[Result] = 'Positive'