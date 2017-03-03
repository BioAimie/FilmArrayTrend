SELECT
	R.[RunDataId],
	P.[CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [SerialNo],
	R.[PositiveAssays],
	P.[ResultType],
	W.[Name] AS [AssayName],
	IIF(W.[Cp] IS NULL OR W.[Cp] = 0, 30, W.[Cp]) AS [Cp]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK)
	ON R.[RunDataId] = P.[RunDataId] INNER JOIN [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK)
		ON R.[RunDataId] = W.[RunDataId] AND P.[Interpretation] = W.[TargetName] AND P.[ResultType] = W.[ResultType]
WHERE R.[PouchTitle] = 'Respiratory Panel v1.7' AND R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 10 AND 16 AND W.[Result] IN ('Positive','Pass') AND W.[IsHidden] = 0
ORDER BY [RunDataId], W.[Name]