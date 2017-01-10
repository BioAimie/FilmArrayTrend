SELECT 
	[RunDataId],
	[Interpretation],
	[SummarizedPositiveAssayResultId]
FROM [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] WITH(NOLOCK)
WHERE [RunDataId] IN (384534,  30587, 381958, 410473, 647519, 647542) AND [ResultType] <> 'Control'
ORDER BY [RunDataId], [Interpretation]