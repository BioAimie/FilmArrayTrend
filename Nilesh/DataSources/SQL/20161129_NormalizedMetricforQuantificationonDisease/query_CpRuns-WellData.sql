SET NOCOUNT ON

SELECT
	[EpiYear] AS [Year],
	[EpiWeek] AS [Week],
	IIF([EpiWeek] > 9, CONCAT([EpiYear], [EpiWeek]), CONCAT([EpiYear], 0, [EpiWeek])) AS [ConcatDate],
	T2.*
FROM
(
	SELECT
		CAST(R.[StartTime] AS DATE) AS [RunDate],
		T.*,
		R.[PositiveAssays],
		R.[PouchLotNumber],
		R.[PouchSerialNumber]
	FROM
	(
		SELECT DISTINCT
			P.[CustomerSiteId],
			W.[RunDataId],
			W.[WellDataId],
			W.[WellName],
			W.[ResultType],
			W.[Name] AS [AssayName],
			W.[Result] AS [AssayResult],
			W.[TargetName],
			W.[TargetResult],
			IIF(W.[Cp] IS NULL OR W.[Cp] = 0, 30, [Cp]) AS [Cp]
		FROM [FADataWarehouse].[dbo].[WellData] W WITH(NOLOCK) LEFT JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] P WITH(NOLOCK) 
			ON W.[RunDataId] = P.[RunDataId]
		WHERE W.[TargetResult] = 'Positive'  OR W.[TargetResult] = 'Pass'
		--WHERE W.[ResultType] LIKE 'organism'  AND [TargetName] LIKE 'Adenovirus' AND W.[TargetResult] = 'Positive' 
	)T LEFT JOIN [FADataWarehouse].[dbo].[RunData] R 
		ON T.[RunDataId] = R.[RunDataId] 
		WHERE R.[RunStatus] LIKE 'Completed' AND R.[PositiveAssays] < 3 AND [PouchLotNumber] LIKE '%332916%'
)T2 RIGHT JOIN [FADataWarehouse].[dbo].[EpiWeeks] E WITH(NOLOCK) 
	ON T2.[RunDate] = E.[Date]
WHERE E.[Date] >= (SELECT MIN(CAST([StartTime] AS DATE)) FROM [FADataWarehouse].[dbo].[RunData] WITH(NOLOCK) WHERE [Cp] IS NOT NULL) 
	AND E.[Date] <= CAST(GETDATE() AS DATE)
ORDER BY [Year], [Week], [RunDataId]

