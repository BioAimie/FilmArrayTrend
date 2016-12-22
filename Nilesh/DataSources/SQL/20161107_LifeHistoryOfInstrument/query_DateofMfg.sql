SET NOCOUNT ON

SELECT 
	[LotNumber] AS [SerialNo],
	CAST([DateOfManufacturing] AS DATE) AS [ManufactureDate]
FROM [ProductionWeb].[dbo].[Lots] L WITH(NOLOCK)
WHERE [LotNumber] LIKE 'FA2127'