SET NOCOUNT ON

SELECT DISTINCT
	[InstrumentSerialNumber]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] S WITH(NOLOCK)
	ON R.[RunDataId] = S.[RunDataId]
WHERE [CustomerSiteId] = 7

