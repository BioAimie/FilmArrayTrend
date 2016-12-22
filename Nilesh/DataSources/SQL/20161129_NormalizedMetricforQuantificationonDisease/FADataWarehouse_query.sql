SET NOCOUNT ON

-- find distinct [PouchLotNumber] for [CustomerSiteId] = 7 
SELECT DISTINCT
	S.[CustomerSiteId],		
	R.[PouchTitle],	
	R.[PouchLotNumber]
	--R.[PouchSerialNumber],
	--S.[Interpretation],
	--R.[SuppressState],
	--R.[InstrumentSerialNumber]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[SummarizedPositiveAssayResults] S WITH(NOLOCK)
		ON R.[RunDataId] = S.[RunDataId]
WHERE R.[PouchTitle] LIKE 'Resp%' AND S.[CustomerSiteId] = 7 AND R.[PositiveAssays] >= 3
ORDER BY R.[PouchLotNumber] --, R.[PouchSerialNumber], S.[Interpretation]


