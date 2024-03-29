SELECT DISTINCT 
	L.[CustomerSiteId],
	R.[PouchTitle]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L WITH(NOLOCK)
	ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
WHERE [PouchTitle] = 'Respiratory Panel v1.7'