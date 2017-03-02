SELECT
	R.[RunDataId],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [Instrument],
	CAST(R.[StartTime] AS DATE) AS [Date],
	C.[Name],
	C.[Province],
	C.[CustomerId],
	S.[CustomerSiteId]
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK) INNER JOIN [FADataWarehouse].[dbo].[ConnectorLaptops] L WITH(NOLOCK)
	ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId] INNER JOIN [FADataWarehouse].[dbo].[CustomerSites] S WITH(NOLOCK)
		ON L.[CustomerSiteId] = S.[CustomerSiteId] INNER JOIN [FADataWarehouse].[dbo].[Customers] C WITH(NOLOCK)
			ON S.[CustomerId] = C.[CustomerId]
WHERE R.[RunStatus] LIKE 'Completed' AND R.[PouchTitle] LIKE '%Resp%'