SELECT
	S.[CustomerSiteId],
	C.[Name],
	S.[Note],
	C.[Province] AS [State],
	C.[City],
	C.[ContactEmail]
FROM [FADataWarehouse].[dbo].[CustomerSites] S INNER JOIN [FADataWarehouse].[dbo].[Customers] C
	ON S.[CustomerId] = C.[CustomerId]