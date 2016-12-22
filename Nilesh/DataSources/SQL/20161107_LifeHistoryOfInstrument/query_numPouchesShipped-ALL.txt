SET NOCOUNT ON

SELECT
	[CustID],
	[CustName],
	[ItemShortDesc],
	[Panel],
	[QtyShipped],
	CAST([ShipDate] AS DATE) AS [ShipDate]
FROM [PMS1].[dbo].[vPouchShipmentsWithAnnotations_IOID] WITH(NOLOCK)
WHERE [ProductClass] LIKE 'IVD' AND [CustID] LIKE 'INTHEA'
ORDER BY [ShipDate]




