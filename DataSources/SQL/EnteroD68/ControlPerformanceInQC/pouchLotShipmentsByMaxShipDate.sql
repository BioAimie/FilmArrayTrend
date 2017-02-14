
SELECT 
	[KitLot],
	MAX([ShipDate]) AS [RecentKitShipDate]
FROM
(
	SELECT
		CAST([ShipDate] AS DATE) AS [ShipDate],
		REPLACE(REPLACE(LOWER([CustName]),'''',''),' ','') AS [CustName],
		[ItemID] AS [PouchID],
		[ShortDesc] AS [PouchDesc],
		[LotNo] AS [KitLot]
	FROM [SQL1-RO].[mas500_app].[dbo].[vdvLotsShippedToCustomers_BIO] WITH(NOLOCK)
	WHERE [ItemID] IN ('RFIT-ASY-0001','RFIT-ASY-0105','RFIT-ASY-0107','RFIT-ASY-0120','RFIT-ASY-0124','RFIT-ASY-0125')
) T
GROUP BY [KitLot]