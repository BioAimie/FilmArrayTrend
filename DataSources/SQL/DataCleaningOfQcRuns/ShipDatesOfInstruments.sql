SET NOCOUNT ON
/*
SELECT DISTINCT
	[TranID],
	REPLACE([CustID],' ','') AS [CustID],
	REPLACE([CustName],' ','') AS [CustName]
INTO #Customers
FROM [SQL1-RO].[mas500_app].[dbo].[vdvShipmentLine] T WITH(NOLOCK)	
WHERE (T.[ItemID] LIKE 'FLM1-ASY-0001%' OR T.[ItemID] LIKE 'FLM2-ASY-0001%' OR T.[ItemID] LIKE 'HTFA-ASY-0003%' OR T.[ItemID] LIKE 'HTFA-ASY-0001%')

SELECT
	[TranID],
	[SerialNo],
	CAST([TranDate] AS DATE) AS [Date]
INTO #shipments
FROM [PMS1].[dbo].[vSerialTransactions] WITH(NOLOCK)
WHERE ([TranType] = 'SH' OR ([TranType] IN ('SA','IS') AND [DistQty] = -1))

SELECT 
	S.[SerialNo],
	S.[Date], 
	C.[CustID]
FROM #shipments S LEFT JOIN #Customers C
	ON S.[TranID] = C.[TranID]
WHERE 
(
	[CustID] LIKE '%INTHEA%' OR
	[CustID] = 'MEDUSC' OR
	[CustID] LIKE '%NORUNI' OR
	[CustID] LIKE '%NORLAB' OR
	[CustID] LIKE '%ALBMED' OR
	[CustID] LIKE '%SOUBEN' OR
	[CustID] LIKE 'SUNYHOS' OR
	[CustID] LIKE '%WINHOS' OR
	[CustID] LIKE '%WINTRES' OR
	[CustID] LIKE '%NATCHI' OR
	[CustID] LIKE '%CHILOS' OR
	[CustID] LIKE '%CHIMER' OR
	[CustID] = 'DETMED' OR
	[CustID] = 'UCSD' OR
	[CustID] = 'UNICSD' OR
	[CustID] LIKE '%BAYHEA' OR
	[CustID] LIKE '%NYUMED' OR
	[CustID] LIKE '%NEBMED' OR
	[CustID] = 'UNINMC' OR
	[CustID] IS NULL
)

DROP TABLE #Customers, #shipments
*/

SELECT
	[SerialNo],
	CAST([TranDate] AS DATE) AS [Date]
INTO #shipments
FROM [PMS1].[dbo].[vSerialTransactions] WITH(NOLOCK)
WHERE ([TranType] = 'SH' OR ([TranType] IN ('SA','IS') AND [DistQty] = -1))