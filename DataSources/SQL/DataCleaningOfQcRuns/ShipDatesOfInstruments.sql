SET NOCOUNT ON

SELECT
	[SerialNo],
	CAST([TranDate] AS DATE) AS [Date]
FROM [PMS1].[dbo].[vSerialTransactions] WITH(NOLOCK)
WHERE ([TranType] = 'SH' OR ([TranType] IN ('SA','IS') AND [DistQty] = -1))