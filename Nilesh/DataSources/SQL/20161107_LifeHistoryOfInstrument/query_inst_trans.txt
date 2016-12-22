SET NOCOUNT ON

SELECT
	[SerialNo],
	[TranType],
	[TranTypeDesc],
	[WhseID],
	[CustID],
	[CustName],
	[DistQty],
	CAST([TranDate] AS DATE) AS [TranDate]
INTO #instTrans
FROM [PMS1].[dbo].[vInstrumentTransactions] WITH(NOLOCK)
WHERE [SerialNo] LIKE 'FA2127'

SELECT 
	[SerialNo],
	[TranTypeDesc],
	CONCAT([TranType],'-', [WhseID]) AS [TranID],
	[TranDate],
	[RowNo]
FROM
(
	SELECT *,
		IIF(LEAD([Flag]) OVER(ORDER BY [TranDate], [DistQty])  = 2, [Row] + 1,
			IIF([Flag] = 2, [Row] - 1, [Row])) AS [RowNo]
	FROM
	(
		SELECT *,
			ROW_NUMBER() OVER(ORDER BY [TranDate], [DistQty]) AS [Row]
		FROM(
			SELECT *,
				([DistQty] + LEAD([DistQty]) OVER(ORDER BY [TranDate], [DistQty])) AS [Flag]
			FROM #instTrans
		)T
	) T1
)T2
ORDER BY [RowNo]

DROP TABLE #instTrans