SET NOCOUNT ON

SELECT 
	[InstrumentSerialNumber] AS [SerialNo],
	[RunDataId],
	[RunStatus],
	[SuppressState],
	CAST([StartTime] AS DATE) AS [ErrorDate],
	[PouchTitle]
FROM [FADataWarehouse].[dbo].[RunData]
WHERE [RunStatus] LIKE '%error%' AND  [InstrumentSerialNumber] LIKE 'FA2127'
