SET NOCOUNT ON

SELECT DISTINCT
	REPLACE(REPLACE([PouchLotNumber],'`',''),'''','') AS [PouchLotNumber] 
FROM [FADataWarehouse].[dbo].[RunData] R WITH(NOLOCK)
WHERE R.[PouchTitle] LIKE '%Resp%' AND R.[RunStatus] = 'Completed' AND R.[PositiveAssays] BETWEEN 0 AND 4