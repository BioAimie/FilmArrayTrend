SET NOCOUNT ON

SELECT 
       [TicketId]
INTO #serialRMAs
FROM [PMS1].[dbo].[vTrackers_AllObjectPropertiesByStatus] WITH(NOLOCK)
WHERE [ObjectName] LIKE 'Part Information' AND [PropertyName] LIKE 'Lot/Serial Number' AND [RecordedValue] LIKE 'FA2127' 

SELECT *
INTO #properties
FROM [PMS1].[dbo].[vTrackers_AllPropertiesByStatus] WITH(NOLOCK)
WHERE [PropertyName] IN ('Customer Id', 'RMA Title','RMA Type','Complaint Number', 'System Failure', 'Hours Run','Service Completed','Received Date', 'Shipping Date') AND [TicketId] IN (SELECT [TicketId] FROM #serialRMAs)

SELECT *
INTO #objects
FROM [PMS1].[dbo].[vTrackers_AllObjectPropertiesByStatus] WITH(NOLOCK)
WHERE [ObjectName] IN ('Part Information','Root Causes','QC Check') AND [TicketId] IN (SELECT [TicketId] FROM #serialRMAs)

SELECT *
INTO #pivProperties
FROM
(
       SELECT 
             [TicketId],
             [TicketString],
             [CreatedDate],
             [Status],
             [PropertyName],
             [RecordedValue]
       FROM #properties 
) P
PIVOT
(
       MAX([RecordedValue])
       FOR [PropertyName]
       IN
       (
	     [Customer Id],
         [Received Date],
         [RMA Title],
         [RMA Type],
         [Complaint Number],
		 [System Failure],
         [Hours Run],
         [Service Completed],
         [Shipping Date]
       )
) PIV

SELECT 
       [TicketId],
       [Lot/Serial Number] AS [SerialNo],
       [Early Failure Type] AS [EarlyFailure]
INTO #partInfo
FROM
(
       SELECT 
             [TicketId],
             [ObjectId],
             [PropertyName],
             [RecordedValue]
       FROM #objects
       WHERE [ObjectName] LIKE 'Part Information'
) P
PIVOT
(
       MAX([RecordedValue])
       FOR [PropertyName]
       IN
       (
             [Part Number],
             [Lot/Serial Number],
             [Early Failure Type]
       )
) PIV
WHERE [Part Number] LIKE '%FLM%-ASY-0001%' OR [Part Number] LIKE 'FA%'  OR [Part Number] LIKE 'HTFA-ASY-0003%' OR [Part Number] LIKE 'HTFA-SUB-0103%'

SELECT 
       [TicketId],
       [RecordedValue] AS [RootCause]
INTO #rootCause
FROM #objects
WHERE [ObjectName] LIKE 'Root Causes' AND [PropertyName] LIKE 'Part Number'

SELECT 
       [TicketId],
       MAX([RecordedValue]) AS [QcDate]
INTO #qcDate
FROM #objects
WHERE [ObjectName] LIKE 'QC Check' AND [PropertyName] LIKE 'QC Date'
GROUP BY [TicketId]

SELECT 
       [Customer Id] AS [CustomerID],
       [SerialNo],
       [TicketString],
       [Status],
       CAST([CreatedDate] AS DATE) AS [CreatedDate],
       CAST([Received Date] AS DATE) AS [ReceiveDate],
       CAST([Service Completed] AS DATE) AS [ServiceDate],
       CAST([QcDate] AS DATE) AS [QcDate],
       CAST([Shipping Date] AS DATE) AS [ShipDate],
       [Hours Run] AS [HoursRun],
       I.[EarlyFailure],
	   P.[System Failure],
       IIF([RMA Type] LIKE '%- Failure', 1, 
             IIF([EarlyFailure] IN ('SDOA','ELF','SELF','DOA'), 1, 
             IIF([RootCause] IS NOT NULL AND [RootCause] NOT LIKE '%n%a%', 1,
             IIF([RMA Title] LIKE '%error%' OR [RMA Title] LIKE '%fail%'OR [RMA Title] LIKE '%poor%alignment%', 1, 
             IIF(ISNUMERIC([Complaint Number]) = 1 AND [RMA Title] NOT LIKE '%loaner%', 1, 0))))) AS [Failure]
FROM #pivProperties P LEFT JOIN #partInfo I
       ON P.[TicketId] = I.[TicketId] LEFT JOIN #rootCause R
             ON P.[TicketId] = R.[TicketId] LEFT JOIN #qcDate Q
                    ON P.[TicketId] = Q.[TicketId]

DROP TABLE #serialRMAs, #objects, #properties, #pivProperties, #partInfo, #qcDate, #rootCause


