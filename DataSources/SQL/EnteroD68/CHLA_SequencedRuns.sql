SET NOCOUNT ON

SELECT 
	R.[Id],
	R.[PouchSerialNumber],
	25 AS [CustomerSiteId],
	CAST(R.[StartTime] AS DATE) AS [Date],
	R.[PouchLotNumber] AS [LotNo],
	R.[InstrumentSerialNumber] AS [SerialNo]
INTO #runs
FROM [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] R WITH(NOLOCK)
WHERE [PouchSerialNumber] IN
(
	'07357638', '07357723', '07357726', '07357698', '07357716', '07357697', '07357644', '07357685', '07357703', '07357641', '07357674', '07357720', '07357713', '07357686', '07357700', '07357669', '07357706', 
	'07357689', '07357725', '07357670', '07357667', '07357647', '07357705', '07357704', '07357712', '07357630', '07357650', '07357691', '07357661', '07357690', '07357678', '07357715', '07357640', '07357648', 
	'07357711', '07357662', '07357694', '07357682','07357692', '07357718', '07357672', '07357696'
)

SELECT
	ER.[Id],
	'Human Rhinovirus/Enterovirus' AS [TargetName],
	AA.[Name] AS [AssayName],
	IIF(RR.[MeltDetectorCall] = 'Positive' AND (RR.[Cp] IS NULL OR RR.[Cp] = 0), 30, 
		IIF(RR.[MeltDetectorCall] = 'Positive', RR.[Cp], 40)) AS [Cp], -- if an assay is positive by Tm with no Cp, change to 30... if negative change to 40
	ISNULL(RR.[Tm1], 0) AS [Tm], -- if an assay is negative, then change the Tm to zero
	RR.[MaxFluor] -- every well should have a fluor value
INTO #assayData
FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
	ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
		ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
			ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
				ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
					ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
						ON MA.[experiment_id] = ER.[Id]
WHERE ER.[Id] IN (SELECT [Id] FROM #runs)
 
SELECT 
	R.[Id],
	R.[PouchSerialNumber],
	R.[CustomerSiteId],
	R.[Date],
	R.[LotNo],
	R.[SerialNo],
	1 AS [PositiveAssays],
	0 AS [PositiveGenes],
	T.[TargetName],
	T.[AssayName],
	T.[Cp],
	T.[Tm],
	T.[MaxFluor],
	IIF(T.[AssayName] = 'yeastRNA', 'Control', 'Organism') AS [AssayType]
INTO #master
FROM #runs R INNER JOIN #assayData T
	ON R.[Id] = T.[Id]
WHERE T.[AssayName] LIKE 'HRV%' OR T.[AssayName] LIKE 'Entero%' OR T.[AssayName] = 'yeastRNA'

SELECT
	[RunDataId] + 111999999 AS [RunDataId],
	M.*
FROM #master M INNER JOIN
(
	SELECT ROW_NUMBER() OVER(ORDER BY [PouchSerialNumber]) AS [RunDataId],
		[PouchSerialNumber]
	FROM
	(
		SELECT DISTINCT 
			[PouchSerialNumber]
		FROM #master
	) T
) T
	ON M.[PouchSerialNumber] = T.[PouchSerialNumber]

DROP TABLE #runs, #assayData, #master