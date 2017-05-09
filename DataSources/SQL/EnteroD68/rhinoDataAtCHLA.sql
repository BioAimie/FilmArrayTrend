SET NOCOUNT ON

SELECT 
	ER.[Id],
	ER.[PouchSerialNumber],
	25 AS [CustomerSiteId],
	CAST(ER.[StartTime] AS DATE) AS [Date],
	ER.[PouchLotNumber] AS [LotNo],
	ER.[InstrumentSerialNumber] AS [SerialNo],
	1 AS [PositiveAssays],
	0 AS [PositiveGenes],
	'Rhinovirus/Enterovirus' AS [TargetName],
	[Name] AS [AssayName],
	IIF([Cp] IS NULL OR [Cp] = 0, 30, [Cp]) AS [Cp],
	[Tm1] AS [Tm],
	[MaxFluor],
	IIF([Name] IN ('yeastRNA','PCR2'), 'Control', 'Organism') AS [AssayType]
INTO #master
FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
		ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
			ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
				ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
					ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
						ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
							ON MA.[experiment_id] = ER.[Id]
WHERE ER.[PouchSerialNumber] IN
(
'07357638', '07357723', '07357726', '07357698', '07357716', '07357697', '07357644', '07357685', '07357703', '07357641', '07357674', '07357720', '07357713', '07357686', '07357700', '07357669', '07357706', 
'07357689', '07357725', '07357670', '07357667', '07357647', '07357705', '07357704', '07357712', '07357630', '07357650', '07357691', '07357661', '07357690', '07357678', '07357715', '07357640', '07357648', 
'07357711', '07357662', '07357694', '07357682','07357692', '07357718', '07357672', '07357696'
) AND [MeltDetectorCall] = 'Positive' AND [IsHidden] = 0 AND ([Name] = 'yeastRNA' OR [Name] LIKE 'HRV%' OR [Name] LIKE 'Entero%')

SELECT
	[Id],
	[NewRunDataId] + 111999999 AS [RunDataId],
	[PouchSerialNumber],
	[CustomerSiteId],
	[Date],
	[LotNo],
	[SerialNo],
	[PositiveAssays],
	[PositiveGenes],
	[TargetName],
	[AssayName],
	[Cp],
	[Tm],
	[MaxFluor],
	[AssayType]
FROM
(
	SELECT 
		M.*
	FROM #master M INNER JOIN 
	(
		SELECT 
			[RunDataId],
			[AssayName],
			COUNT([Cp]) AS [PositiveWells]
		FROM #master
		GROUP BY [RunDataId], [AssayName]
	) C
		ON M.[RunDataId] = C.[RunDataId] AND M.[AssayName] = C.[AssayName]
	WHERE [PositiveWells] > 1
) P INNER JOIN 
(
	SELECT ROW_NUMBER() OVER(ORDER BY [RunDataId]) AS [NewRunDataId], [RunDataId]
	FROM (SELECT DISTINCT [RunDataId] FROM #master) T
) I
	ON P.[RunDataId] = I.[RunDataId]

DROP TABLE #master