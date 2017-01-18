SET NOCOUNT ON

SELECT
	ER.[StartTime],
	ER.[PouchSerialNumber],
	ER.[SampleType],
	ER.[PouchLotNumber],
	AA.[Name],
	RR.[Cp],
	AR.[Result],
	IIF(AR.[Result] = 'Positive', 1, 0) AS [FlagPositives]
INTO #allFilmArray1ControlCp
FROM [FILMARRAYDB].[FilmArray1].[FilmArray].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay] AA WITH(NOLOCK) 
	ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay_Reaction] ARX WITH(NOLOCK) 
		ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray1].[FilmArray].[Reaction] RX WITH(NOLOCK) 
			ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ReactionResult] RR WITH(NOLOCK) 
				ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[MetaAnalysis] MA WITH(NOLOCK) 
					ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ExperimentRun] ER WITH(NOLOCK) 
						ON MA.[experiment_id] = ER.[Id]
WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[PouchLotNumber] LIKE '%227415%'  AND ER.[SampleId] LIKE '%QC%' AND [SampleType] LIKE 'NPS v%'
ORDER BY ER.[PouchLotNumber], ER.[PouchSerialNumber], AA.[Name]


SELECT *
INTO #positiveAssays
FROM
(
	SELECT 
		T.[PouchSerialNumber],
		T.[Name],
		COUNT(T.[FlagPositives]) AS [CountPositives]
	FROM #allFilmArray1ControlCp T
	GROUP BY T.[PouchSerialNumber], T.[Name]
	
) T1 
WHERE [CountPositives] >= 2


SELECT 
	CAST(A.[StartTime] AS DATE) AS [RunDate],
	A.[PouchSerialNumber],
	A.[SampleType],
	A.[PouchLotNumber],
	A.[Name],
	A.[Cp],
	A.[Result]
FROM #allFilmArray1ControlCp A
WHERE A.[PouchSerialNumber] IN (SELECT [PouchSerialNumber] FROM #positiveAssays)

DROP TABLE #allFilmArray1ControlCp, #positiveAssays