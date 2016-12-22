SET NOCOUNT ON

SELECT 
	ER.[PouchSerialNumber],
	ER.[SampleType],
	ER.[PouchLotNumber],
	AA.[Name],
	RR.[Cp],
	AR.[Result],
	IIF(AR.[Result] = 'Positive', 1, 0) AS [FlagPositives]
INTO #allFilmArray2ControlCp
FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
	ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
		ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
			ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
				ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
					ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
						ON MA.[experiment_id] = ER.[Id]
WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[PouchLotNumber] LIKE '%227415%'  AND ER.[SampleId] LIKE '%QC%'
ORDER BY ER.[PouchLotNumber], ER.[PouchSerialNumber], AA.[Name]

-- Keep only [PouchSerialNumbers] where Assay wells with >= 2 positive results
SELECT *
INTO #positiveAssays
FROM
(
	SELECT 
		T.[PouchSerialNumber],
		T.[Name],
		COUNT(T.[FlagPositives]) AS [CountPositives]
	FROM #allFilmArray2ControlCp T
	GROUP BY T.[PouchSerialNumber], T.[Name]
	
) T1 
WHERE [CountPositives] >= 2

-- Filtered Cp for positive assays
SELECT 
	A.[PouchSerialNumber],
	A.[SampleType],
	A.[PouchLotNumber],
	A.[Name],
	A.[Cp],
	A.[Result]
FROM #allFilmArray2ControlCp A
WHERE A.[PouchSerialNumber] IN (SELECT [PouchSerialNumber] FROM #positiveAssays)


DROP TABLE #allFilmArray2ControlCp, #positiveAssays