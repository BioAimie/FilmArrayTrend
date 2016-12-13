SET NOCOUNT ON

-- find Cp for controls
SELECT
	ER.[PouchSerialNumber],
	ER.[SampleType],
	ER.[PouchLotNumber],
	AA.[Name],
	RR.[Cp]
FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
	ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
		ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
			ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
				ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
					ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
						ON MA.[experiment_id] = ER.[Id]
WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[PouchLotNumber] LIKE '%227415%' 
ORDER BY ER.[PouchLotNumber], ER.[PouchSerialNumber], AA.[Name]




