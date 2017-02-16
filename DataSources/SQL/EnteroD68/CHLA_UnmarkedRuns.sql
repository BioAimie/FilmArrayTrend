SELECT * /*
	ER.[PouchSerialNumber],
	ER.[PouchLotNumber],
	AA.[Name],
	ISNULL(RR.[Cp], 30) AS [Cp]*/
FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
	ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
		ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
			ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
				ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
					ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
						ON MA.[experiment_id] = ER.[Id]
WHERE (AA.[Name] LIKE 'Entero%' OR AA.[Name] LIKE 'HRV%') AND ER.[PouchSerialNumber] IN
(
	07357723, 
	07357698, 
	07357697, 
	07357685, 
	07357703, 
	07357641, 
	07357674,
	07357720, 
	07357713, 
	07357686, 
	07357700, 
	07357669, 
	07357706, 
	07357689, 
	07357670, 
	07357667, 
	07357705, 
	07357712, 
	07357630, 
	07357650, 
	07357690, 
	07357678, 
	07357715, 
	07357648, 
	07357711, 
	07357662, 
	07357694, 
	07357682, 
	07357692, 
	07357718, 
	07357672, 
	07357696
)	