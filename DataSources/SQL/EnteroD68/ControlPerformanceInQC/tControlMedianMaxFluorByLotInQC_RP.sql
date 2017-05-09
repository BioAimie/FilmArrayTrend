USE [PMS1]
GO
IF OBJECT_ID('dbo.tMedianMaxFluorOfControlsInQC_RP','U') IS NOT NULL
DROP TABLE [dbo].[tMedianMaxFluorOfControlsInQC_RP]

USE [PMS1]
GO

SELECT *
INTO #controlMFs
FROM
(
	SELECT
		ER.[SampleId],
		ER.[PouchSerialNumber],
		ER.[PouchLotNumber],
		AA.[Name],
		RR.[MaxFluor]
	--INTO #controlCps
	FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
		ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
			ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
				ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
					ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
						ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
							ON MA.[experiment_id] = ER.[Id]
	WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[SampleId] LIKE 'QC_RP%' AND [MeltDetectorCall] = 'positive' AND ER.[RunStatus] = 'Completed'
	UNION
	SELECT
		ER.[SampleId],
		ER.[PouchSerialNumber],
		ER.[PouchLotNumber],
		AA.[Name],
		RR.[MaxFluor]
	--INTO #controlCps
	FROM [FILMARRAYDB].[FilmArray1].[FilmArray].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay] AA WITH(NOLOCK) 
		ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay_Reaction] ARX WITH(NOLOCK) 
			ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray1].[FilmArray].[Reaction] RX WITH(NOLOCK) 
				ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ReactionResult] RR WITH(NOLOCK) 
					ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[MetaAnalysis] MA WITH(NOLOCK) 
						ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ExperimentRun] ER WITH(NOLOCK) 
							ON MA.[experiment_id] = ER.[Id]
	WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[SampleId] LIKE 'QC_RP%' AND [MeltDetectorCall] = 'positive' AND ER.[RunStatus] = 'Completed'
) T

SELECT ROW_NUMBER() OVER(PARTITION BY CC.[PouchSerialNumber], CC.[Name] ORDER BY [MaxFluor]) AS [MFOrder],
	C.*,
	CC.[WellPositives]
INTO #ordered
FROM #controlMFs C INNER JOIN
(
	SELECT 
		[PouchSerialNumber],
		[PouchLotNumber],
		[Name],
		COUNT([MaxFluor]) AS [WellPositives]
	FROM #controlMFs
	GROUP BY
		[PouchSerialNumber],
		[PouchLotNumber],
		[Name]
) CC
	ON C.[PouchSerialNumber] = CC.[PouchSerialNumber] AND C.[Name] = CC.[Name]
WHERE [WellPositives] >= 2

SELECT *
INTO #master
FROM
(
	SELECT 
		[PouchSerialNumber],
		[PouchLotNumber],
		[Name],
		AVG([MaxFluor]) AS [MedianMaxFluor]
	FROM #ordered
	WHERE [WellPositives] = 2
	GROUP BY 
		[PouchSerialNumber],
		[PouchLotNumber],
		[Name]

	UNION ALL

	SELECT
		[PouchSerialNumber],
		[PouchLotNumber],
		[Name],
		[MaxFluor]
	FROM #ordered 
	WHERE [WellPositives] = 3 AND [MFOrder] = 2
) T

SELECT *
INTO #lotOrdered
FROM 
(
	SELECT ROW_NUMBER() OVER(PARTITION BY [PouchLotNumber], [Name] ORDER BY [MedianMaxFluor]) AS [MFOrder],
		*
	FROM #master
) T

SELECT 
	L.*,
	[Runs]
INTO #lotMedian
FROM #lotOrdered L INNER JOIN 
(
	SELECT 
		[PouchLotNumber],
		[Name],
		COUNT([PouchSerialNumber]) AS [Runs]
	FROM #lotOrdered
	GROUP BY [PouchLotNumber], [Name]
) LL
	ON L.[PouchLotNumber] = LL.[PouchLotNumber] AND L.[Name] = LL.[Name]

SELECT 
	[PouchLotNumber],
	[PouchSerialNumber],
	[Name],
	[MedianMaxFluor],
	[MFOrder],
	[Runs],
	IIF([Runs] % 2 = 0, CAST([Runs] AS FLOAT)/2, ROUND(CAST([Runs] AS FLOAT)/2,0)) AS [LotMedianIndex],
	IIF([Runs] % 2 = 0, CAST([Runs] AS FLOAT)/2+1, ROUND(CAST([Runs] AS FLOAT)/2,0)) AS [HighMedianIndex]
INTO #temp
FROM #lotMedian

CREATE TABLE [PMS1].[dbo].[tMedianMaxFluorOfControlsInQC_RP] (
	[PouchLotNumber] VARCHAR(200),
	[Name] VARCHAR(20),
	[MedianMaxFluor] FLOAT
)
INSERT INTO [PMS1].[dbo].[tMedianMaxFluorOfControlsInQC_RP]
SELECT 
	[PouchLotNumber],
	[Name],
	AVG([MedianMaxFluor]) AS [MedianMaxFluor]
FROM
(
	SELECT *,
		IIF([Runs] % 2 = 0 AND ([MFOrder] BETWEEN [LotMedianIndex] AND [HighMedianIndex]), 1, 
			IIF([Runs] % 2 <> 0 AND [MFOrder] = [LotMedianIndex], 1, 0)) AS [x]
	FROM #temp
) T
WHERE [x] = 1
GROUP BY
	[PouchLotNumber],
	[Name]

DROP TABLE #controlMFs, #ordered, #master, #lotOrdered, #lotMedian, #temp