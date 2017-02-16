USE [PMS1]
GO
IF  OBJECT_ID(N'dbo.pFluorQC_1_5', N'P') IS NOT NULL
DROP PROCEDURE [dbo].[pAddControlMedianCpsInQC_RP];
IF OBJECT_ID('dbo.TempLots','U') IS NOT NULL
DROP TABLE [dbo].[TempLots]
IF OBJECT_ID('dbo.LotsToAdd','U') IS NOT NULL
DROP TABLE [dbo].[LotsToAdd]
IF OBJECT_ID('dbo.MedianCpByRun','U') IS NOT NULL
DROP TABLE [dbo].[MedianCpByRun]
IF OBJECT_ID('dbo.MedianCpByLot','U') IS NOT NULL
DROP TABLE [dbo].[MedianCpByLot] 

USE [PMS1]
GO
CREATE PROCEDURE [dbo].[pAddControlMedianCpsInQC_RP]
AS

BEGIN
	SET NOCOUNT ON;
	
	CREATE TABLE [PMS1].[dbo].[TempLots]
	(
		[PouchLotNumber] VARCHAR(200)
	)

	INSERT INTO [PMS1].[dbo].[TempLots]
	SELECT *
	FROM
	(
	SELECT DISTINCT
		ER.[PouchLotNumber]
	FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
		ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
			ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
				ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
					ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
						ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
							ON MA.[experiment_id] = ER.[Id]
	WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[SampleId] LIKE 'QC_RP%' AND [MeltDetectorCall] = 'positive' AND ER.[RunStatus] = 'Completed' AND ER.[StartTime] >= GETDATE()-3
	UNION
	SELECT DISTINCT 
		ER.[PouchLotNumber]
	FROM [FILMARRAYDB].[FilmArray1].[FilmArray].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay] AA WITH(NOLOCK) 
		ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay_Reaction] ARX WITH(NOLOCK) 
			ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray1].[FilmArray].[Reaction] RX WITH(NOLOCK) 
				ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ReactionResult] RR WITH(NOLOCK) 
					ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[MetaAnalysis] MA WITH(NOLOCK) 
						ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ExperimentRun] ER WITH(NOLOCK) 
							ON MA.[experiment_id] = ER.[Id]
	WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[SampleId] LIKE 'QC_RP%' AND [MeltDetectorCall] = 'positive' AND ER.[RunStatus] = 'Completed' AND ER.[StartTime] >= GETDATE()-3
	) T

	CREATE TABLE [PMS1].[dbo].[LotsToAdd] 
	(
		[PouchSerialNumber] VARCHAR(200),
		[PouchLotNumber] VARCHAR(200),
		[Name] VARCHAR(20),
		[Cp] FLOAT
	)

	INSERT INTO [PMS1].[dbo].[LotsToAdd] 
	SELECT *
	FROM
	(
		SELECT
			ER.[PouchSerialNumber],
			ER.[PouchLotNumber],
			AA.[Name],
			ISNULL(RR.[Cp], 30) AS [Cp]
		FROM [FILMARRAYDB].[FilmArray2].[dbo].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay] AA WITH(NOLOCK) 
			ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[Assay_Reaction] ARX WITH(NOLOCK) 
				ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray2].[dbo].[Reaction] RX WITH(NOLOCK) 
					ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ReactionResult] RR WITH(NOLOCK) 
						ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[MetaAnalysis] MA WITH(NOLOCK) 
							ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray2].[dbo].[ExperimentRun] ER WITH(NOLOCK) 
								ON MA.[experiment_id] = ER.[Id]
		WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[SampleId] LIKE 'QC_RP%' AND [MeltDetectorCall] = 'positive' AND ER.[RunStatus] = 'Completed' AND ER.[PouchLotNumber] IN (SELECT [PouchLotNumber] FROM [PMS1].[dbo].[TempLots])
		UNION
		SELECT
			ER.[PouchSerialNumber],
			ER.[PouchLotNumber],
			AA.[Name],
			ISNULL(RR.[Cp], 30) AS [Cp]
		FROM [FILMARRAYDB].[FilmArray1].[FilmArray].[AssayResult] AR WITH(NOLOCK) INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay] AA WITH(NOLOCK) 
			ON AR.[assay_id] = AA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[Assay_Reaction] ARX WITH(NOLOCK) 
				ON AA.[Id] = ARX.[assay_id] INNER JOIN  [FILMARRAYDB].[FilmArray1].[FilmArray].[Reaction] RX WITH(NOLOCK) 
					ON ARX.[reaction_id] = RX.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ReactionResult] RR WITH(NOLOCK) 
						ON RX.[Id] = RR.[reaction_id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[MetaAnalysis] MA WITH(NOLOCK) 
							ON AR.[analysis_id] = MA.[Id] INNER JOIN [FILMARRAYDB].[FilmArray1].[FilmArray].[ExperimentRun] ER WITH(NOLOCK) 
								ON MA.[experiment_id] = ER.[Id]
		WHERE AA.[Name] IN ('PCR1','PCR2','yeastRNA') AND ER.[SampleId] LIKE 'QC_RP%' AND [MeltDetectorCall] = 'positive' AND ER.[RunStatus] = 'Completed' AND ER.[PouchLotNumber] IN (SELECT [PouchLotNumber] FROM [PMS1].[dbo].[TempLots])
	) T

	CREATE TABLE [PMS1].[dbo].[MedianCpByRun] 
	(
		[PouchSerialNumber] VARCHAR(200),
		[PouchLotNumber] VARCHAR(200),
		[Name] VARCHAR(20),
		[MedianCp] FLOAT
	)

	INSERT INTO [PMS1].[dbo].[MedianCpByRun] 
	SELECT *
	FROM
	(
		SELECT 
			[PouchSerialNumber],
			[PouchLotNumber],
			[Name],
			AVG([Cp]) AS [MedianCp]
		FROM
		(
			SELECT ROW_NUMBER() OVER(PARTITION BY CC.[PouchSerialNumber], CC.[Name] ORDER BY [Cp]) AS [CpOrder],
				C.*,
				CC.[WellPositives]
			FROM [PMS1].[dbo].[LotsToAdd] C INNER JOIN
			(
				SELECT 
					[PouchSerialNumber],
					[PouchLotNumber],
					[Name],
					COUNT([Cp]) AS [WellPositives]
				FROM [PMS1].[dbo].[LotsToAdd]
				GROUP BY
					[PouchSerialNumber],
					[PouchLotNumber],
					[Name]
			) CC
				ON C.[PouchSerialNumber] = CC.[PouchSerialNumber] AND C.[Name] = CC.[Name]
			WHERE [WellPositives] >= 2
		) T
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
			[Cp]
		FROM 
		(
			SELECT ROW_NUMBER() OVER(PARTITION BY CC.[PouchSerialNumber], CC.[Name] ORDER BY [Cp]) AS [CpOrder],
				C.*,
				CC.[WellPositives]
			FROM [PMS1].[dbo].[LotsToAdd] C INNER JOIN
			(
				SELECT 
					[PouchSerialNumber],
					[PouchLotNumber],
					[Name],
					COUNT([Cp]) AS [WellPositives]
				FROM [PMS1].[dbo].[LotsToAdd]
				GROUP BY
					[PouchSerialNumber],
					[PouchLotNumber],
					[Name]
			) CC
				ON C.[PouchSerialNumber] = CC.[PouchSerialNumber] AND C.[Name] = CC.[Name]
			WHERE [WellPositives] >= 2
		) T
		WHERE [WellPositives] = 3 AND [CpOrder] = 2
	) T

	CREATE TABLE [PMS1].[dbo].[MedianCpByLot] 
	(
		[PouchLotNumber] VARCHAR(200),
		[PouchSerialNumber] VARCHAR(200),
		[Name] VARCHAR(20),
		[MedianCp] FLOAT,
		[CpOrder] INT,
		[Runs] INT,
		[LowMedianIndex] INT,
		[HighMedianIndex] INT
	)

	INSERT INTO [PMS1].[dbo].[MedianCpByLot] 
	SELECT 
		[PouchLotNumber],
		[PouchSerialNumber],
		[Name],
		[MedianCp],
		[CpOrder],
		[Runs],
		IIF([Runs] % 2 = 0, CAST([Runs] AS FLOAT)/2, ROUND(CAST([Runs] AS FLOAT)/2,0)) AS [LowMedianIndex],
		IIF([Runs] % 2 = 0, CAST([Runs] AS FLOAT)/2+1, ROUND(CAST([Runs] AS FLOAT)/2,0)) AS [HighMedianIndex]
	FROM
	(
		SELECT 
			L.*,
			[Runs]
		FROM 
		(
			SELECT ROW_NUMBER() OVER(PARTITION BY [PouchLotNumber], [Name] ORDER BY [MedianCp]) AS [CpOrder],
				*
			FROM [PMS1].[dbo].[MedianCpByRun]
		) L INNER JOIN
		(
			SELECT 
				[PouchLotNumber],
				[Name],
				COUNT([PouchSerialNumber]) AS [Runs]
			FROM [PMS1].[dbo].[MedianCpByRun]
			GROUP BY [PouchLotNumber], [Name]
		) LL
			ON L.[PouchLotNumber] = LL.[PouchLotNumber] AND L.[Name] = LL.[Name]
	) T

	DELETE FROM [PMS1].[dbo].[tMedianCpsOfControlsInQC_RP]
	WHERE [PouchLotNumber] IN (SELECT [PouchLotNumber] FROM [PMS1].[dbo].[TempLots])
	
	INSERT INTO [PMS1].[dbo].[tMedianCpsOfControlsInQC_RP]
	SELECT 
		[PouchLotNumber],
		[Name],
		AVG([MedianCp]) AS [MedianCp]
	FROM
	(
		SELECT *,
			IIF([Runs] % 2 = 0 AND ([CpOrder] BETWEEN [LowMedianIndex] AND [HighMedianIndex]), 1, 
				IIF([Runs] % 2 <> 0 AND [CpOrder] = [LowMedianIndex], 1, 0)) AS [x]
		FROM [PMS1].[dbo].[MedianCpByLot]
	) T
	WHERE [x] = 1
	GROUP BY
		[PouchLotNumber],
		[Name]

	DROP TABLE [PMS1].[dbo].[TempLots]
	DROP TABLE [PMS1].[dbo].[LotsToAdd]
	DROP TABLE [PMS1].[dbo].[MedianCpByRun]
	DROP TABLE [PMS1].[dbo].[MedianCpByLot] 

END;