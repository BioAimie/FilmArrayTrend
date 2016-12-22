USE [FADataWarehouse]
GO

IF OBJECT_ID('dbo.SitePercentDetectionRP','U') IS NOT NULL
DROP TABLE [dbo].[SitePercentDetectionRP]
IF  OBJECT_ID(N'dbo.spSitePercentDetectionRP', N'P') IS NOT NULL
DROP PROCEDURE [dbo].[spSitePercentDetectionRP];

USE [FADataWarehouse]
GO

CREATE PROCEDURE [dbo].[spSitePercentDetectionRP]
AS
BEGIN

	-- Get all the site ids that are running respiratory panels that are trendable....
	DECLARE @customerSiteId TABLE
	(
		[customerSiteIndex] INT,
		[CustomerSiteId] INT
	) 
	INSERT INTO @customerSiteId
	SELECT 
		ROW_NUMBER() OVER(ORDER BY [CustomerSiteId]) AS [customerSiteIndex],
		[CustomerSiteId]
	FROM
	(
		SELECT DISTINCT
			L.[CustomerSiteId]
		FROM [dbo].[RunData] R WITH(NOLOCK) INNER JOIN [dbo].[ConnectorLaptops] L WITH(NOLOCK)
				ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
		WHERE R.[PouchTitle] LIKE 'Resp%' AND R.[SuppressState] LIKE 'Trendable'
	) T

	-- Create a table that filters the RunData table by siteId and panel, and only includes trendable runs
	DECLARE @runDataRaw TABLE
	(
		[RunDataId] INT,
		[CustomerSiteId] INT, 
		[StartTime] DATETIME,
		[Date] DATE,
		[Record] INT
	)
	INSERT INTO @runDataRaw
	SELECT 
		R.[RunDataId],
		L.[CustomerSiteId],
		R.[StartTime],
		CAST([StartTime] AS DATE) AS [Date],
		1 AS [Record]
	FROM [dbo].[RunData] R WITH(NOLOCK) INNER JOIN [dbo].[ConnectorLaptops] L WITH(NOLOCK)
		   ON R.[ConnectorLaptopId] = L.[ConnectorLaptopId]
	WHERE R.[PouchTitle] LIKE 'Resp%'
		   AND R.[SuppressState] LIKE 'Trendable' AND R.[RunStatus] LIKE 'Completed' 

	-- Using the @runData table, create a calendar of all year and weeks as a placeholder
	DECLARE @calendar TABLE 
	(
		[Date] DATE,
		[Year] INT,
		[Week] INT   
	)
	DECLARE @DateFrom DATE, @DateTo DATE;
	SELECT @DateFrom = MIN(CAST([StartTime] AS DATE)) FROM @runDataRaw WHERE YEAR([StartTime]) >= 2011;
	SET @DateTo = CAST(GETDATE() AS DATE);

	INSERT INTO @calendar
	SELECT DISTINCT
		[Date],
		[EpiYear],
		[EpiWeek]
	FROM [dbo].[EpiWeeks] WITH(NOLOCK)
	WHERE [Date] >= @DateFrom AND [Date] <= @DateTo

	DECLARE @runData TABLE
	(
		[RunDataId] INT,
		[CustomerSiteId] INT, 
		[StartTime] DATETIME,
		[Year] INT,
		[Week] INT,
		[Record] INT
	)
	INSERT INTO @runData
	SELECT 
		R.[RunDataId],
		R.[CustomerSiteId],
		R.[StartTime],
		C.[Year],
		C.[Week],
		R.[Record]
	FROM @runDataRaw R LEFT JOIN @calendar C
		ON R.[Date] = C.[Date]

	-- Identify the assays that are included in the panel
	DECLARE @assays TABLE
	(
		[AssayNo] INT,
		[AssayName] VARCHAR(200)
	) 
	INSERT INTO @assays
	SELECT 
		ROW_NUMBER() OVER(ORDER BY [Interpretation]) AS [AssayNo],
		[Interpretation]
	FROM
	(
		SELECT DISTINCT 
			[Interpretation] 
		FROM [dbo].[SummarizedPositiveAssayResults] WITH(NOLOCK) 
		WHERE [RunDataId] IN (SELECT [RunDataId] FROM @runData) AND [ResultType] NOT LIKE 'Control'
	) D

	-- Read in positive assays related to each run id
	DECLARE @assayPositives TABLE
	(
		[CustomerSiteId] INT,
		[Year] INT,
		[Week] INT,
		[AssayName] VARCHAR(200),
		[Record] INT
	)
	INSERT INTO @assayPositives
	SELECT 
		R.[CustomerSiteId],
		R.[Year],
		R.[Week],
		A.[Interpretation],
		1 AS [Record]
	FROM @runData R INNER JOIN [dbo].[SummarizedPositiveAssayResults] A WITH(NOLOCK) 
			ON R.[RunDataId] = A.[RunDataId] 
	WHERE A.[ResultType] NOT LIKE 'Control'

	-- Loop through each assay and join the positive count (3-week sum centered about the week) to @rolledRuns so prevalence can be determined
	DECLARE @count_customerid INT; SET @count_customerid = 1;
	DECLARE @stopcount_customerid INT; SELECT @stopcount_customerid = MAX([customerSiteIndex]) FROM @customerSiteId
	DECLARE @centered TABLE
	(
		[CustomerSiteId] INT,
		[Year] INT,
		[Week] INT,
		[Interpretation] VARCHAR(200),
		[CenteredRuns] INT,
		[CenteredPositives] INT
	)

	-- Loop '@centered' over [CustomerSiteId]
	WHILE @count_customerid <= @stopcount_customerid
	BEGIN
   
		DECLARE @count_assay INT; SET @count_assay = 1;
		DECLARE @stopcount_assay INT; SELECT @stopcount_assay = MAX([AssayNo]) FROM @assays
		DECLARE @custSiteId INT; SELECT @custSiteId = [CustomerSiteId] FROM @customerSiteId WHERE [customerSiteIndex] = @count_customerid;

			-- Loop over [AssayNames]
			WHILE @count_assay <= @stopcount_assay
			BEGIN
				
				DECLARE @assayName VARCHAR(200); SELECT @assayName = [AssayName] FROM @assays WHERE [AssayNo] = @count_assay;

				INSERT INTO @centered
				SELECT
					@custSiteId AS [CustomerSiteId],
					R.[Year],
					R.[Week],
					@assayName AS [AssayName],
					R.[CenteredRuns],
					(ISNULL(LAG(A.[Positives]) OVER(ORDER BY R.[Year], R.[Week]), 0)			
						+ ISNULL(A.[Positives],0) 
						+ ISNULL(LEAD(A.[Positives]) OVER(ORDER BY R.[Year], R.[Week]), 0))	 AS [CenteredPositives]	
				FROM
				(
					SELECT 
						[Year],
						[Week],
						[CenteredRuns] 
					-- ........ '@rolledRuns' code block BEGIN .....
					FROM 
					(
						SELECT
							@custSiteId AS [CustomerSiteId],
							[Year],
							[Week],
							(ISNULL(LAG([Runs]) OVER(ORDER BY [Year], [Week]),0) + [Runs] + ISNULL(LEAD([Runs]) OVER(ORDER BY [Year], [Week]),0)) AS [CenteredRuns]
						FROM
						(		
							SELECT DISTINCT 
								@custSiteId AS [CustomerSiteId],
								C.[Year],
								C.[Week],
								ISNULL(R1.[Runs], 0) AS [Runs]
							 FROM @calendar C LEFT JOIN 
							(
								SELECT
									[Year],
									[Week],
									SUM([Record]) AS [Runs]
								FROM @runData 
								WHERE [CustomerSiteId] = @custSiteId                          
								GROUP BY [Year], [Week]
							) R1
							ON C.[Year] = R1.[Year] AND C.[Week] = R1.[Week]
						) T
					) a
					-- ......... '@rolledRuns' code block END .....
				) R LEFT JOIN
				(
					SELECT
						[Year],
						[Week],
						[AssayName],
						SUM([Record]) AS [Positives]
					FROM @assayPositives
					WHERE [AssayName] LIKE (SELECT [AssayName] FROM @assays WHERE [AssayNo] = @count_assay)
							AND [CustomerSiteId] = @custSiteId		
					GROUP BY [Year], [Week], [AssayName]							
				) A
				ON R.[Year] = A.[Year] AND R.[Week] = A.[Week]					
				SET @count_assay = @count_assay + 1

			END 
		SET @count_customerid = @count_customerid + 1
	END

	CREATE TABLE [dbo].[SitePercentDetectionRP]
	(
		[CustomerSiteId] INT,
		[Year] INT,
		[Week] INT,
		[Interpretation] VARCHAR(200),
		[CenteredRuns] INT,
		[CenteredPositives] INT,
		[CenteredRate] FLOAT
	)

	INSERT INTO [dbo].[SitePercentDetectionRP]
	SELECT
		[CustomerSiteId],
		[Year],
		[Week],
		[Interpretation],
		[CenteredRuns],
		[CenteredPositives],
		IIF([CenteredRuns] < 1, 0, CAST([CenteredPositives] AS FLOAT)/CAST([CenteredRuns] AS FLOAT)) AS [CenteredRate]
	FROM @centered

END