USE [FADataWarehouse]
GO

IF OBJECT_ID('dbo.EpiWeeks','U') IS NOT NULL
DROP TABLE [dbo].[EpiWeeks]
IF  OBJECT_ID(N'dbo.spEpiWeeks', N'P') IS NOT NULL
DROP PROCEDURE [dbo].[spEpiWeeks];

USE [FADataWarehouse]
GO

CREATE PROCEDURE [dbo].[spEpiWeeks]
AS
BEGIN
-- 
-- OBJECTIVE: This code finds and 'EPI Week' for a given date
--
--'What is EPI Week?"
-- Definition-1: "The first epi week of the year ends, by definition, on the first Saturday of January, as long as it falls at least four days into the month. Each epi week begins on a Sunday and ends on a Saturday."
-- Source-1: 'Central Massachusetts Mosquito Control Project'; www.cmmcp.org/epiweek.html; Downloaded in September 2016
-- Definition-2: "The epidemiological calendar is used on a daily basis in disease surveillance and divides the 365 days of the year in weeks which usually begin on Sunday and ends on Saturday." ... "Sunday being the staring day of the week"
-- Source-2: 'Centers for Disease Control and Prevention'; wwwn.cdc.gov/epiinfo/user-guide/; Downloaded in September 2016
--
	SET NOCOUNT ON;

	-- Create a table that has every date from Jan 1, 2013 through today
	DECLARE @DateFrom DATE, @DateTo DATE;
	SET @DateFrom = CONVERT(DATE, '2000-01-01');
	SET @DateTo = CAST(GETDATE() AS DATE);
	WITH T(date)
	AS
	(
		SELECT @DateFrom
		UNION ALL
		SELECT DATEADD(day, 1, T.date) FROM T WHERE T.date < @DateTo
	)
	SELECT 
		date AS [Date],
		YEAR(date) AS [Year],
		DATEPART(dy, date) AS [DayOfYear],
		DATEPART(dw, date) AS [DayOfWeek],
		DATEPART(d, date) AS [DayofMonth],
		1 AS [Day]
	INTO #calendar
	FROM T
	GROUP BY date, YEAR(date), DATEPART(dy,date)
	OPTION(MAXRECURSION 32767);

	-- Create temporary columns:
	-- [DayOfYear] is the day in a year from 1 to 365 or 366
	-- [DayOfWeek] is the day in a week such that 'Sunday = 1', 'Monday = 2', 'Tuesday = 3', 'Wednesday = 4', 'Thursday = 5', 'Saturday = 6'
	-- [FirstDayOfYear] is the weekday for 1st of January
	-- [LastDayOfYear] is the weekday for 31st of December
	--
	SELECT *,
	(
		SELECT 
			MAX([DayOfYear]) 
		FROM #calendar C2
		WHERE C2.[Year] = C1.[Year]
	) AS [MaxDay],
	(
		SELECT
			[DayOfWeek]
		FROM #calendar C2
		WHERE [DayOfYear] = 1 AND C2.[Year] = C1.[Year]
	) AS [FirstDayOfYear],
	(
		SELECT
			[DayOfWeek]
		FROM #calendar C2
		WHERE [DayOfYear] = (SELECT MAX([DayOfYear]) FROM #calendar C3 WHERE C3.[Year] = C2.[Year]) AND C2.[Year] = C1.[Year] 
	) AS [LastDayOfYear]
	INTO #augCal
	FROM #calendar C1                         


	-- Write conditions to identify a date in EPI Week (Epidemiological Week) or CDC week
	--
	-- The 'IIF' statements conditions are:
	-- s1: checks if the day falls on 'Sunday', 'Monday', or 'Tuesday' of last week of a year in December where 'December 31st' is either on 'Sunday', 'Monday' or 'Tuesday'
	--		If TRUE, then the date is assigned to week '1', if FALSE, then next IIF
	-- s2: checks if the day is '<= 4th' day of a year and if 'January 1st' of that year falls on 'Sunday', 'Monday', 'Tuesday' or 'Wednesday'
	--		If TRUE, then date is assigned to week '1', if FALSE then next IIF
	-- s3: checks if the day is '>= 4th' day of a year and if 'January 1st' falls on 'Sunday', 'Monday' or 'Tuesday'
	--		If TRUE, then the date is assigned to a week calculated by the function 'DATEPART(ww, [Date])', if FALSE the next IIF
	-- s4: checks if the day is '<= 3rd' day of a year and if the weekday is 'Thursday', 'Friday' or 'Saturday' and if 'January 1st' falls on 'Sunday', 'Monday', 'Tuesday' or 'Wednesday' and if 'December 31st' falls on 'Wednesday', 'Thursday', 'Friday', 'Saturday'
	--		If TRUE, then the date is assigned to week '53', if FALSE, then next IIF
	-- s5: checks if the day is '<= 3rd' day of a year and if the weekday is 'Thursday','Friday', 'Saturday' and if 'January 1st' falls on a 'Thursday'
	--		If TRUE, then the date is assigned to week '53', if FALSE, the next IIF
	-- s6: checks if the day is '<= 3rd' day of a year and if the weekday is 'Friday' or 'Saturday' and if 'January 1st' falls on 'Friday' and if 'December 31st' falls on 'Friday'
	--		If TRUE, then the date is assigned to week '52', if FALSE, then next IIF
	-- s7: checks if the day is '<= 3rd' day of a year and if the weekday is 'Thursday', 'Friday' or 'Saturday' and if 'January 1st' falls on 'Friday' and if 'December 31st' falls on 'Friday'
	--		If TRUE, then the date is assigned to week '53', if FALSE, then next IIF
	-- s8: checks if the day is '<= 3rd' day of a year and if the weekday is 'Thursday', 'Friday' or 'Saturday'
	--		If TRUE, then the date is assigned to week '52', if FALSE, then next IIF
	-- s9: checks if 'January 1st' falls on 'Thursday', 'Friday' or 'Saturday'
	--		If TRUE, then the date is assigned to week calculated as 'DATEPART(ww, [Date]) - 1', if FALSE, then 'DATEPART(ww, [Date])'
	--	This creates the [TempWeek] column
	-- s10: checks if a 'week number' in a row of [TempWeek] is '= 52' and if 'week number' in two rows preceding that row is '= 53'
	--		If TRUE, then the 'week number' is assigned to '53', if FALSE, the same 'week number' from [TempWeek] is retained
	-- [EpiWeek]: lists 'EPI Weeks' for each date in the [Date]
	--
	-- s11: calculates the [EpiYear] which can be different in 1st and last week of a year from the regular year
	CREATE TABLE [dbo].[EpiWeeks]
	(
		[Date] DATE,
		[EpiYear] INT,
		[EpiWeek] INT
	)

	INSERT INTO [dbo].[EpiWeeks]
	SELECT 
		[Date],
		IIF((DATEPART(m, [Date]) = 12 AND [DayofMonth] = 29 AND [DayOfWeek] = 1)										-- s11
				OR (DATEPART(m, [Date]) = 12 AND [DayofMonth] = 30 AND ([DayOfWeek] <= 2))
				OR (DATEPART(m, [Date]) = 12 AND [DayofMonth] = 31 AND ([DayOfWeek] >= 1 AND [DayOfWeek] <= 3)), [Year] + 1, 
			IIF(([DayOfYear] = 1 AND ([DayOfWeek] >= 5))
				OR ([DayOfYear] = 2 AND ([DayOfWeek] >= 6))
				OR ([DayOfYear] = 3 AND ([DayOfWeek] = 7)), [Year] - 1, [Year]) ) AS [EpiYear],
		IIF(([TempWeek] = 52 AND (LAG([TempWeek],2) OVER(ORDER BY [Date]) = 53)), 53, [TempWeek]) AS [EpiWeek]			-- s10
	FROM
	(
		SELECT *,
			IIF([DayOfYear] > 357 AND [DayofMonth] >= 29 AND [DayOfWeek] <= 3 AND [LastDayOfYear] <= 3, 1,				-- s1
				IIF([DayOfYear] <= 4 AND [FirstDayOfYear] <= 4, 1,														-- s2
				IIF([DayOfYear] >= 4 AND [FirstDayOfYear] <= 3, DATEPART(ww, [Date]),									-- s3
				IIF([DayOfYear] <= 3 AND [DayOfWeek] > 4 AND [FirstDayOfYear] <= 4 AND [LastDayOfYear] >= 4, 53,		-- s4
				IIF([DayOfYear] <= 3 AND [DayOfWeek] > 4 AND [FirstDayOfYear] = 5, 53,									-- s5
				IIF([DayOfYear] <= 3 AND [DayOfWeek] >= 6 AND [FirstDayOfYear] = 6 AND [LastDayOfYear] = 6 , 52,		-- s6
				IIF([DayOfYear] <= 3 AND [DayOfWeek] > 4 AND [FirstDayOfYear] = 6 AND [LastDayOfYear] = 6, 53,			-- s7
				IIF([DayOfYear] <= 3 AND [DayOfWeek] > 4, 52,															-- s8
				IIF([FirstDayOfYear] >= 5, DATEPART(ww, [Date])-1, DATEPART(ww, [Date]))))))))))  AS [TempWeek]			-- s9
		FROM #augCal
	) T
	WHERE [Date] > = '2012-01-01' AND [Date] <= GETDATE()
	ORDER BY [Date]

	DROP TABLE #calendar, #augCal
END

