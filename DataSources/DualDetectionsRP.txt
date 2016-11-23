SET NOCOUNT ON

SELECT 
	r.RunDataId,  
	pa.Interpretation, 
	r.Starttime, 
	CAST(r.PositiveAssays AS DECIMAL(19,5)) as PositiveAssays, 
	year(r.Starttime) as Year, 
	DATEPART(WEEK, r.Starttime) as Interval, 
	Cast(concat(year(starttime), datepart(ww,starttime)) as Int) as YearWeek
Into #Runs
From FADataWarehouse.dbo.RunData r with(nolock) Left Join FADataWarehouse.dbo.ConnectorLaptops s 
	on r.[ConnectorLaptopId] = s.[ConnectorLaptopID] Left join 
	(
		Select *
		From FADataWarehouse.dbo.SummarizedPositiveAssayResults pa with(nolock)
		Where ResultType !='Control' and ResultType !='Gene'
	) pa 
		on pa.RunDataId = r.RunDataId
WHERE r.NoTrend  = 0 and r.SuppressState = 'Trendable' and r.pouchTitle like 'R%' and r.HasBeenCollected = 1
	and s.CustomerSiteId in (26, 2, 7, 13, 10, 5, 29, 36) and YEAR(r.StartTime) >= 2014
Update #Runs  
Set PositiveAssays = 1.0
where Interpretation IS NOT NULL

Create Table #polyConcatedNames 
(RunDataId int, Interpretation nvarchar (1000), PositiveAssays DECIMAL(19,5), Starttime datetime, Year int, Interval int)
Insert Into #polyConcatedNames (RunDataId, PositiveAssays, Starttime, Year, Interval, Interpretation)
SELECT t1.RunDataId, t1.PositiveAssays, t1.Starttime, year(t1.Starttime) as year, datepart(ww, starttime) as Interval, STUFF(
		(SELECT ', ' + t2.Interpretation
			FROM #Runs as t2 with(nolock)
		where t1.RunDataid = t2.RunDataId
		FOR XML PATH (''))
		, 1, 1, '')  AS Interpretation
			FROM (Select RunDataId, Interpretation, PositiveAssays, Starttime
					From #Runs w With (NoLock)  
					)  t1
					Group By t1.RunDataId, Starttime, year(t1.Starttime), t1.PositiveAssays, datepart(ww, starttime)
					order by Interpretation

Select Interpretation as Name, RunCount, PositiveCount
Into #WeeklyRate	
From	
	(Select  Count(*) RunCount
	
			From 
			(
			Select RunDataId
			From #polyConcatedNames  
			Group By 
			 RunDataId
			) as RunCount
	  	  
		) as R

		Cross Join
			
		(Select  Interpretation, ISNULL(sum(PositiveAssays),0) as PositiveCount
			From #polyConcatedNames  
			Group By 
			Interpretation
		) as I
	  
		Order By Interpretation
			
Select
	Name,
	N,
	X,
	R,
	D1X,
	D2X,
	Xpredicted,
	P,
	SE
From(							
	Select *,   
		SQRT((p*(1-p))/n) as SE
	From(
		Select *,  CAST(Xpredicted/N as FLOAT) as P
		From(
			Select 
				p.Name, 
				p.N, 
				p.X, 
				p.X/p.N as R, 
				Detection1, 
				Detection2, 
				a.X as D1X, 
				a2.X as D2X, 
				CEILING((a.X*a2.X)/p.n) as Xpredicted
			From
			(
				Select Name, SUM(RunCount) as N, Sum (positiveCount) as X
					,Left(Name, CHARINDEX(',', (Name))-1) as Detection1
					,Right(Name, CHARINDEX(',',REVERSE(Name))-1) as Detection2
					From #WeeklyRate 
					Where Name like '%,%'
					Group By Name
				) p
					Inner Join (			Select  Name, SUM(RunCount) as N, Sum (positiveCount) as X
				
									From #WeeklyRate
										Group By Name) a  

					on p.Detection1=a.Name
					Inner Join 
								(Select  Name, SUM(RunCount) as N, Sum (positiveCount) as X
									From #WeeklyRate
					Group By Name) a2 on p.Detection2=a2.Name
					Group By p.Name, p.N, p.X, Detection1, Detection2, a.X, a2.X, (a.X*a2.X)/p.n
					) as A
				) as B
			) as C
	

Drop Table #Runs
Drop Table #polyConcatedNames

Drop Table #WeeklyRate
