--DROP TABLE [PMS1].[dbo].[tTrendRegionKey]
CREATE TABLE [PMS1].[dbo].[tTrendRegionKey]
(
	[State] VARCHAR(50),
	[StateAbv] VARCHAR(10),
	[CDCRegion] INT,
	[CensusRegion] INT,
	[CensusRegionLocal] VARCHAR(50),
	[CensusRegionNational] VARCHAR(50)
)

INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Alabama','AL',4,6,'East South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Alaska','AK',10,9,'Pacific','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Arizona','AZ',9,8,'Mountain','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Arkansas','AR',6,7,'West South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('California','CA',9,9,'Pacific','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Colorado','CO',8,8,'Mountain','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Connecticut','CT',1,1,'New England','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Delaware','DE',3,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Florida','FL',4,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Georgia','GA',4,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Hawaii','HI',9,9,'Pacific','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Idaho','ID',10,8,'Mountain','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Illinois','IL',5,3,'East North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Indiana','IN',5,3,'East North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Iowa','IA',7,4,'West North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Kansas','KS',7,4,'West North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Kentucky','KY',4,6,'East South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Louisiana','LA',6,7,'West South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Maine','ME',1,1,'New England','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Maryland','MD',3,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Massachusetts','MA',1,1,'New England','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Michigan','MI',5,3,'East North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Minnesota','MN',5,4,'West North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Mississippi','MS',4,6,'East South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Missouri','MO',7,4,'West North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Montana','MT',8,8,'Mountain','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Nebraska','NE',7,4,'West North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Nevada','NV',9,8,'Mountain','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('New Hampshire','NH',1,1,'New England','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('New Jersey','NJ',2,2,'Mid-Atlantic','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('New Mexico','NM',6,8,'Mountain','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('New York','NY',2,2,'Mid-Atlantic','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('North Carolina','NC',4,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('North Dakota','ND',8,4,'West North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Ohio','OH',5,3,'East North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Oklahoma','OK',6,7,'West South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Oregon','OR',10,9,'Pacific','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Pennsylvania','PA',3,2,'Mid-Atlantic','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Rhode Island','RI',1,1,'New England','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('South Carolina','SC',4,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('South Dakota','SD',8,4,'West North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Tennessee','TN',4,6,'East South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Texas','TX',6,7,'West South Central','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Utah','UT',8,8,'Mountain','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Vermont','VT',1,1,'New England','Northeast')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Virginia','VA',3,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Washington','WA',10,9,'Pacific','West')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('West Virginia','WV',3,5,'South Atlantic','South')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Wisconsin','WI',5,3,'East North Central','Midwest')
INSERT INTO [PMS1].[dbo].[tTrendRegionKey] VALUES ('Wyoming','WY',8,8,'Mountain','West')

SELECT *
FROM [PMS1].[dbo].[tTrendRegionKey]
