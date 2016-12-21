-- Table shows classification of each state according to region
--
CREATE TABLE [FADataWarehouse].[dbo].[EpiRegions]
(
	[State] NVARCHAR(255),
	[StateAbv] NVARCHAR(255),
	[CDCRegion] INT,
	[CensusRegion] INT,
	[CensusRegionLocal] NVARCHAR(255),
	[CensusRegionNational] NVARCHAR(255)
)
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Alabama','AL',4,6,'East South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Alaska','AK',10,9,'Pacific','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Arizona','AZ',9,8,'Mountain','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Arkansas','AR',6,7,'West South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('California','CA',9,9,'Pacific','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Colorado','CO',8,8,'Mountain','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Connecticut','CT',1,1,'New England','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Delaware','DE',3,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Florida','FL',4,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Georgia','GA',4,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Hawaii','HI',9,9,'Pacific','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Idaho','ID',10,8,'Mountain','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Illinois','IL',5,3,'East North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Indiana','IN',5,3,'East North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Iowa','IA',7,4,'West North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Kansas','KS',7,4,'West North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Kentucky','KY',4,6,'East South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Louisiana','LA',6,7,'West South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Maine','ME',1,1,'New England','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Maryland','MD',3,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Massachusetts','MA',1,1,'New England','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Michigan','MI',5,3,'East North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Minnesota','MN',5,4,'West North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Mississippi','MS',4,6,'East South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Missouri','MO',7,4,'West North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Montana','MT',8,8,'Mountain','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Nebraska','NE',7,4,'West North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Nevada','NV',9,8,'Mountain','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('New Hampshire','NH',1,1,'New England','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('New Jersey','NJ',2,2,'Mid-Atlantic','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('New Mexico','NM',6,8,'Mountain','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('New York','NY',2,2,'Mid-Atlantic','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('North Carolina','NC',4,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('North Dakota','ND',8,4,'West North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Ohio','OH',5,3,'East North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Oklahoma','OK',6,7,'West South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Oregon','OR',10,9,'Pacific','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Pennsylvania','PA',3,2,'Mid-Atlantic','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Rhode Island','RI',1,1,'New England','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('South Carolina','SC',4,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('South Dakota','SD',8,4,'West North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Tennessee','TN',4,6,'East South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Texas','TX',6,7,'West South Central','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Utah','UT',8,8,'Mountain','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Vermont','VT',1,1,'New England','Northeast')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Virginia','VA',3,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Washington','WA',10,9,'Pacific','West')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('West Virginia','WV',3,5,'South Atlantic','South')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Wisconsin','WI',5,3,'East North Central','Midwest')
INSERT INTO [FADataWarehouse].[dbo].[EpiRegions] VALUES ('Wyoming','WY',8,8,'Mountain','West')