Select RowNumber, count(rundataid) as QCruns, IIF(PositiveAssays <3, '>3', '0-3') as PositiveCount
from
(
SELECT ROW_NUMBER() OVER (PARTITION BY Customersiteid, Pouchlotnumber Order by customersiteid, Pouchlotnumber, starttime) 
       AS RowNumber, pouchlotnumber, starttime, rundataid, positiveassays, customersiteid
FROM Rundata r with(nolock) inner join ConnectorLaptops c with(nolock) on c.connectorlaptopid=r.connectorlaptopid
where pouchtitle like 'Resp%' and pouchlotnumber Not LIKE '%[^0-9]%' And LEN(pouchlotnumber)=6
) run
where rownumber < 50 
Group by Rownumber ,IIF(PositiveAssays <3, '>3', '0-3')