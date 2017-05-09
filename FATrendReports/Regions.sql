SET NOCOUNT ON
Select Province, 
Case 
When Province  in ('CT', 'MA', 'RI', 'NH', 'VT', 'ME') then 1 
When  Province  in ('PA', 'NJ', 'NY') then 2 
When Province  in ('OH', 'IN', 'IL', 'MI', 'WI') then 3
When Province  in ('IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD') then 4
When Province  in ('DE', 'DC', 'FL', 'GA', 'MD', 'NC', 'SC', 'VA', 'WV') then 5
When Province  in ('AL', 'KY', 'MS', 'TN') then 6 
When Province  in ('AR', 'LA', 'OK', 'TX') then 7 
When Province  in ('AZ', 'CO', 'ID', 'NM', 'MT', 'UT', 'NV', 'WY') then 8
When Province  in ('AK', 'CA', 'HI', 'OR', 'WA') then 9

End
As Div
Into #Regions 
From Customers 

Select *,
Case 
When div=1 then 'New England'
When div=2 then 'Middle Atlantic'
When div=3 then 'East North Central'
When div=4 then 'West North Central'
When div=5 then 'South Atlantic'
When div=6 then 'East South Central'
When  div=7 then 'West South Central'
When div=8 then 'Mountain'
When  div=9 then 'Pacific'
End as DivName,

Case 
When Div in (1, 2)   then 'Northeast'
When Div in (3, 4)   then 'Midwest'
When Div  in (5 ,6, 7) then 'South'
When Div  in (8, 9)   then 'West'
End
As Reg
Into #Regions2
From #Regions
Select * from #Regions2
Drop table #regions2, #Regions