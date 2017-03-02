SELECT DISTINCT *
FROM
(
	SELECT 
		IIF([Interpretation] = 'Influenza A (no subtype detected)', 'Influenza A', [Interpretation]) AS [Organism],
		[ShortName]
	FROM [FADataWarehouse].[dbo].[InterpretationDefinitions]
) T