# lightsaber
Draw lightsabers using Racket's pict library (https://docs.racket-lang.org/pict/index.html)

Created for Stephen De Gabrielle's Summer 2019 standard-fish competition

The lightsaber function produces a pict of a lightsaber. The only required argument is a color, which can be either a color name or a color% object. A length can be provided as an optional argument, as well as a style for the lightsaber hilt. The default hilt is Luke Skywalker's (#:style 'luke), but you can also select Darth Vader's (#:style 'vader), Kylo Ren's (#:style 'kylo), or Darth Maul's (#:style 'maul). See racket-lightsaber.png for examples of each.

Modification History
-------------------------------------------------
07/27/2019   Justin Zamora   Initial creation
