QuickSearch
---

WIP (but it does work now, it's just missing some
convenience functions for batch processing)

A framework for quickly locating the most likely match
for a name (or other short string) in another (very large) set of names.

Use case for which it was developed: You need to match records across two data
sets using only names, but have too many records to reasonably
perform string distance calculations between every pair in the cartesian set.
QuickSearch only performs distance calculations between strings
that share an entire token, making it well suited to quickly remove the low-hanging
fruit. This in turn drastically reduces the sizes of the sets requiring a
costly full scan, allowing for more iteration and experimentation on the edge cases.

On my machine, in ghci, QuickSearch can retrieve the best matches for a string
from a population of 100,000 strings in under a quarter of a second.

Usage:

```haskell
> names = ["Rep. Meg Mueller","Twana Jacobs","Terrell Hintz","Fr. Jettie Satterfield","Mr. Robert Robel","Alonso Rodriguez III","Brendan Hyatt","Rep. Kazuko Price","Sammie Paucek","Dewey Armstrong MD"]

> entries = zip names [1..] --Stand-in for your UIDs
> entries
[("Rep. Meg Mueller",1),("Twana Jacobs",2),("Terrell Hintz",3),("Fr. Jettie Satterfield",4),("Mr. Robert Robel",5),("Alonso Rodriguez III",6),("Brendan Hyatt",7),("Rep. Kazuko Price",8),("Sammie Paucek",9),("Dewey Armstrong MD",10)]

-- Scorer can be any func of type (T.Text -> T.Text -> Ratio Int)
> qs = buildQuickSearch entries damerauLevenshteinNorm

> target = pack "Towana Jacobs"

> getTopMatches 5 target qs
[(92,("Twana Jacobs",2))]

> getMatchesWithCutoff 90 target qs
[(92,("Twana Jacobs",2))]
```

Shout out to Charles Sommers, who wrote the original tool I'm porting to Haskell.
