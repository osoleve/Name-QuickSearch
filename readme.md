QuickSearch
---

WIP (but it does work now, it's just missing some
convenience functions for batch processing)

A framework for quickly locating the most likely match
for a name (or other short string) in another set of names.
Due to the heuristic it uses to speed up search, it's well
suited for a first pass to reduce the population to be searched:
It uses maps to partition the search space to only attempt the
expensive calculations on strings that share an entire token with
the target string.

Dependencies: Data.Text.Metrics

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
```
