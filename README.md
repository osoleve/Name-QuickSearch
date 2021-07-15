# QuickSearch

A tool for quickly locating the most likely match
for a name (or other short natural language string) in another (very large) set of names.

Use case for which it was developed: You need to match records across two data
sets using only names, but have too many records to reasonably
perform string distance calculations between every pair in the cartesian set.
QuickSearch only performs distance calculations between strings
that share an entire token, making it well suited to quickly remove the low-hanging
fruit. This in turn drastically reduces the sizes of the sets requiring a
costly full scan, allowing for more iteration and experimentation on the edge cases.

Able to process thousands of names against a population of a hundred thousand names
in under a second (once the QuickSearch is actually built... first search always takes
several seconds to build the filters).

Uses `Data.Text` internally, but there is an identical `String` interface
to be found at `QuickSearch.String` if that suits the pipeline better.

Usage:

```haskell
> import QuickSearch

> names = map T.pack ["Rep. Meg Mueller","Twana Jacobs",...,"Sammie Paucek"]

> targets = zip names [1..] --Stand-in for your UIDs

> qs = buildQuickSearch targets

-- Scorer can be any func of type (T.Text -> T.Text -> Ratio Int)
> entry = pack "Rep. Meg Muller"
> topNMatches qs 1 jaroWinkler entry
[(100,("Rep. Meg Mueller",1))]

> entry = pack "Towana Jacobs"
> matchesWithCutoff qs 90 damerauLevenshteinNorm entry
[(92,("Twana Jacobs",2))]
```

### Batch Usage

Both `topNMatches` and `matchesWithCutoff` have an associated batch function for processing entire lists at once, named `batchTopNMatches` and `batchMatchesWithCutoff`. These take lists of pairs of names and UIDs instead of a target string, and return for each entry the results of running the associated single function.


## QuickSearch.OneShot

If you have your list of names to be matched and list of target names both
in the form `[(T.Text, Int)]` (or `[(String, Int)]`) and you don't plan on re-using
the QuickSearch filters, you can run it as a one-shot batch with

```haskell
import QuickSearch.OneShot
-- or, import QuickSearch.String.OneShot

names, targets :: [(T.Text, Int)]
scorer :: (T.Text -> T.Text -> Ratio Int)

> oneShotGetBestMatches names targets scorer
```
which will return a list of `(entry, [(score, target)])`, where `target`s are the
found names and their UIDs with the highest match score.

`topNMatches` and `matchesWithCutoff` both have similar one-shot batch versions, named
`oneShotTopNMatches` and `oneShotMatchesWithCutoff` respectively.

Shout out to Charles Sommers, who wrote the original tool I'm porting to Haskell.
