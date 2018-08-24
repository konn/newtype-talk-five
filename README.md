# Playground for `DerivingVia` language extension of GHC 8.6
* Talk Slides (in Japanese): [Speaker Deck][SD], [PDF][PDF], [Keynote][Keynote]
* English version: [Speaker Deck][SDE], [PDF][PDFE], [Keynote][KeynoteE]

## Explanation
- **[`example.hs`](app/example.hs)**  
  User-site example of `DerivingVia`, including isomorphic instance derivation exmaples.
- **[`Data.Aeson.Generic.DerivingVia`](src/Data/Aeson/Generic/DerivingVia.hs)**  
  Detailed implementation for the type-driven derivation interface for JSON (de)serialisation with `DerivingVia` and type-level hacks.
- **[`Data.DerivingIso`](src/Data/DerivingIso.hs)**  
  Derivation between isomorphic types with `DerivingVia`, as described in the [original paper][paper].
- **[`Data.Foldable.Monoid`](src/Data/Foldable/Monoid.hs)**  
  Expository module which describes the connection between `Foldable` (and `Traversable`) and [free monoid constructions][free].
  This also utilizes the [`QuantifiedConstraints`][QC] extension to express the freeness as a type-class constraint, which is also introduced in GHC 8.6.



[SD]: http://bit.ly/derivia
[PDF]: slides/slide.pdf
[Keynote]: slides/slide.key
[SDE]: http://bit.ly/derivia-eng
[PDFE]: slides/slides-english.pdf
[KeynoteE]: slides/slides-english.key
[paper]: https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf
[free]: https://en.wikipedia.org/wiki/Free_object

[QC]: https://ghc.haskell.org/trac/ghc/wiki/QuantifiedConstraints


