# Playground for `DerivingVia` language extension of GHC 8.6
Talk Slides (in Japanese): [Speaker Deck][SD] [PDF][PDF], [Keynote][Keynote]

## Explanation
<dl>
<dt>[example.hs](app/example.hs)</dt>
<dd>User-site example of `DerivingVia`, including isomorphic instance derivation exmaples.</dd>
<dt>[Data.Aeson.Generic.DerivingVia](src/Data/Aeson/Generic/DerivingVia.hs)
<dd>
Detailed implementation for the type-driven derivation interface for JSON (de)serialisation with `DerivingVia` and type-level hacks.
</dd>
<dt>[Data.DerivingIso](src/Data/DerivingIso.hs)
<dd>Derivation between isomorphic types with `DerivingVia`, as described in the [original paper][paper].</dd>
<dt>[Data.Foldable.Monoid](src/Data/Foldable/Monoid.hs)
<dd>
Expository module which describes the connectio between `Foldable` (and `Traversable`) and [free monoid constructions][free].
This also utilizes the [`QuantifiedConstraints`][QC] extension to express the freeness as a type-class constraint, which is also introduced in GHC 8.6.
</dd>
</dl>


[SD]: http://bit.ly/derivia
[PDF]: slides/slide.pdf
[Keynote]: slides/slide.key
[paper]: https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf
[free]: https://en.wikipedia.org/wiki/Free_object

[QC]: https://ghc.haskell.org/trac/ghc/wiki/QuantifiedConstraints


