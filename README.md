# CarneadesIntoDung

A translation from the Carneades argumentation model
([CarneadesDSL](http://hackage.haskell.org/package/CarneadesDSL)) into Dung's
argumentation frameworks ([Dung](http://hackage.haskell.org/package/Dung)).

This package provides a translation function and correspondence properties
showing that the translation preserves applicability and acceptability.

For the papers accompanying this library see "Towards a framework for the
implementation and verification of translations between argumentation models"
and "A principled approach to the implementation of argumentation models",
available at <https://scholar.google.com/citations?user=Xu4yjvwAAAAJ&hl>.

## Usage

```haskell
import Language.CarneadesIntoDung.Translation
import Language.CarneadesIntoDung.Examples

-- Translate the example CAES into a Dung AF
translate caes

-- Verify correspondence properties
corApp caes  -- True
corAcc caes  -- True
```

## Executable

The `caell` executable reads a Carneades Argument Evaluation Structure from a
file and translates it into a Dung argumentation framework:

```
caell --filename examplecaes.txt --extension
caell --filename examplecaes.txt --correspondence
caell --filename examplecaes.txt --cegartix --outputfile out.txt
```

## License

BSD-3-Clause. See [LICENSE](LICENSE) for details.
