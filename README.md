# Dynamo

**Dynamo** implements a dynamic JavaScript-like (although more strict) typing system in Nim.

Primary usecase of this library being the creation of scripting languages
in Nim, dynamo takes care of all the boilerplate code for supporting
dynamic objects and their interactions.

## Installation

Library can be installed with nimble package manager:

```console
$ nimble install "https://github.com/cmd410/dynamo"
```

## Usage

Here is some demo of what this library is capable of:

```nim
import dynamo

# Create some variant objects
let n = nil.toVariant
let i = 42.toVariant
let f = 12.0.toVariant
let b = true.toVariant

# Lists and objects
var l = @[1, 2, 3].toVariant
var d = {"key": @[1, 2, 3]}.toVariant

# Lists can contain any kind of variants
l = l & n
l = l & i
l = l & f
l = l & (f + i)
l = l & b
echo l  # @[1, 2, 3, null, 42, 12.0, 54.0, true]

# Objects too
d.key2 = l
echo d  # {"key": @[1, 2, 3], "key2": @[1, 2, 3, null, 42, 12.0, 54.0, true]}

# Implement custom operators easily
func someOp[T: float | int](a, b: T): T = a + b * (a + b)

binOps:
  someOp:
    # left side accepts argument Variant argument `a` of types float and int and assigns it's value to `x`
    left as a from [float, int] as x:
      int:  # when x is int
        when y is float:
          someOp(x.toFloat(),y)
        else:
          someOp(x,y)
    # right side accepts argument Variant argument `right` of types float and int and assigns it's value to `y`
    right from [float, int] as y:
      int:  # when y is int
        when x is float:
          someOp(x, y.toFloat())
        else:
          someOp(x,y)

echo someOp(42.toVariant, 15.0)  # 897.0
```

If you want API reference you can generate it with command:

```console
$ nimble docs
```

Run it in repository root directory after cloning.
Then html documnetation should appear in `docs` subdirectory.
Yeah, it's awkward, but I am too lazy to
host it online rn, but some day I surely will.
