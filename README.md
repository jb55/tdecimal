
# TDecimal

Infinite precision Decimal type with lossless rounding

## Examples

```haskell
import Data.TDecimal

-- make a tdec
t = td (100/3)

-- 33.3333

-- make a tdec with a specific precision
t = td 2 (100/3)

-- 33.33

-- it's lossless
t * 3

-- 100.0000

-- change precision at any time
tround 10 t

-- 33.3333333333

-- show the rounded-off remainder
trem $ tround 10 (td (100/3))
-- 1 % 30000000000


```
