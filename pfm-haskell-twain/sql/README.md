## Warning

We store cents as Integers in the `transactions` table.

That's because SQLite doesn't have a proper decimal type.

Very very important:

- You cannot do any computation at the database level
- You need to convert the int to a either a fixed or arbitrary precision type as
  soon as possible (something like "BigDecimal" in Java or Ruby)

If you were to ever do, let's say tax calculation:

- compute the amount without taxes from the total amount
- compute the tax amount from the total amount

These operations would always introduced problematic floats which can lead to
problematic rounding errors (there is no robust solution to this problem).

In other words, for a 100€ price with a 20% tax rate:

- 100 * 0.2 -> no good as there is an implied division here which introduces
  floats
- 100 / 1.2 -> no good here either

Multiplying integers is in theory okay (no loss of information), but probably
not useful at all when manipulating money values (and quite dangerous!).

One trick is to think "hey, I'll just scale the value in order to squash those
rounding errors". But that's a no go, not deterministic as shown by the
following example.

Let's say you want to compute the amount without taxes from the total 999999 for
a 12% VAT rate:

What we want:

```
999999 / 1,12 = 892856,25
```

What we get:

```
> 999999 / 1.12
892856.2499999999
> (999999 * 10 / 1.12) / 10
892856.25
> (999999 * 100 / 1.12) / 100
892856.2499999999
> (999999 * 1000 / 1.12) / 1000
892856.2499999999
> (999999 * 10000 / 1.12) / 10000
892856.25
```

There is no way to determine in advance which scale factor to choose.

You could also think "Hey, just multiply by the VAT basis and divide at the end"

> 999999 * 12/100 119999.88

That could work, for a 20% VAT rate, but will break when introducing a non
integer VAT rate.

2.1% here for example

What we want:

```
(9999999 × 2,1) / 100 = 209999,979
```

What we get

```
> 9999999 * 2.1/9999999 * 2.1100
209999.97900000002
> (9999999 * (2.1 * 10)/100) / 10
209999.979
```

I suppose we could scale up or down the VAT basis (to ensure we always deal with
integers until the end), but still, it's quite dangerous as we can introduce
subtle errors very easily. While this strategy _could_ work, it'd be rather
limiting as you could only derive with vat amount from the without taxes amount,
and **never** either the amount without taxes nor the vat amount from the total
amount with taxes:

```
Compute amount without taxes: oops we would introduce floats via the division
> 100 / 1.2

  100 / 1,2 ≈ 83,33333333

Compute the vat amount: oops, once again we introduce floats via the division!
> 100 / 1.2 * 0.2

  (100 / 1,2) × 0,2 ≈ 16,66666667
```

## Ideas

Maybe look into duckdb! It has a builtin decimal type!!
