# crimsopie

DSL for computational operator theory. Compiles .pu8 to ATS2 via .pu4 intermediate form.

Compressed block syntax: \*yG ... /yG*

## Requirements

- ATS2/Postiats 0.4.2
- patscc
- make
- libgc

## Build

```
make clean
make
```

Binary: ./crimsopie

## Usage

```
./crimsopie check <file.pu8>
./crimsopie build <file.pu8>
```

check verifies linearity, locks, dimensions, poles, binding.
build emits .pu4, .sats, .dats and invokes patscc.

## Verification

| rule | condition |
|------|-----------|
| unbound operator | operator used before introduction |
| linearity.violation | re-apply or re-elim of linear operator without ~ or @ lock |
| noncommutative.violation | non-commuting pair without lock |
| numerical_commutativity_check | Frobenius residual above threshold for @ commute |
| lock not released | lock without matching unlock at join |
| dimension.underflow | spectral parameter underflows |
| pole.unhandled | pole declared without handler |
| unapplied operator | introduced operator never applied at join |

Parameters: shared operator Name or linear operator Name / operator Name (default linear).

## Axioms

Trusted praxi: mtx_make, mtx_free, lock_m, unlock_m, step_inv, use_lt, check_exit.

## Examples

| file | expected |
|------|----------|
| parallel_sort.pu8 | sorted =1,2,3,4,5 |
| power_iteration.pu8 | eigval = 4.879385 |
| spectral_decompose.pu8 | A/B/C/D blocks |
| quantum_pair.pu8 | X_on_e0, P_on_e0, locks_ok = 1 |
| fresh_4x4.pu8 | eigval = 4.658173 |
| pair.pu8 | X_on_e0, P_on_e0, locks_ok = 1 |

## Version

0.0.1-alpha (WIP)

## License

BSD 2-Clause. See LICENSE.
