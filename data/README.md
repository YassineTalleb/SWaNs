# Public data files for SWaNs

This directory contains only public example and synthetic inputs that can be shared together with the SWaNs application code.

## Example input

The files in [data/example/](example) mirror the built-in 100-node demonstration case used throughout the app.

- `demo_network_100_adjacency.csv`
- `demo_weights_100.csv`

The adjacency convention is:

- row = flow source
- column = flow target
- a value of `1` means wastewater flows from the row node to the column node

## Synthetic benchmark inputs

The files in [data/synthetic/](synthetic) are synthetic benchmark inputs for demonstration, testing, and reproducibility.

- `synthetic_weights_right_skewed.xlsx`
- `synthetic_weights_uniform.xlsx`
- `synthetic_network_shallow_symmetric.xlsx`
- `synthetic_network_deep_symmetric_depth5_8subtrees.xlsx`
- `synthetic_network_deep_asymmetric.xlsx`

These files are synthetic and do not describe operational real-world sewer infrastructure.

## Real-world data

Detailed real-world sewer-network data are not included in this repository. They are omitted because they contain security-sensitive infrastructure information and are subject to access restrictions.

## Licensing note for data files

The example and synthetic data files are distributed through this repository for reproducibility and testing, but **no separate data license is granted for them**.
