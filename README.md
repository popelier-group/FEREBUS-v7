# FEREBUS7

FEREBUS v7 is a Gaussian Process Regression engine written in Fortran90 designed to produce models for atomistic simulations. FEREBUS uses Particle Swarm Optimisation accelerated using OpenMP and OpenAcc.

## How to Build

FEREBUS requires cmake (at least version 3.5) and an appropriate Fortran90 compiler capable of compiling Fortran 2008 code. If cmake cannot find precompiled BLAS and LAPACK routines it will download from github and build from source greatly increasing build time and likely resulting in a slower binary. If this occurs, one can manually set the BLAS/LAPACK location using `-DBLAS_LOCATION=...` when running the `cmake` command.

```bash
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make
```

To compile for GPU usage, either the PGI or NVHPC compilers are required as the GPU kernels are implemented using OpenAcc.

## How to Run

There are examples in the `examples/` directory of input files for a water model. Running the example will produce an O1 IQA model.

Typically, to run FEREBUS, a config file (in the toml format) and a training set file (in the csv format) is required. An example `ferebus.toml` file can be found in the `examples/WATER/` directory along with example training set files. The default config filename is `ferebus.toml`, if this is the case FEREBUS may be ran with the following:
```bash
./ferebus
```

If the name of the config file differs from `ferebus.toml`, one can manually set the config file name using the following:
```bash
./ferebus -c config.toml
```

A full list of arguments may be found using the `-h` flag.

FEREBUS is parallelised using OpenMP and as such the number of threads FEREBUS uses can be controlled via the `OMP_NUM_THREADS` environment variable. Note that FEREBUS is parallelised over the particles in the swarm so optimal usage may be achieved by ensuring the number of threads is a factor of the number of particles.
