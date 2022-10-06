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

## Input Files
### Configuration File

The configuration file uses the toml file format, the standard name for this toml file is `ferebus.toml`, an example of which can be seen below:

```toml
[system]
name = "WATER"
natoms = 3
atoms = [{name="O1", alf=[1, 2, 3]}]
properties = ["iqa"]

[model]
mean = "constant"
optimiser = "pso"
kernel = "k1+k2"

[optimiser.pso]
swarm_size = 20
iterations = 1
inertia_weight = 0.789
cognitive_learning_rate = 1.494
social_learning_rate = 1.494

[kernels.k1]
type = "rbf"
active_dimensions = [1, 2]

[kernels.k2]
type = "periodic"
active_dimensions = [3]

[notes]
method = "B3LYP"
basis_set = "6-31+g(d,p)"
```

The configuration file has the following sections:

- `system`
  - `name` - system name that will be used to find the training set file
  - `natoms` - number of atoms in the system
  - `atoms` - list of dictionaries containing the `name` of each atom along with it's `alf`
  - `properties` - list of properties to train from the training set file, can be the string `"all"` which will train all properties, will default to `all` if omitted
  - `nfeats` - number of features to use in the training set file, will default to `3*natoms-6` if omitted
- `model`
  - `mean` - specifies mean function to use, can be one of the following:
    - `zero` - uses a zero mean (simple kriging)
    - `constant` - uses a constant mean of the training outputs (ordinary kriging)
    - `linear` - uses a linear mean function
    - `quadratic` - uses a quadratic mean function
  - `optimiser` - currently can only be set to `pso`
  - `kernel` - specifies the kernel composition to use based on the kernels specified in the later `kernels` section
  - `noise` - noise value to use in the GPR (defaults to `1e-10`)
  - `optimise_noise` - boolean toggle to specify whether to optimise the noise or not (defaults to `false`)
  - `likelihood` - likelihood function to use:
    - `marginal` (default)
    - `concentrated` (overrides mean function)
- `optimiser`
  - `search_min` - minimum value to initialise theta to
  - `search_max` - maximum value to initialise theta to
- `optimiser.pso`
  - `swarm_size` - number of particles in the swarm
  - `iterations` - maximum number of iterations to run the PSO for
  - `inertia_weight` - inertia weight control parameter used for velocity update
  - `cognitive_learning_rate` - cognitive learning rate control parameter used for velocity update
  - `social_learning_rate` - social learning rate control parameter used for velocity update
  - `swarm_updater` - algorithm used to update the particles positions, can be one of the following:
    - `global`, `default` - regular swarm update
    - `forced` - uses the fPSO algorithm which forces particles to move if their velocity falls below a threshold
  - `parameter_updater` - algorithm used to update the control parameters during optimisation, can be one of the following:
    - `constant`, `none` - parameters don't change over the PSO run
    - `unified_adaptive`, `uapso`, `ua` - parameters are updated using a learning automaton once per swarm cycle
    - `individual_adaptive`, `iapso`, `ia` - parameters are updated using a learning automaton once per particle update
    - `random_unified`, `random`, `ru` - parameters are updated to random values once per swarm cycle
    - `random_individual`, `ri` - parameters are updated to random values once per particle update
  - `stopping_criteria` - the stopping criteria used to terminate the pso, can be one of the following:
    - `none` - no stopping criteria is used, PSO will run to `iterations`
    - `relative_change` - stops optimisiing when the relative difference of the log-likelihood falls below a threshold
      - `stall_iterations` - specify number of stall iterations for `relative_change`
      - `tolerance` - specify the tolerance for the `relative_change` stopping crtieria
- `kernels.` - each kernel is defined through a `kernels` section with the name of the kernel following the `.` (e.g. `kernels.k1`)
  - `type` - specifies the type of the kernel, can be one of the following:
    - `rbf` - standard RBF kernel
    - `rbf-cyclic` - RBF kernel with cyclic feature correction
    - `periodic` - Exponential sine-squared kernel
    - `linear` - Linear Kernel
  - `active_dimensions` - 1-index list of dimensions the kernel should use (e.g. `[1, 2]` will use the first two features)
- `notes` - key value pairs which are saved and written to the model file in the `[metadata]` section

### Training Set File

The training set file is a comma separated value (csv) file with each column specifying a feature/property and each row specifying a different training point. The file name should be of the form `{system.name}_{system.atom}_TRAINING_SET.csv`, e.g. `WATER_O1_TRAINING_SET.csv`. The first row should be a header line which indicates what each column specifies. A blank header indicates an index column, a header starting with a `f` followed by numbers (e.g. `f1, f2, f3, ...`) indicates a feature column and any other header indicates an output property (e.g. `iqa, q00, ...`). Below is an example:

```csv
,f1,f2,f3,q00, ... ,iqa
0,1.616963022210368,1.7169801391501756,2.106531780913996,-1.2916677642, ... ,-75.483131072
1,2.2927445012455863,1.9952769005395277,1.4599745364814953,-0.91103337252, ... ,-75.44497336
2,1.8768963528713782,1.8877857855487428,1.5831209340077512,-1.0561834928, ... ,-75.481489236
3,1.8215725126136177,1.5739974726494603,1.6641337606436784,-1.1835198694, ... ,-75.474453811
4,2.031804052031453,2.518683146161058,1.4308417261744317,-0.85826580395, ... ,-75.422065546
```

### 
