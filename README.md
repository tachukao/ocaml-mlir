# OCaml Bindings to MLIR


## Getting started

1. Build MLIR 

```sh
git clone https://github.com/tachukao/llvm-project.git
mkdir llvm-project/build
cd llvm-project/build
cmake -G Ninja ../llvm \
   -DLLVM_ENABLE_PROJECTS=mlir \
   -DLLVM_BUILD_EXAMPLES=ON \
   -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;AMDGPU" \
   -DCMAKE_BUILD_TYPE=Release \
   -DLLVM_ENABLE_ASSERTIONS=ON \
#  -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DLLVM_ENABLE_LLD=ON

cmake --build . --target check-mlir
```
Note that the these bindings currently only work with my fork of the [llvm-project](https://github.com/tachukao/llvm-project.git) (see related patch [here](https://reviews.llvm.org/D92700)).
Please refer to MLIR's [documentation](https://mlir.llvm.org/getting_started/) for more information.

2. Build the OCaml bindings

```sh
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(llvm-config --libdir) 
cd ocaml-mlir
dune build @install
dune install
dune runtest # test the build
```

## API
The entry point to the bindings is `mlir.core` (see [core.mli](src/core/core.mli)).
It is deliberately low-level, closely matching the MLIR C API.
In the future, we plan to write a higher-level API that wraps around `mlir.core`. 
This is a first shot at OCaml bindings to the MLIR C API and is likely to undergo major changes in the near future. 