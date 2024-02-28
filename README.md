# How to run it
You can use docker image [Here].
MuHFL uses [RETHFL](https://github.com/jgvt5ti/hflmc2-ls) as a backend solver, so download it.
Then you can run the image by:
```
docker run -it -v <path to RETHFL>:/opam/repos/hflmc2-ls  -v <path to MUHFL>:/opam/repos/muapprox-ls  -v <path to it>:/opam/repos/coinductive
```
To transform the target program to hfl formula, pass the program to the standard input, then it will print the correspondig HFL forumla.
```
cd /opam/repos/coinductive
dune exec main.exe << program.text >> hfl.in
```
# Call backend solver
You can solve the productivity of the input program by using HFL(List Z) solver such as [MuHFL](https://github.com/jgvt5ti/muapprox-ls).
If you use MuHFL, run
```
cd ../muapprox-ls
./x --solver eldarica hfl.in
```
