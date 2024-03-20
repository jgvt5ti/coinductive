# Syntax
The program inputs lambda calculus with fix points defined below:
```
  M ::= IDENT | n | M_0 M_1 | \ IDENT. M | fix IDENT .M | [M (; M)*] | prj i (M) | inj i (M) | case M (IDENT. M ; IDENT. M)
```
IDENT ranges over string which start from a lowercase letter.
The program outputs a HFL formula that represent the productivity of the input program.
# How to run it
You can use docker image [Here].
To transform the target program to hfl formula, pass the program, then it will print the correspondig HFL forumla.
```
cd /opam/repos/coinductive
dune exec main.exe program.text >> hfl.in
```
# Call backend solver
You can solve the productivity of the input program by using HFL(List Z) solver such as [MuHFL](https://github.com/jgvt5ti/muapprox-ls).
If you use MuHFL, run
```
cd ../muapprox-ls
./x --solver eldarica hfl.in
```
