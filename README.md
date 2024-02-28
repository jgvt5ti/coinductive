# How to run it
Pass the program to the standard input, then it will print the correspondig HFL forumla.
```
dune exec main.exe << program.text >> hfl.in
```

# Call backend solver
You can solve the productivity of the input program by using HFL(List Z) solver such as [MuHFL](https://github.com/jgvt5ti/muapprox-ls).
If you use MuHFL, run
```
./x --solver eldarica hfl.in
```
