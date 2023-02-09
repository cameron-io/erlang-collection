# Erlang NIFs

### Visual C/C++ (Windows)
`cl /MD /LD complex.c complex6_nif.c /I 'C:\Program Files\erl-24.0\erts-12.0\include' /link /out:complex6_nif.dll /nologo`

### GCC (Unix)
`gcc -o complex6_nif.so -fpic -shared complex.c complex6_nif.c -I "/path/to/erts/include"`
