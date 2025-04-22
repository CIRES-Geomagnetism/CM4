from numpy import f2py
f2py.compile(open('cm4filed_.F').read(), 'cm4_py312')