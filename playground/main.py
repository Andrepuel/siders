# Compile main.c if needed
import os.path
from re import sub
import subprocess

if not os.path.exists("main.h"):
    subprocess.check_call(["cargo", "build"])

if not os.path.exists("libmain.so"):
    subprocess.check_call(["gcc", "-shared", "-g", "-o", "libmain.so", "main.c"])

# Interop code


import ctypes
def siders_malloc_words(len):
    buffer = ctypes.create_string_buffer(ctypes.sizeof(ctypes.c_void_p) * (len + 1))
    ptr = ctypes.cast(buffer, ctypes.POINTER(ctypes.c_void_p))
    ptr_1 = ctypes.cast(ctypes.byref(ptr.contents, 8), ctypes.POINTER(ctypes.c_void_p))

    ptr_self = ctypes.py_object(buffer)
    ctypes.pythonapi.Py_IncRef(ptr_self)
    ptr[0] = ctypes.c_void_p(id(ptr_self.value))

    return ctypes.cast(ctypes.cast(ptr_1, ctypes.c_void_p), ctypes.POINTER(ctypes.c_void_p))


def siders_dealloc(ptr):
    ptr_self = ctypes.cast(ptr[-1], ctypes.py_object)
    ctypes.pythonapi.Py_DecRef(ptr_self)

ProviderVtableDestruct = ctypes.CFUNCTYPE(None, ctypes.POINTER(ctypes.c_void_p))
ProviderVtablePrint = ctypes.CFUNCTYPE(None, ctypes.POINTER(ctypes.c_void_p), ctypes.c_int8)
class ProviderVtable:
    def __init__(self):
        def func_destruct(ptr):
            obj = ctypes.cast(ptr[1], ctypes.py_object)
            ctypes.pythonapi.Py_DecRef(obj)
            siders_dealloc(ptr)
        self.func_destruct = ProviderVtableDestruct(func_destruct)

        def func_print(obj, a1):
            ptr_self = ctypes.cast(obj[-1], ctypes.POINTER(ctypes.py_object))
            obj = ctypes.cast(obj[1], ctypes.py_object)
            obj.value.print(a1)
            gc.collect()

        self.func_print = ProviderVtablePrint(func_print)

        self.buffer = siders_malloc_words(2)
        self.buffer[0] = ctypes.cast(self.func_destruct, ctypes.c_void_p)
        self.buffer[1] = ctypes.cast(self.func_print, ctypes.c_void_p)

vtableProviderVtable = ProviderVtable()

main_lib = ctypes.CDLL("./libmain.so")
main_lib.Main_set.argtypes = [ctypes.c_void_p]
main_lib.Main_set.restype = None
main_lib.Main_run.argtypes = []
main_lib.Main_run.restype = None

class Main:
    @staticmethod
    def set(provider):
        ptr = siders_malloc_words(2)
        provider = ctypes.py_object(provider)
        ctypes.pythonapi.Py_IncRef(provider)

        ptr[0] = ctypes.cast(vtableProviderVtable.buffer, ctypes.c_void_p)
        ptr[1] = ctypes.c_void_p(id(provider.value))

        main_lib.Main_set(ctypes.cast(ptr, ctypes.c_void_p))

    @staticmethod
    def run():
        main_lib.Main_run()

# Client code
import sys

class Puel:
    def print(self, a1):
        sys.stdout.write(chr(a1))

    def __del__(self):
        print("")
        print("Done")

import gc

Main.set(Puel())
gc.collect()
Main.run()
gc.collect()