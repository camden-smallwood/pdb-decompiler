import ida_auto
import ida_loader
import ida_hexrays
import ida_kernwin
import os

def decompile_to_file(ea, path):
    dir = os.path.dirname(path)
    if not os.path.exists(dir):
        os.makedirs(dir)
    with open(path, "a") as outfile:
        try:
            cf = ida_hexrays.decompile(ea)
            if cf:
                outfile.write(str(cf) + "\n")
            else:
                outfile.write("//decompilation failure at %X!\n" % ea)
        except:
            outfile.write("//decompilation failure at %X!\n" % ea)

print("Waiting for autoanalysis...")
ida_auto.auto_wait()
ida_loader.load_plugin("hexx64")
assert ida_hexrays.init_hexrays_plugin(), "Missing Hexrays Decompiler..."
