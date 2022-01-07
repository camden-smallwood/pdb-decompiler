import ida_auto
import ida_loader
import ida_hexrays
import ida_kernwin

def decompile_to_file(ea, path):
    with open(path, "a") as outfile:
        ida_kernwin.msg("Decompiling at: %X..." % ea)
        try:
            cf = ida_hexrays.decompile(ea)
            if cf:
                ida_kernwin.msg("OK\n")
                outfile.write(str(cf) + "\n")
            else:
                ida_kernwin.msg("failed!\n")
                outfile.write("//decompilation failure at %X!\n" % ea)
        except:
            ida_kernwin.msg("failed!\n")
            outfile.write("//decompilation failure at %X!\n" % ea)

print("Waiting for autoanalysis...")
ida_auto.auto_wait()
ida_loader.load_plugin("hexx64")
assert ida_hexrays.init_hexrays_plugin(), "Missing Hexrays Decompiler..."
