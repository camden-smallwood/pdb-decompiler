import ida_auto
import ida_lines
import ida_loader
import ida_hexrays
import ida_kernwin
import os

def decompile(ea):
    try:
        pseudocode = ida_hexrays.decompile(ea)
        if pseudocode:
            return str(pseudocode)
    except ida_hexrays.DecompilationFailure:
        print(f"Decompilation failure for function at: {hex(ea)}")
    return None

json_lines = []
def decompile_to_json(ea):
    global json_lines
    pseudocode = decompile(ea)
    if pseudocode == None:
        return
    pseudocode = pseudocode.replace("\\", "\\\\").replace("\n", "\\n").replace("\"", "\\\"")
    json_lines.append(u''.join((f"    \"0x{ea:X}\": \"", pseudocode, "\",\n")))

def decompile_to_file(ea, path):
    dir = os.path.dirname(path)
    if not os.path.exists(dir):
        os.makedirs(dir)
    with open(path, "a") as outfile:
        pseudocode = decompile(ea)
        if pseudocode == None:
            outfile.write("//decompilation failure at %X!\n" % ea)
        else:
            outfile.write(str(pseudocode) + "\n")

print("Waiting for autoanalysis...")
ida_auto.auto_wait()
ida_loader.load_plugin("hexx64")
assert ida_hexrays.init_hexrays_plugin(), "Missing Hexrays Decompiler..."

json_lines.clear()

print("Decompiling...")