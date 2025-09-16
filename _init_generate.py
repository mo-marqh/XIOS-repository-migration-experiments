import os
import ast

MODULE_DIR = "src/interface/python/xios"
OUTPUT_FILE = os.path.join(MODULE_DIR, "__init__.py")

all_classes = []
all_functions = []


def is_top_level_class(node):
    return isinstance(node, ast.ClassDef)

def is_top_level_function(node):
    return isinstance(node, ast.FunctionDef)

def is_top_level_assignment(node):
    return isinstance(node, ast.Assign)

def is_top_level_import(node):
    return isinstance(node, (ast.Import, ast.ImportFrom))

for filename in os.listdir(MODULE_DIR):
    if filename.endswith(".py") and filename != "__init__.py":
        module_name = filename[:-3]
        with open(os.path.join(MODULE_DIR, filename), "r") as f:
            tree = ast.parse(f.read(), filename=filename)

        classes = [n.name for n in tree.body if is_top_level_class(n)]
        functions = [n.name for n in tree.body if is_top_level_function(n)]

        assigned_names = []
        for node in tree.body:
            if is_top_level_assignment(node):
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        assigned_names.append(target.id)

        imported_names = []


        all_classes.append((module_name, classes))
        all_functions.append((module_name, functions))


with open(OUTPUT_FILE, "w") as f:

    for module, classes in all_classes:
        for cls in classes:
            if cls not in ['Object', 'CObject', 'XIOS']:
                f.write(f"from .{module} import {cls}\n")



    for module, functions in all_functions:
        for func in functions:
            if func not in ['dynamic_methods', 'initialize', 'finalize', 'typecheck', 'swipswap_ctx'] and func[-1]!='_': 
                f.write(f"from .{module} import {func}\n")

    f.write(f"from .odata import initialize\n")
    f.write(f"from .odata import finalize\n")


print(" __init__.py generated successfully with the following items:")
print("\n Classes:")
for module, classes in all_classes:
    for cls in classes:
        print(f"  - {cls} (from {module}.py)")

print("\n Functions:")
for module, functions in all_functions:
    for func in functions:
        print(f"  - {func} (from {module}.py)")

