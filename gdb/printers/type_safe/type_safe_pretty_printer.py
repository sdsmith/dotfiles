import gdb.printing

class Type_Safe_Strong_Typedef_Printer:
    "Print type_safe::strong_typedef"

    def __init__(self, val):
        self.val_ = val

    def to_string(self):
        return str(self.val_['value_'])

def build_pretty_printer():
    pp = gdb.printing.RegexpCollectionPrettyPrinter("type_safe_printers")
    pp.add_printer("strong_typedef", "^strong_typedef$", Type_Safe_Strong_Typedef_Printer)
    return pp
