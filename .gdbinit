#
# gdbinit Documentation:
# - User-defined commands:
#   https://sourceware.org/gdb/current/onlinedocs/gdb/Define.html
# - User-defined command hooks:
#   https://sourceware.org/gdb/current/onlinedocs/gdb/Hooks.html#Hooks
#

# Save command history
set history save on
set history size 256
set history remove-duplicates 1
set history filename ~/.gdb_history

#.# Skip STL code during stepping
skip file allocator.h
skip file auto_ptr.h
skip file basic_string.h
skip file basic_string.tcc
skip file move.h
skip file range_access.h
skip file shared_ptr.h
skip file shared_ptr_base.h
skip file stl_deque.h
skip file stl_iterator.h
skip file stl_tree.h
skip file stl_vector.h
skip file unique_ptr.h

add-auto-load-safe-path /lib64/libthread_db-1.0.so

# STL pretty printer
python
import sys
sys.path.insert(0, '/home/scratch.stewarts_sw/gdb/printers/stlprettyprinter/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers(None)

# TODO: add type_safe::strong_typedef pretty printers
end
