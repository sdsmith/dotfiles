#
# Copyright 2019 Stewart Smith
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

#
# gdbinit Documentation:
# - User-defined commands:
#   https://sourceware.org/gdb/current/onlinedocs/gdb/Define.html
# - User-defined command hooks:
#   https://sourceware.org/gdb/current/onlinedocs/gdb/Hooks.html#Hooks
#

set pagination on

# Log gdb output (gdb.txt in working dir)
set logging on

# Log commands
#
# NOTE(sdsmith): this will produce a _ton_ of output if running as part of
# editor integration that polls. Every command you enter will also be echoed
# back to you prefixed witha a "+". Be aware...
#set trace-commands on

# Save command history
set history save on
set history size 10000
set history remove-duplicates 1
set history filename ~/.gdb_history

# Display instructions in Intel format
set disassembly-flavor intel

# Readable structure prints
set print pretty on

# Skip STL files
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
skip file stl_bvector.h

# Skip type_safe files
skip file strong_typedef.hpp

# Skip Google Test files
skip file gtest.h
skip file gtest-message.h
skip file gtest-port.h

# Skip MODS files
skip file strong_typedef_helper.h
skip file memtypes.h
skip file rc.h

# Thread debugging path
add-auto-load-safe-path /lib64/libthread_db-1.0.so

define hookpost-step
  refresh
end

define hookpost-next
  refresh
end

define hookpost-finish
  refresh
end

# Hook 'stop' executes every time execution stops in the program:
# - before breakpoint commands are run
# - displays are printed
# - the stack frame is printed
define hookpost-stop
  refresh
end

# STL pretty printer
python
import sys, os

dotfiles_path = os.getenv("DOTFILES")
if dotfiles_path:
  sys.path.insert(0, f'{dotfiles_path}/gdb/printers/stlprettyprinter/python')
  from libstdcxx.v6.printers import register_libstdcxx_printers
  #sys.path.insert(0, '/home/scratch.stewarts_sw/gdb/printers')
  #from stlprettyprinter.python.libstdcxx.v6.printers import register_libstdcxx_printers
  register_libstdcxx_printers(None)

  # TODO: add type_safe::strong_typedef pretty printers
  # - not tirggering. Try: http://forums.codeblocks.org/index.php?topic=22216.0
  import gdb.printing
  sys.path.insert(0, f'{dotfiles_path}/gdb/printers/type_safe')
  from type_safe_pretty_printer import build_pretty_printer as build_type_safe_printer
  gdb.printing.register_pretty_printer(gdb.current_objfile(), build_type_safe_printer())

end
