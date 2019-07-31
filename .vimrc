"""
" u                     undo
" v                     select chars
" V                     select lines
" C-v                   select block
" d                     cut text (delete)
" y                     copy text (yank)
" p                     paste after cursor
" P                     paste before cursor
" i                     Insert mode
" a                     Append mode (insert mode with cursor at EOL)
" {                     move up block
" }                     move down block

" Replace tabs with 4 spaces
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

" Colour scheme
colorscheme morning

" 
" Custom syntax highlighting
"
" Each type of highlighting is called a 'group' and are part of 'highlight
" groups'. Search http://vimdoc.sourceforge.net/htmldoc/syntax.html for 
" 'highlight groups' to see the list of generic groups. See all highlight
" groups for the current buffer with ':so $VIMRUNTIME/syntax/hitest.vim'.
"
" Controlled through [hi]ghlight. Font key value terms are optional.
" highlight <group> <font_key_value_terms>
" Ex/
"   highlight Constant cterm=underline ctermfg=White ctermbg=Black
"
" Font key-value terms:
"  cterm
"    bold
"    underline
"    reverse
"    italic
"    none
"  ctermfg
"  ctermbg
"
" Colors:
"  NR-16   NR-8    COLOR NAME 
"  0       0       Black
"  1       4       DarkBlue
"  2       2       DarkGreen
"  3       6       DarkCyan
"  4       1       DarkRed
"  5       5       DarkMagenta
"  6       3       Brown, DarkYellow
"  7       7       LightGray, LightGrey, Gray, Grey
"  8       0*      DarkGray, DarkGrey
"  9       4*      Blue, LightBlue
"  10      2*      Green, LightGreen
"  11      6*      Cyan, LightCyan
"  12      1*      Red, LightRed
"  13      5*      Magenta, LightMagenta
"  14      3*      Yellow, LightYellow
"  15      7*      White
"
highlight Identifier ctermfg=DarkYellow
highlight Statement cterm=none ctermfg=DarkYellow

" Highlight current line
set cursorline
highlight CursorLine cterm=none ctermbg=Grey
