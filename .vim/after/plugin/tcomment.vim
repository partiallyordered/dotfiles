
" Can't use nnoremap here because we require tcomments mappings
" Empty comment on next line
nmap <leader>tcj o<c-_>i
" Empty comment on previous line
nmap <leader>tck O<c-_>i
" Empty comment at end of line
nmap <leader>tca A <c-_>i
" Empty comment correctly indented on the current line
nmap <leader>tcc cc<c-_>i
" Empty TODO on previous, next, current line
" TODO: use <leader>tdj from within a comment adds a new comment. Fix this.
" (perhaps with o<c-c>cc before adding a comment.)
" TODO: Some filetypes don't have a space after their comment by default.
" This can be seen when using <leader>tdj in a python file, then in a c/cpp
" file.  Either work out how to mitigate this, by deleting back to the
" comment character, or by entering a space for filetypes that don't have
" one, and not entering a space for those that do.
nmap <leader>tdk O<c-_>i TODO: 
nmap <leader>tdj o<c-_>i TODO: 
nmap <leader>tda A <c-_>i TODO: 
nmap <leader>tdd cc<c-_>i TODO: 
" Duplicate the current line, but comment it afterward
nmap <leader>tcy yygccp
" gcac does what I think gcic should do, and I keep using gcic instead
nmap gcic gcac
