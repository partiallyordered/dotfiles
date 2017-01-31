
if exists('tcommentMaps')
    " Can't use nnoremap here because we require tcomments mappings
    " Empty comment on next line
    nmap <leader>tcj o<c-_>i
    " Empty comment on previous line
    nmap <leader>tck O<c-_>i
    " Empty comment at end of line
    nmap <leader>tca A <c-_>i
    " Empty TODO on previous, next, current line
    nmap <leader>tdk O<c-_>i TODO: 
    nmap <leader>tdj o<c-_>i TODO: 
    nmap <leader>tdd A<c-_>i TODO: 
    " Duplicate this line, but comment it afterward
    nmap <leader>tcy yygccp
endif
