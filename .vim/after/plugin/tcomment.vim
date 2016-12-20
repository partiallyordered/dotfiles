
if exists('tcommentMaps')
    " Can't use nnoremap here because we require tcomments mappings
    " Empty comment on next line
    nmap <leader>tcj o<c-_>i
    " Empty comment on previous line
    nmap <leader>tck O<c-_>i
    " Empty comment at end of line
    nmap <leader>tca A <c-_>i
    " Empty TODO on previous line
    nmap <leader>td O<c-_>iTODO: 
endif
