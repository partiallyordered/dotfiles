
" Disable default easymotion mappings
let g:EasyMotion_do_mapping = 0

" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
" nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move within line
nmap f <Plug>(easymotion-fl)
nmap F <Plug>(easymotion-Fl)
nmap t <Plug>(easymotion-tl)
nmap T <Plug>(easymotion-Tl)
nmap ; <Plug>(easymotion-repeat)

" Display upper-case targets, but allow lower-case selection of targets
let g:EasyMotion_use_upper = 1

" Use the easiest keys to reach (they're upper-case because of use upper)
let g:EasyMotion_keys = 'ASDFKJL;WEIO'

" Does what it says on the tin
let g:EasyMotion_smartcase = 1

" Silence information messages
let g:EasyMotion_verbose = 0

" Means I can press 9 to go to (.
" TODO: contribute uk key mapping to easymotion, as at present f 2 won't go to "
" let g:EasyMotion_use_smartsign_us = 1
