
" Disable default easymotion mappings
let g:EasyMotion_do_mapping = 0

" <Leader>f{char} to move to {char} on screen
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
" nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move within line
nmap f <Plug>(easymotion-bd-f)
vmap f <Plug>(easymotion-bd-f)
" nmap f <Plug>(easymotion-fl)
nmap t <Plug>(easymotion-bd-t)
vmap t <Plug>(easymotion-bd-t)
" nmap F <Plug>(easymotion-Fl)
" nmap T <Plug>(easymotion-Tl)
nmap ; <Plug>(easymotion-next)
nmap , <Plug>(easymotion-prev)
" Actions within line
" nmap dT d<Plug>(easymotion-Tl)
" nmap yT y<Plug>(easymotion-Tl)
" nmap cT c<Plug>(easymotion-Tl)
" nmap dF d<Plug>(easymotion-Fl)
" nmap yF y<Plug>(easymotion-Fl)
" nmap cF c<Plug>(easymotion-Fl)
nmap dt d<Plug>(easymotion-bd-t)
nmap yt y<Plug>(easymotion-bd-t)
nmap ct c<Plug>(easymotion-bd-t)
nmap df d<Plug>(easymotion-bd-f)
nmap yf y<Plug>(easymotion-bd-f)
nmap cf c<Plug>(easymotion-bd-f)
" Jump to any old line
nmap <leader>j <Plug>(easymotion-bd-jk)

" Display upper-case targets, but allow lower-case selection of targets
let g:EasyMotion_use_upper = 1

" Use the easiest keys to reach (they're upper-case because of use upper)
let g:EasyMotion_keys = 'ASDFKJLWEIO'

" Does what it says on the tin
let g:EasyMotion_smartcase = 0

" Silence information messages
let g:EasyMotion_verbose = 0

" Means I can press 9 to go to (.
" TODO: contribute uk key mapping to easymotion, as at present f 2 won't go to "
" let g:EasyMotion_use_smartsign_us = 1
