
" Required for operations modifying multiple buffers like rename.
set hidden

let g:LanguageClient_serverCommands = {
    \ 'go': ['go-langserver'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'typescript': ['javascript-typescript-stdio'],
    \ 'python': ['pyls'],
    \ 'rust': ['rust-analyzer'],
    \ 'haskell': ['haskell-language-server'],
    \ }

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gld :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> glt :call LanguageClient#textDocument_typeDefinition()<CR>
nnoremap <silent> gle :call LanguageClient#explainErrorAtPoint()<CR>
nnoremap <silent> glr :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> gls :call LanguageClient#textDocument_documentSymbol()<CR>
