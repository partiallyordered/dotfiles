
if exists(':YcmForceCompileAndDiagnostics')
    nnoremap <F5> :YcmForceCompileAndDiagnostics<CR>
    nnoremap <leader>df :YcmCompleter GoToDefinition<CR>
    nnoremap <leader>dc :YcmCompleter GoToDeclaration<CR>
endif
