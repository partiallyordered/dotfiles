{ config, pkgs, lib, ... }:

''
-- # lua
-- TODO: how to avoid duplicating this stuff:
local capabilities = require('cmp_nvim_lsp').default_capabilities()

------------------------------------------------------------------------------
-- DERIVED FROM https://github.com/mfussenegger/nvim-jdtls/tree/87bdf2b216f3346abb099704c9bb45e5eb9df43a
------------------------------------------------------------------------------
-- TODO: this section should go in ftplugin/java.lua (i.e. only be executed for java files) as per
-- https://github.com/mfussenegger/nvim-jdtls#configuration.

-- project_name and workspace_dir derived from:
-- https://github.com/mfussenegger/nvim-jdtls#data-directory-configuration
-- If you started neovim within `~/dev/xy/project-1` this would resolve to `project-1`
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
-- local workspace_dir = '$XDG_CACHE_HOME/nvim-jdtls/workspaces/' .. project_name
local workspace_dir = '/home/msk/.cache/jdtls/workspaces/' .. project_name
-- TODO: *does it matter what the name of this function is?!* It seems like changing the name of
-- this function causes breakage.. Did I miss something?!
local on_attach = function(client, bufnr)
  require('jdtls.setup').add_commands()
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  -- TODO: For some reason, omnifunc doesn't seem to get set, but everything else seems to work.
  -- It's possible to run:
  --   :set omnifunc="v:lua.vim.lsp.omnifunc"
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap = false, silent = false }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'glf', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gld', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gli', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', 'glt', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', 'glr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>lde', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  buf_set_keymap('n', '<space>ldp', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', '<space>ldn', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  -- buf_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>lf", "<cmd>lua vim.lsp.buf.format()<CR>", opts)
  buf_set_keymap('v', "<space>lf", "<cmd>lua vim.lsp.buf.format()<CR>", opts)
end

-- See `:help vim.lsp.start_client` for an overview of the supported `config` options.
local nvim_jdtls_config = {
  -- The command that starts the language server
  -- See: https://github.com/eclipse/eclipse.jdt.ls#running-from-the-command-line
  cmd = {
    -- 'JAVA_OPTS="-javaagent:${pkgs.lombok}/share/java/lombok.jar -Xbootclasspath/a:${pkgs.lombok}/share/java/lombok.jar"',
    -- 'JAVA_OPTS="-javaagent:/nix/store/pw7s22bk6ngn5mv952bmqlg6p0w1aw05-lombok-1.18.24/share/java/lombok.jar -Xbootclasspath/a:/nix/store/pw7s22bk6ngn5mv952bmqlg6p0w1aw05-lombok-1.18.24/share/java/lombok.jar"',
    -- /nix/store/pw7s22bk6ngn5mv952bmqlg6p0w1aw05-lombok-1.18.24/share/java/lombok.jar
    -- Nix gives us a convenient wrapper that supplies the necessary arguments to jdtls to get it working:
    -- https://github.com/NixOS/nixpkgs/blob/6cb5aad76b9bbb6fb61938982d68d3e19e4aa16f/pkgs/development/tools/jdt-language-server/default.nix#L79-L93
    '${pkgs.jdk17}/bin/java',
    '-Declipse.application=org.eclipse.jdt.ls.core.id1',
    '-Dosgi.bundles.defaultStartLevel=4',
    '-Declipse.product=org.eclipse.jdt.ls.core.product',
    '-Dosgi.sharedConfiguration.area=${pkgs.jdt-language-server}/share/config',
    '-Dosgi.sharedConfiguration.area.readOnly=true',
    '-Dosgi.checkConfiguration=true',
    '-Dosgi.configuration.cascaded=true',
    '-Dlog.level=ALL',
    '-noverify',
    '-javaagent:${pkgs.lombok}/share/java/lombok.jar',
    '-jar', '${pkgs.jdt-language-server}/share/java/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar',
    '--add-modules=ALL-SYSTEM',
    '--add-opens', 'java.base/java.util=ALL-UNNAMED',
    '--add-opens', 'java.base/java.lang=ALL-UNNAMED',
    '-Dlog.protocol=true',

    -- From: https://github.com/mfussenegger/nvim-jdtls/issues/28
    -- '-javaagent:${pkgs.lombok}/share/java/lombok.jar',
    -- '-Xbootclasspath/a:${pkgs.lombok}/share/java/lombok.jar',

    -- 💀
    -- See `data directory configuration` section in the README
    '-data', workspace_dir
  },

  on_attach = on_attach,

  -- TODO: what does this actually do?
  flags = {
    allow_incremental_sync = true,
  },

  capabilities = capabilities,

  -- 💀
  -- This is the default if not provided, you can remove it. Or adjust as needed.
  -- One dedicated LSP server & client will be started per unique root_dir
  root_dir = require('jdtls.setup').find_root({'.git', 'mvnw', 'gradlew'}),

  -- Here you can configure eclipse.jdt.ls specific settings
  -- See https://github.com/eclipse/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
  -- for a list of options
  settings = {
    java = {
    }
  },

  -- Language server `initializationOptions`
  -- You need to extend the `bundles` with paths to jar files
  -- if you want to use additional eclipse.jdt.ls plugins.
  --
  -- See https://github.com/mfussenegger/nvim-jdtls#java-debug-installation
  --
  -- If you don't plan on using the debugger or other eclipse.jdt.ls plugins you can remove this
  init_options = {
    bundles = {}
  },
}
-- This starts a new client & server,
-- or attaches to an existing client & server depending on the `root_dir`.
require('jdtls').start_or_attach(nvim_jdtls_config)
''
