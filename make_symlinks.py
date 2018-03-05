#!/usr/bin/env python3

#TODO: colorise ok/warning output- it'd be nice to see all-green for 'symlink
#exists' orange for warnings and say blue for 'made a symlink for x file'

#TODO: two directories, one which mirrors the structure of files in the user's
#home directory, and another for which mappings need to be provided?

if __name__ == "__main__":
    import os
    from collections import namedtuple

    Dotfile = namedtuple('Dotfile', ['localaddr', 'targetaddr'])

    curdir = os.path.dirname(os.path.realpath(__file__)) + '/'
    home = os.path.expanduser('~') + '/'
    # TODO: handle wildcards here? Let .gitignore handle white/blacklisting
    # files? Use some py-git to check what's actually in version control?
    local_to_target_map = [
            ['init.vim', '/.config/nvim/init.vim'],
            ['.vim/after/plugin/incsearch.vim', '/.config/nvim/after/plugin/incsearch.vim'],
            ['.vim/after/plugin/ycm.vim', '/.config/nvim/after/plugin/ycm.vim'],
            ['.vim/after/plugin/easymotion.vim', '/.config/nvim/after/plugin/easymotion.vim'],
            ['.vim/after/plugin/tcomment.vim', '/.config/nvim/after/plugin/tcomment.vim'],
            ['.vim/after/plugin/sideways.vim', '/.config/nvim/after/plugin/sideways.vim'],
            ['.vim/after/plugin/delimitmate.vim', '/.config/nvim/after/plugin/delimitmate.vim'],
            ['.vim/after/plugin/youcompleteme.vim', '/.config/nvim/after/plugin/youcompleteme.vim'],
            ['.tern-config', '/.tern-config'],
            ['.npmrc', '/.npmrc'],
            ['.gitconfig', '/.gitconfig'],
            ['.spacemacs', '/.spacemacs'],
            ['.sackrc', '/.sackrc'],
            ['.ignore', '/.ignore'],
            ['xmonad.hs', '/.xmonad/xmonad.hs'],
            ['.vimperatorrc', '/.vimperatorrc'],
            ['.xscreensaver', '/.xscreensaver'],
            ['.zshrc', '/.zshrc'],
            ['UltiSnips/c_my.snippets', '/.config/nvim/UltiSnips/c_my.snippets'],
            ['UltiSnips/cpp_my.snippets', '/.config/nvim/UltiSnips/cpp_my.snippets'],
            ['UltiSnips/html_my.snippets', '/.config/nvim/UltiSnips/html_my.snippets'],
            ['UltiSnips/javascript_my.snippets', '/.config/nvim/UltiSnips/javascript_my.snippets'],
            ['UltiSnips/make.snippets', '/.config/nvim/UltiSnips/make.snippets'],
            ['UltiSnips/sh_my.snippets', '/.config/nvim/UltiSnips/sh_my.snippets'],
            ['.Xresources', '.Xresources'],
            ['.xinitrc', '.xinitrc'],
            ['ranger/commands_full.py', '/.config/ranger/commands_full.py'],
            ['ranger/commands.py', '/.config/ranger/commands.py'],
            ['ranger/rc.conf', '/.config/ranger/rc.conf'],
            ['ranger/rifle.conf', '/.config/ranger/rifle.conf'],
            ['ranger/scope.sh', '/.config/ranger/scope.sh'],
            ['alacritty.yml', '/.config/alacritty/alacritty.yml'],
            ['bin/remind', '/bin/remind'],
            ['bin/dzenotify', '/bin/dzenotify'],
            ['rebar3/templates/gen_server.erl', '/.config/rebar3/templates/gen_server.erl'],
            ['rebar3/templates/gen_server.template', '/.config/rebar3/templates/gen_server.template'],
            ['rebar3/templates/cowboy_http.erl', '/.config/rebar3/templates/cowboy_http.erl'],
            ['rebar3/templates/cowboy_http.template', '/.config/rebar3/templates/cowboy_http.template'],
            ['rebar3/templates/cowboy_loop.erl', '/.config/rebar3/templates/cowboy_loop.erl'],
            ['rebar3/templates/cowboy_loop.template', '/.config/rebar3/templates/cowboy_loop.template'],
            ['rebar3/templates/cowboy_rest.erl', '/.config/rebar3/templates/cowboy_rest.erl'],
            ['rebar3/templates/cowboy_rest.template', '/.config/rebar3/templates/cowboy_rest.template'],
            ['rebar3/templates/cowboy_ws.erl', '/.config/rebar3/templates/cowboy_ws.erl'],
            ['rebar3/templates/cowboy_ws.template', '/.config/rebar3/templates/cowboy_ws.template']
    ]
    dotfiles = [Dotfile(os.path.normpath(curdir + l), os.path.normpath(home + t)) for (l, t) in local_to_target_map]

    # TODO: colour output, red warnings, yellow 'made a symlink', green
    # 'symlink exists and points to this directory' etc.
    for df in dotfiles:
        if os.path.lexists(df.targetaddr):
            if os.path.islink(df.targetaddr):
                if not os.path.exists(df.targetaddr):
                    print('WARNING:')
                    print('%s exists but appears to be a broken symlink.' % df.targetaddr)
                else:
                    if os.path.realpath(df.targetaddr) == os.path.realpath(df.localaddr):
                        print('%s exists and points to this directory' % df.targetaddr)
                    else:
                        print('WARNING:')
                        print('%s exists and does not point to this directory.' % df.targetaddr)
            else:
                print('WARNING:')
                print('%s exists and does not point to this directory.' % df.targetaddr)
        elif os.path.isdir(df.targetaddr):
            print('%s is a directory.' % df.targetaddr)
        else:
            d = os.path.dirname(df.targetaddr)
            os.makedirs(d, exist_ok=True)
            os.symlink(df.localaddr, df.targetaddr)
            print('Made a symlink from %s to %s' % (df.targetaddr, df.localaddr))

    # Need to finish writing this. The gist is that ~/.config/nvim (directory)
    # gets linked to ~/.vim, and then ~/.config/nvim/init.vim is linked to
    # ~/.vimrc. It makes sense to link ~/.vim/init.vim instead, however, as
    # then the order of these operations doesn't matter.
    nvim_symlinks = [
            ['/.config/nvim', '/.vim'],
            ['/.vim/init.vim', '/.vimrc']
    ]
