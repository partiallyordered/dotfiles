
### Fonts of interest
Phosphor icons: https://phosphoricons.com/

### Exploring glyphs

Find material design icons and codepoints (perhaps update the version to match https://www.npmjs.com/package/@mdi/font):
  https://pictogrammers.github.io/@mdi/font/6.6.96/

There are also:
- gucharmap - https://github.com/polybar/polybar/wiki/Fonts#gnome-character-map
- nix-shell -p fontforge-gtk --command "fontforge $(fc-list | sk | cut -d: -f1)"
- https://www.nerdfonts.com/cheat-sheet
- https://fontdrop.info/#/?darkmode=true
    weak search functionality, find a font to load with fc-list | grep -i <font-name>
- https://beautifulwebtype.com/fira-code/glyphs/?i=5
- https://mathew-kurian.github.io/CharacterMap/
- https://fontawesome.com/v5/cheatsheet
- https://github.com/Keyamoon/IcoMoon-Free
- https://feathericons.com/ | https://github.com/AT-UI/feather-font
- https://github.com/lukas-w/font-logos

### Vim

Insert unicode codepoints in vim in insert mode using C-V then
- for a codepoint smaller than u00001 press u (lower case u) then enter the codepoint padded with leading zeroes to 4 chars in length
- for a codepoint greater than uffff press U (upper case u) then enter the codepoint padded with leading zeroes to 8 chars in length
