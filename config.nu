alias v = nvim
alias lg = lazygit
let-env config = {
  show_banner: false
  edit_mode: vi
  keybindings: [
    {
      name: open_in_editor
      modifier: control
      keycode: char_i
      mode: [emacs, vi_normal, vi_insert]
      event: { send: OpenEditor }
    }
    {
      name: history_hint_complete
      modifier: control
      keycode: char_l
      mode: [vi_insert]
      event: { send: HistoryHintComplete }
    }
  ]
}
