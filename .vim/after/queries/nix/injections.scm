; (
; find the node query, i.e. "string_fragment" using :TSPlaygroundToggle
; (string_fragment) @lua
; (#match? @lua "-- lua")
; [ (string) (indented_string) ] @bash

; https://github.com/oxalica/tree-sitter-nix/blob/master/queries/nvim-injections.scm#L112
; (
;   ((comment) @_language (#any-of? @_language "# lua" "/* lua */") (#set! "language" "lua")) .
;   [
;     ((indented_string) @content)
;     (bind
;       expression: [
;         (indented_string) @content
;         (binary (indented_string) @content)
;         (app argument: (indented_string) @content)])]
;   (#offset! @content 0 2 0 -2))

; ((string_fragment) @injection.content
;  (#set! injection.language "lua"))

; https://github.com/cstrahan/tree-sitter-nix/pull/31/files
; mark arbitary languages with a comment
((((comment) @injection.language) .
  (indented_string_expression (string_fragment) @injection.content))
  (#set! injection.combined))

((binding
   attrpath: (attrpath (identifier) @_path)
   expression: (indented_string_expression
     (string_fragment) @injection.content))
 (#match? @_path "(^\\w*Phase|(pre|post)\\w*|(.*\\.)?\\w*([sS]cript|[hH]ook)|(.*\\.)?startup)$")
 (#set! injection.language "bash")
 (#set! injection.combined))

((apply_expression
   function: (apply_expression function: (_) @_func)
   argument: (indented_string_expression (string_fragment) @injection.content))
 (#match? @_func "(^|\\.)writeShellScript(Bin)?$")
 (#set! injection.language "bash")
 (#set! injection.combined))

(apply_expression
  (apply_expression
    function: (apply_expression
      function: ((_) @_func)))
    argument: (indented_string_expression (string_fragment) @injection.content)
  (#match? @_func "(^|\\.)runCommand(((No)?(CC))?(Local)?)?$")
  (#set! injection.language "bash")
  (#set! injection.combined))

(apply_expression
  function: ((_) @_func)
  argument: (_ (_)* (_ (_)* (binding
    attrpath: (attrpath (identifier) @_path)
     expression: (indented_string_expression
       (string_fragment) @injection.content))))
  (#match? @_func "(^|\\.)writeShellApplication$")
  (#match? @_path "^text$")
  (#set! injection.language "bash")
  (#set! injection.combined))
