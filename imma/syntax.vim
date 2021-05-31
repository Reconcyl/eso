set ft=

syntax keyword ppx_kwd hlt nop get lit not add mul max dmp sav chr num chi
syntax match   ppx_sym "[$?]"
syntax match   ppx_lbl "\v\h\w*:?"
syntax match   ppx_num "\v\d+"
syntax match   ppx_str '\v"[^"]+"'
syntax region  ppx_cmt start=";" end="$"

highlight default link ppx_kwd Keyword
highlight default link ppx_sym Macro
highlight default link ppx_lbl Identifier
highlight default link ppx_num Number
highlight default link ppx_str String
highlight default link ppx_cmt Comment
