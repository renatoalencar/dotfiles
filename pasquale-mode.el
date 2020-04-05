(defconst pasquale-keywords
  (regexp-opt '("algoritmo" "fimalgoritmo" "inicio" "início"
                "para" "fimpara" "ate" "até" "de" "enquanto" "fimenquanto"
                "funcao" "função" "fimfuncao" "fimfunção" "procedimento" "fimprocedimento"
                "se" "senao" "senão" "fimse" "dos" "var" "vetor" "faca" "faça"
                "aleatorio" "on" "off" "repita" "timer" "pausa"
                "debug" "eco" "cronometro" "mod" "e" "é" "ou" "nao" "não" "xou"
                "const" "tipo" "registro" "fimregistro" "arquivo"
                "escolha" "fimescolha" "caso" "outrocaso" "interrompa"
                "retorne" "entao" "então") 'words))

(defconst pasquale-builtin
  (regexp-opt '("leia" "escreva" "escreval") 'words))

(defconst pasquale-types
  "real\\|inteiro\\|logico\\|caractere?")

(defconst pasquale-comment "//.*$")

(defconst pasquale-variable-name
  "\\([[:alpha:]][[:alnum:]_]*\\)[ \t]*:[ \t]*[[:alpha:]][[:alnum:]_]*.*$")

(defconst pasquale-function-name
  "\\([[:alpha:]][[:alnum:]_]*\\)[ \t]*([^)]*)")

(defconst pasquale-constants
  (regexp-opt '("verdadeiro" "falso") 'words))

(defconst pasquale-highlights
  `((,pasquale-comment . font-lock-comment-face)
    (,pasquale-keywords . font-lock-keyword-face)
    (,pasquale-types . font-lock-type-face)
    (,pasquale-builtin . font-lock-builtin-face)
    (,pasquale-variable-name . (1 font-lock-variable-name-face))
    (,pasquale-function-name . (1 font-lock-function-name-face))
    (,pasquale-constants . font-lock-constant-face)))

(define-derived-mode pasquale-mode prog-mode "Pasquale"
  "Pasquale mode"
  (setq-local case-fold-search t)
  (setq-local font-lock-defaults '(pasquale-highlights nil t)))

(add-to-list 'auto-mode-alist '("\\.alg\\'" . pasquale-mode))

(provide 'pasquale-mode)
