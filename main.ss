(load "intcps.ss")

;;<program> ::= <expression>
;;<form> ::= <definition>
;;          |<expression>
;;<definition> ::= <variable definition>
;;                |(begin <definition>*)
;;<variable definition> ::= (define <variable> <expression>)
;;<expression ::= <literal>
;;               |<variable>
;;               |(quote <datum>)
;;               |(begin <expression>*)
;;               |(if <expression> <expression> <expression>?)
;;               |(cond (<expression> <expression>)+)
;;               |(and <expression>*)
;;               |(or <expression>*)
;;               |(let ((<variable> <expression>)*) <expression> <expression>*)
;;               |(let* ((<variable> <expression>)*) <expression> <expression>*)
;;               |(letrec ((<variable> <expression>)*) <expression> <expression>*)
;;               |(case <expression> (<expression> <expression>*)*)
;;               |(while <expression> <expression> <expression>*)
;;               |(lambda <formals> <expression> <expression>*)
;;               |<application>
;;<application>  ::= (<procedure> <expression>*)
;;<procedure> ::= <primitive>
;;            ::= <closure>
;;<primitive> ::= built in procedure such as +, =, and null?
;;<closure> ::= user defined function made from a lambda expression
;;<literal> ::= <boolean>|<number>|<string>|<quoted list>|<vector>
;;<formals> ::= <variable>
;;             |(<variable>*)
;;             |(<variable> <variable>* . <variable>)
;;<variable> ::= any scheme identifier
