#!/usr/bin/env bash
langdir=Language/Scheme/R5RS
parsedir=$langdir/Parser
syntaxdir=$langdir/Syntax
transformerdir=$langdir/Transformer
alex -g $parsedir/Lexer.x # -o Lexer.hs
happy -g -a -c $parsedir/Parser.y # -o Parser.hs
ghci Parser.hs $syntaxdir/Expression.hs Lexer.hs $parsedir/Token.hs $parsedir/ParserUtils.hs $syntaxdir/PrettyPrint.hs
