#!/usr/bin/env bash
langdir=src/Language/Scheme/R5RS
parsedir=$langdir/Parser
syntaxdir=$langdir/Syntax
transformerdir=$langdir/Transformer
alex -g $parsedir/Lexer.x -o $parsedir/Lexer.hs
happy -g -a -c $parsedir/Parser.y -o $parsedir/Parser.hs
ghci $parsedir/Parser.hs $syntaxdir/Expression.hs $parsedir/Lexer.hs $parsedir/Token.hs $parsedir/ParserUtils.hs $syntaxdir/PrettyPrint.hs
