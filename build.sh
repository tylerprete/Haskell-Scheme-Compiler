#!/usr/bin/env bash
langdir=src/Language/Scheme/R5RS
parsedir=$langdir/Parser
syntaxdir=$langdir/Syntax
transformerdir=$langdir/Transformer
alex -g $parsedir/Lexer.x # -o Lexer.hs
happy -g -a -c $parsedir/Parser.y # -o Parser.hs
ghc -isrc --make src/Main -o tylerscheme
