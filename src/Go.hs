-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Executable.Go
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Test module
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Executable.Go (main)
where
import Language.Scheme.R5RS.Parser.Lexer (alexScanTokens)
import Language.Scheme.R5RS.Parser.Parser (parse)
import Language.Scheme.R5RS.Syntax.Expression
import Language.Scheme.R5RS.Syntax.PrettyPrint
import Language.Scheme.R5RS.Transformer.CPSTransformer

import Data.Supply

main = do
  	input <- getContents
	tokens <- return $ alexScanTokens input
	ast <- return $ parse tokens
	print ast
	putStrLn (pp ast)
	s <- newNumSupply
	halt <- return $ Ref (Var "halt")
	e <- return $ smarter_t s ast halt
	putStrLn "CPS Converted"
	print e
	putStrLn (pp e)
	

