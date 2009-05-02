{
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Parser.Lexer
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Scheme R5RS lexer.
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Parser.Parser ()
where
import Language.Scheme.R5RS.Parser.Token
import Language.Scheme.R5RS.Parser.Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token	let		{ Identifier "let" 	}
	lambda		{ Identifier "lambda" 	}
	if		{ Identifier "if"	}
	and		{ Identifier "and"	}
	'('		{ LeftParen		}
	')'		{ RightParen		}
	'#('		{ PoundLeftParen	}
	'\''		{ Tick			}
	'`'		{ BackTick		}
	','		{ Comma			}
	',@'		{ CommaAt		}
	'.'		{ Dot			}
	bool		{ Boolean _		}
	number		{ Number _		}
	char		{ Character _		}
	string		{ String _		}
	variable	{ Identifier $$		}
--	symbol		{ Identifier _		}		

%%

Exp	:: { Exp }
Exp	: variable 			{ Var $1 	} 
    	| '(' Exp Exps ')'		{ App $2 $3 	}
    	| '(' if Exp Exp Exp ')'	{ If $3 $4 $5 	}
	| Datum				{ $1		}

Exps	:: { [Exp] }
Exps	: ExpsRev	{ reverse $1 }

ExpsRev	:: { [Exp] }
ExpsRev	: {- empty -} { [] }
    	| ExpsRev Exp { $2 : $1 }

Datum :	SimpleDatum 	{ Literal SimpleDatum }
--      | CompoundDatum	{ Literal CompoundDatum }

SimpleDatum	:: { Token }
SimpleDatum	: bool 		{ $1 }
	   	| number	{ $1 }
		| char		{ $1 }
		| string	{ $1 }
--		| symbol	{ $1 }

{-
CompoundDatum	: List		{ $1 }
	      	| Vector	{ $1 }

List	: '(' Datums ')'		{ $1 }
     	| '(' Datums1 '.' Datum	')'	{ $2 : $4 }
	| Abbreviation			{ $1 }

Datums	: {- empty -} { [] }
       	| Datums Datum { $2 : $1 }

Datums1 : Datum 	{ $1 }
	| Datums1 Datum	{ $2 : $1 }

Abbreviation	: AbbrevPrefix Datum	{ $2 } -- interesting...

AbbrevPrefix	: '\''	{ $1 }
	     	| '`'	{ $1 }
		| ','	{ $1 }
		| ',@'	{ $1 }

Vector : {- empty -} { () }
-}

{
type Ref = String
data Formals	= SingleRef Ref
		| ManyRef [Ref]
		| RefsDotRef [Ref] Ref
	deriving Show

data Exp	= Var Ref
		-- | literal stuff
		| Literal Datum -- Temporary
		| App Exp [Exp]
		| Lam [Formals]
		| If Exp Exp Exp
		| SetBang Ref Exp
	deriving Show

data Datum = SimpleDatum | CompoundDatum
	deriving Show
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
