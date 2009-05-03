{
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Parser.Parser
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Implementation of the Scheme R5RS Parser
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Parser.Parser ()
where
import qualified Language.Scheme.R5RS.Parser.Token as Token
import Language.Scheme.R5RS.Parser.Lexer (alexScanTokens)
}

%name parse Exp
--%name parseTest Datums1

%tokentype { Token.Token }
%error { parseError }

%token	let		{ Token.Identifier "let" 	}
	lambda		{ Token.Identifier "lambda" 	}
	if		{ Token.Identifier "if"		}
	and		{ Token.Identifier "and"	}
	quote		{ Token.Identifier "quote"	}
	'('		{ Token.LeftParen		}
	')'		{ Token.RightParen		}
	'#('		{ Token.PoundLeftParen		}
	'\''		{ Token.Tick			}
	'`'		{ Token.BackTick		}
	','		{ Token.Comma			}
	',@'		{ Token.CommaAt			}
	'.'		{ Token.Dot			}
	bool		{ Token.Boolean $$		}
	number		{ Token.Number $$		}
	char		{ Token.Character $$		}
	string		{ Token.String $$		}
	identifier	{ Token.Identifier $$		}

%%

Exp	:: { Exp }
Exp	: identifier 			{ Ref $ Var $1 	} 
    	| '(' Exp Exps ')'		{ App $2 $3 	}
    	| '(' if Exp Exp Exp ')'	{ If $3 $4 $5 	}
	| Quotation			{ $1		}
	| SelfEvaluating		{ $1		}

Exps	:: { [Exp] }
Exps	: ExpsRev	{ reverse $1 }

ExpsRev	:: { [Exp] }
ExpsRev	: {- empty -} { [] }
    	| ExpsRev Exp { $2 : $1 }

SelfEvaluating	: bool		{ Boolean $1 	}
		| number	{ Number $1	}
		| char		{ Character $1	}
		| string	{ String $1	}

Quotation	: '\'' Datum		{ Literal $2 }
	  	| '(' quote Datum ')'	{ Literal $3 }
		| Vector		{ Literal $ CompoundDatum $1 }
		| Abbreviation		{ Literal $ CompoundDatum $ List [$1] }

Datum : SimpleDatum 	{ SimpleDatum $1 }
      | CompoundDatum	{ CompoundDatum $1 }

SimpleDatum	:: { Exp }
SimpleDatum	: bool 		{ Boolean $1 	}
	   	| number	{ Number $1 	}
		| char		{ Character $1 	}
		| string	{ String $1 	}
		| identifier	{ Symbol $1 	}

CompoundDatum	:: { DatumList }
CompoundDatum	: List		{ $1 }
	      	| Vector	{ $1 }

List	:: { DatumList }
List	: '(' InList		{ $2 }
	| Abbreviation		{ List [$1] }

InList	:: { DatumList }
InList	: ')'			{ List [] }
       	| Datums1 EndList	{ makeList (reverse $1) $2 }

EndList	: ')'		{ [] 	}
	| '.' Datum ')'	{ [$2] 	}

{-
Datums :: { [Datum] }
Datums	: {- empty -} { [] }
       	| Datums Datum { $2 : $1 }
-}
Datums1 :: { [Datum] }
Datums1 : Datum 	{ [$1] }
	| Datums1 Datum	{ $2 : $1 }

Abbreviation	: AbbrevPrefix Datum	{ $2 } -- interesting...

AbbrevPrefix	--: '\''	{ () }
	     	: '`'	{ () }
		| ','	{ () }
		| ',@'	{ () }

Vector : '#(' InList { makeVector $2 }

{
data Var = Var String
	deriving Show

data Formals	= SingleVar Var
		| ManyVar [Var]
		| DottedVars [Var] Var
	deriving Show

data Exp	= Ref Var
		| Boolean Bool
		| Number Token.Number
		| Character Char
		| String String
		| Symbol String
		-- | literal stuff
		| Literal Datum -- Temporary
		| App Exp [Exp]
		| Lam [Formals]
		| If Exp Exp Exp
		| SetBang Var Exp
	deriving Show

data DatumList = List [Datum] | DottedList [Datum] Datum | Vector [Datum]
	deriving Show

makeList xs [d]	= DottedList xs d
makeList xs []	= List xs
makeList _  _	= error "bad List"

makeVector (List xs) = Vector xs
makeVector _ = error "Bad input to makeVector"

data Datum = SimpleDatum Exp | CompoundDatum DatumList
	deriving Show
parseError :: [Token.Token] -> a
parseError _ = error $ "Parse error"
}
