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

module Language.Scheme.R5RS.Parser.Parser (parse)
where
import qualified Language.Scheme.R5RS.Parser.Token as Token
import Language.Scheme.R5RS.Parser.Lexer (alexScanTokens)
import Language.Scheme.R5RS.Data.Expression
import Data.Maybe

-- Need to import the following if running parser in debug mode
import System.IO
import System.IO.Unsafe
}

%name parse Exp

%tokentype { Token.Token }
%error { parseError }

%token	let		{ Token.Identifier "let" 	}
	lambda		{ Token.Identifier "lambda" 	}
	if		{ Token.Identifier "if"		}
	and		{ Token.Identifier "and"	}
	quote		{ Token.Identifier "quote"	}
	setbang		{ Token.Identifier "set!"	}
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
	| '(' lambda Formals Exp ')'	{ Lambda $3 $4	}
	| Quotation			{ $1		}
	| SelfEvaluating		{ $1		}
	| '(' setbang identifier Exp ')'	{ SetBang (Var $3) $4 }

Exps	:: { [Exp] }
Exps	: ExpsRev	{ reverse $1 }

ExpsRev	:: { [Exp] }
ExpsRev	: {- empty -} { [] }
    	| ExpsRev Exp { $2 : $1 }

Formals	:: { Formals }
Formals	: identifier		{ SingleVar (Var $1) 	}
	| '(' FormalList	{ $2			}	

FormalList	:: { Formals }
FormalList	: ')'				{ VarList [] }
       		| Formals1 FormalEndList	{ makeFormalList (reverse $1) $2 }

FormalEndList :: { Maybe Var }
FormalEndList	: ')'			{ Nothing }
		| '.' identifier ')'	{ Just (Var $2) }

Formals1 :: { [Var] }
Formals1	: identifier 		{ [(Var $1)] 	}
		| Formals1 identifier	{ (Var $2) : $1 }

SelfEvaluating :: { Exp }
SelfEvaluating	: bool		{ Boolean $1 	}
		| number	{ Number $1	}
		| char		{ Character $1	}
		| string	{ String $1	}

Quotation :: { Exp }
Quotation	: '\'' Datum		{ $2 }
	  	| '(' quote Datum ')'	{ $3 }
		| Vector		{ $1 }
--		| Abbreviation		{ $1 }

Datum :: { Exp }
Datum : SimpleDatum 	{ $1 }
      | CompoundDatum	{ $1 }

SimpleDatum	:: { Exp }
SimpleDatum	: bool 		{ Boolean $1 	}
	   	| number	{ Number $1 	}
		| char		{ Character $1 	}
		| string	{ String $1 	}
		| identifier	{ Symbol $1 	}
{- Wow, really lame.  No way to grab all identifiers,
so have to match if, let, etc by hand -}
		| if		{ Symbol $ extractIdent $1 }
		| let		{ Symbol $ extractIdent $1 }
		| and		{ Symbol $ extractIdent $1 }
		| quote		{ Symbol $ extractIdent $1 }
		| lambda	{ Symbol $ extractIdent $1 }

CompoundDatum	:: { Exp }
CompoundDatum	: List		{ $1 }
	      	| Vector	{ $1 }

List	:: { Exp }
List	: '(' InList		{ $2 }
	| Abbreviation		{ List [$1] }

InList	:: { Exp }
InList	: ')'			{ List [] }
       	| Datums1 EndList	{ makeList (reverse $1) $2 }

EndList :: { Maybe Exp }
EndList	: ')'		{ Nothing }
	| '.' Datum ')'	{ Just $2 }

Datums1 :: { [Exp] }
Datums1 : Datum 	{ [$1] }
	| Datums1 Datum	{ $2 : $1 }

-- Ignoring prefix is no doubt a bad move, but works for now
Abbreviation :: { Exp }
Abbreviation	: AbbrevPrefix Datum	{ $2 }

AbbrevPrefix :: { () }
AbbrevPrefix	: '\''	{ () }
	     	| '`'	{ () }
		| ','	{ () }
		| ',@'	{ () }

Vector :: { Exp }
Vector : '#(' InList { makeVector $2 }

{
extractIdent (Token.Identifier s) = s
extractIdent _ = error "bad use of extractIdent"

parseError :: [Token.Token] -> a
parseError _ = error $ "Parse error"
}
