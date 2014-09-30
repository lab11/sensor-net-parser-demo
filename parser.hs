{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

{- To install the required package, and compile this app : 
 - Install the Haskell Platform : https://www.haskell.org/platform/
 - Get Peggy : $ cabal install peggy 
 - Compile this app : ghc parser.hs
 -}

import Text.Peggy

data Id = Dat String deriving (Show)    -- Identifiers used in the program

data Program = Program {                -- First Class Assignments and Rules
        assignments :: [Assignment],
        rules :: [Rule]  	                   
    } deriving (Show)

data Rule = Rule {                   -- A set of actions to perform when triggered
        trigger :: Event,
        actions :: [Action]
    } deriving (Show)

data Event = Every Interval		            -- Event that procs every <interval> 
	       | After Event Interval 	        -- Event that procs <interval> 
                                            --   after <event>
           | Begins Expression Interval     -- Event that happens when 
                                            --   <bool:Expression> goes from false to
                                            --   true when checking every 
                                            --   <interval> .
           | Cooldown Event Interval        -- Will trigger on <event> unless it
                                            --   has already been triggered 
                                            --   within <interval>. 
           | Within Event Interval Interval -- Will trigger every <interval2> 
                                            --   within <interval1> after <event>
           deriving (Show) 

data Action = Gather [Record] Table         -- Save a set of records into the 
                                            --  appropriate column of a given table
            | Message String                -- Send a message to controllers
            | Execute String                -- Run a piece of code             
            | Assign Assignment             -- Assign a particular value to an ID
            | AIdent Id                          
            deriving (Show)

data Assignment = Assignment {
        varname :: Id,
        value :: Assignable 
    } deriving (Show)

data Assignable = Easgn Expression | Aasgn Action
                deriving (Show) 

data Expression = Str String
                | Intg Int
                | Flt Float
                | Bl  Bool 
                | Inte Interval
                | EIdent Id 
                | Call String
                | Unop UnaryOp Expression
                | Binop BinaryOp Expression Expression
                deriving (Show)

data UnaryOp = UNot 
             deriving (Show)

data BinaryOp = BAnd | BOr | BEquals | BGrt | BLst | BGte | BLte | BNeq 
              deriving (Show) 

data Interval = Time Int 
              deriving (Show)    -- An interval in seconds 

data Field = Fld String 
           deriving(Show)      -- The name of a column in a table. 

data Table = Tbl String 
           deriving (Show)     -- The name of a table to add data to

data Record = Record {
        recexpr :: Expression, 
        recfield :: Field
    } deriving (Show)


[peggy|

-- ############# Tokens and Simple Elements #################

ident ::: Id = [a-z][0-9a-zA-Z_]* { Dat ($1 : $2)} 

table ::: Table = [A-Z][0-9a-zA-Z_]* { Tbl ($1 : $2)} 

field ::: Field = [a-zA-Z][0-9a-zA-Z_]* { Fld ($1 : $2)} 

{- TODO -- System Calls need to be full expressions that are typed properly for
 -         both input and output, this will have to do for the moment -} 
call ::: String = [a-z][a-z_]*'('[^)]*')' { $1 : $2 ++ "(" ++ $3 ++ ")" }

strn ::: String = '\"' [^""]* '\"' { $1 } 

intgr ::: Int = [0-9]+ {read $1}

-- ################# Program data and complex expressions ###############

prog ::: Program = assignment* rule* { Program $1 $2 }

rule ::: Rule = 'ON' '(' event ')' '{' action* '}' { Rule $1 $2 } 

event ::: Event = 'EVERY' interval { Every $1 } 
                / interval 'AFTER' event { After $2 $1 }
                / expression 'BEGINS' 'WHILE' 'CHECKING' interval { Begins $1 $2}
                / expression 'ENDS' 'WHILE' 'CHECKING' interval 
                                  { Begins (Unop UNot $1) $2 }
                / event 'WITH' 'COOLDOWN' interval 
                                  { Cooldown $1 $2 } 
                / 'EVERY' interval 'WITHIN' interval 'AFTER' event
                                  { Within $3 $1 $2 }
                / '(' event ')'   { $1 }

action ::: Action = 'GATHER' '{' records '}' 'INTO' table ';'   { Gather $1 $2 } 
                  / 'SEND' strn ';'                           { Message $1 }
                  / 'EXECUTE' call ';'                          { Execute $1 }
                  / assignment                                  { Assign $1 }
                  / ident ';'                                   { AIdent $1 }

records ::: [Record] = record ',' records { $1 : $2 }
                     / record { [$1] } 

record ::: Record = 'SAVE' expression 'AS' field { Record $1 $2 }

expression ::: Expression = '(' expression ')'  { $1 } 
                          / interval            { Inte $1 }
                          / call                { Call $1 }
                          / strn                { Str $1 } 
                          / [0-9]+              { Intg (read $1 ) } 
                          / [0-9]+'.'[0-9]+     { Flt (read ($1 ++ $2))}
                          / 'True'              { Bl True }
                          / 'False'             { Bl False }
                          / unop expression     { Unop $1 $2 } 
                          / expression binop expression
                                                { Binop $2 $1 $3 } 
                          / ident               { EIdent $1 }

unop ::: UnaryOp = '!'          { UNot }
                 / 'NOT'        { UNot }


binop ::: BinaryOp = '&&'  { BAnd }
                   / 'AND' { BAnd }
                   / '||'  { BOr }
                   / 'OR'  { BOr }
                   / '=='  { BEquals }
                   / '>'   { BGrt }
                   / 'GREATER' 'THAN' { BGrt }
                   / '<'   { BLst }
                   / 'LESS' 'THAN' { BLst }
                   / '>='  { BGte }
                   / 'GREATER' 'THAN' 'OR' 'EQUAL' { BGte }
                   / '<='  { BLte }
                   / 'LESS' 'THAN' 'OR' 'EQUAL' { BLte }
                   / '!='  { BNeq }
                   / 'NOT' 'EQUAL' { BNeq }


interval ::: Interval = timesec+   { Time $ foldl (+) 0 $1  }

timesec ::: Int = intgr 'min''s'?  { 60 * $1 } 
                / intgr 'sec''s'?  { $1 }
                / intgr 'hrs'      { 60 * 60 * $1 }
                / intgr 'hour'     { 60 * 60 * $1 }
                / intgr 'day''s'?  { 60 * 60 * 24 * $1 } 

assignment ::: Assignment = ident ':=' expression ';' { Assignment $1 (Easgn $2)}
                          / ident ':=' action      { Assignment $1 (Aasgn $2)}
                          

|]

{- #### Notes ####
 - this is a very preliminary grammar, it ignores order of operations and a 
 - huge bevy of other things in favour of knocking together a basic grammer 
 - that gets the job done. 
 -
 - Among other things it has no proper type system, and external calls are 
 - not handled in any meaningful sense at all. 
 -
 - Float handling is not very good;
 -
 - Assignment syntax structure is weird and requires an ending ';' 
 -}

main :: IO()
main = print . parseString prog "<stdin>" =<< getContents
