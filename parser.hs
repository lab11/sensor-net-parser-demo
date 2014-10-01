{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

{- to install the required package, and compile this app : 
 - install the haskell platform : https://www.haskell.org/platform/
 -      on ubuntu the following might work? 
 -      > apt-get install haskell-platform
 - get peggy : 
 -      > cabal install peggy 
 - get pretty-show : 
 -      > cabal install pretty-show
 - compile this app : 
 -      > ghc parser.hs
 -}

import Text.Peggy
import Text.Show.Pretty
import Data.Maybe

data Id = Dat String deriving (Show)    -- Identifiers used in the program

data Program = Program {                -- First Class Assignments and Rules
        assignments :: [Assignment],
        rules :: [Rule]  	                   
    } deriving (Show)

data Rule = Rule {                   -- A set of actions to perform when triggered
        trigger :: Event,
        actions :: [Action]
    } deriving (Show)

data Event = Every Interval                 -- Event that procs every <interval> 
	   | After Event Interval 	    -- Event that procs <interval> 
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
            | If Expression [Action]
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
              | BAdd | BSub | BDiv | BMul
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

flot ::: Float = [0-9]+'.'[0-9]+   {read ($1 ++ "." ++ $2)}

bol ::: Bool = 'True' {True}
             / 'False' {False}


-- ################# Program data and complex expressions ###############

prog :: Program = assignmentc* space* rule+  { Program $1 $3 }

rule :: Rule = 'ON' space* event space* '{' space* actionc* space* '}' space* 
                                    { Rule $2 $5 } 

event :: Event = 'EVERY' interval { Every $1 } 
               / interval space* 'AFTER' space* event { After $4 $1 }
               / expression space* 'BEGINS' space* 'WHILE' space* 'CHECKING' space* 'EVERY' space* interval 
                                    { Begins $1 $7}
               / expression space* 'ENDS' space* 'WHILE' space* 'CHECKING' space* 'EVERY' space* interval 
                                 { Begins (Unop UNot $1) $7 }
               / 'PERFORM' space* event space* 'WITH' space* 'COOLDOWN' space* interval 
                                 { Cooldown $2 $6 } 
               / 'EVERY' space* interval space*  'WITHIN' space* interval space* 'AFTER' space* event
                                 { Within $8 $2 $5 }
               / '(' space* event space* ')'   { $2 }

actionc :: Action = action space* ';' space* { $1 } 

action :: Action = 'GATHER' space* '{' space* records space* '}' space* 'INTO' space* table 
                    { Gather $3 $7 } 
                 / 'SEND' strn                          { Message $1 }
                 / 'EXECUTE' call                           { Execute $1 }
                 / space* 'IF' space* expression space* '{' space* actionc* space* '}' { If $3 $6 } 
                 / assignment                                   { Assign $1 }
                 / ident                                    { AIdent $1 }

records :: [Record] = record space* ',' space* records { $1 : $4 }
                    / record { [$1] } 

record :: Record = 'SAVE' expression 'AS' field { Record $1 $2 }

expression :: Expression = interval            { Inte $1 }
                         / flot                { Flt $1}
                         / intgr               { Intg $1 } 
                         / call                { Call $1 }
                         / strn                { Str $1 } 
                         / bol                 { Bl $1 }
                         / unop space* expression     { Unop $1 $3 } 
                         / '(' space*  expression space* binop space* expression space* ')'
                                               { Binop $4 $2 $6 } 
                         / ident               { EIdent $1 }
                         / '(' expression ')'  { $1 } 

unop :: UnaryOp = '!'          { UNot }
                / 'NOT'        { UNot }

binop :: BinaryOp = '&&'  { BAnd }
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
                  / '+'  { BAdd }
                  / '*'  { BMul }
                  / '/'  { BDiv }
                  / '-'  { BSub }


interval :: Interval = timesec+   { Time $! foldl (+) 0 $1  }

timesec :: Int = intgr 'min''s'?  { 60 * $1 } 
               / intgr 'sec''s'?  { $1 }
               / intgr 'hrs'      { 60 * 60 * $1 }
               / intgr 'hour''s'?     { 60 * 60 * $1 }
               / intgr 'day''s'?  { 60 * 60 * 24 * $1 } 

assignmentc :: Assignment = assignment space* ';' space* { $1 }

assignment :: Assignment = ident space* ':=' space* expression  { Assignment $1 (Easgn $4)}
                         / ident space* ':=' space* action      { Assignment $1 (Aasgn $4)}
                          

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
 - there's no actual tokenization step so we've got 'space*' tokens *everywhere*
 -
 - Actions *all* require ';' behind them, even IF based blocks. 
 -
 - We shoudl be able to infer types for all Call statements and execution blocks
 - this doesn't do that. 
 -
 - Infix Binops shouldn't need parens, they are manditory at the moment, and 
 - therefor IF , ON, and other blocks that can take events or expressions don't 
 - manditorily need parens as a hack to prevent duplication of parens. 
 -
 - Seriously, this parser needs a seperate tokenization and interpretation step. 
 -}

main :: IO()
main = putStr . ppShow . parseString prog "<stdin>" =<< getContents
-- main = putStr . ppShow . parseString rule "<stdin>" =<< getContents

