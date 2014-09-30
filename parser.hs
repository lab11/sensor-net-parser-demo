
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

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
            deriving (Show)

data Assignment = Assignment {
        varname :: Id,
        value :: Expression 
    } deriving (Show)


data Expression = Str String
                | Intg Int
                | Flt Float
                | Bl  Bool 
                | Inte Interval 
                | Call String
                | Unop UnaryOp Expression
                | Binop BinaryOp Expression Expression
                deriving (Show)

data UnaryOp = Not deriving (Show)

data BinaryOp = And | Or | Equals | Grt | Lst | Gte | Lte | Neq deriving (Show) 

data Interval = Time Int deriving (Show)    -- An interval in seconds 

data Field = Fld String deriving(Show)      -- The name of a column in a table. 

data Table = Tbl String deriving (Show)     -- The name of a table to add data to

data Record = Record {
        record :: Expression, 
        field :: Field
    } deriving (Show)


[peggy|



|]


main :: IO()
main = interact id
