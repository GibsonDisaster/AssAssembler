module Types where
  import Control.Monad.Identity
  import Text.Parsec (Parsec, ParsecT, modifyState)
  import qualified Data.Sequence as Seq
  import qualified Data.Map as M

  type Parse a =  ParsecT String ParserState Identity a

  data ParserState = ParserState String deriving (Show)

  data Computer = Computer {
    registers :: [Int],
    testRegisters :: [Int],
    pc :: Int,
    prog :: M.Map Int AssStruct,
    stack :: Seq.Seq Int,
    labels :: M.Map String Int
  }

  data AssStruct = GlobalStart String
                   | DataSection [AssStruct]
                   | Mov AssStruct AssStruct
                   | Add AssStruct AssStruct AssStruct
                   | Sub AssStruct AssStruct AssStruct
                   | Mul AssStruct AssStruct AssStruct
                   | Div AssStruct AssStruct AssStruct
                   | Syscall Int
                   | Cmp AssStruct AssStruct
                   | Je AssStruct
                   | Jne AssStruct
                   | Jle AssStruct
                   | Jl AssStruct
                   | Jg AssStruct
                   | Jge AssStruct
                   | Jz AssStruct
                   | Jnz AssStruct
                   | Label String Int
                   | Reg Int
                   | Str String
                   | LitInt Int
                   | LitStr String
                   | Null
                   deriving (Show, Eq, Ord)

  initParserState :: ParserState
  initParserState = ParserState "text"