import System.Environment
import System.IO

import Numeric
import Data.Ratio
import Data.Complex
import Data.Functor.Identity

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Text.ParserCombinators.Parsec

import Data.IORef

-- ***************************************************************************** Program Logic **************************************************************************

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
  
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
             
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++ (case varargs of {Nothing -> ""; Just arg -> " . " ++ arg}) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ show expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ show varname

instance Show LispVal where show = showVal
instance Show LispError where show = showError

type ThrowsError = Except LispError

type Env = IORef [(String, IORef LispVal)]

--String MUST be specified in the signature because show returns a String so
--using a type variable would imply other types.  
trapError :: (Monad m, Show a) => ExceptT a m String -> ExceptT e' m String
trapError action = catchE action (return . show) 
--if action was ok return it else transform error in string -> same result type

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwE $ Parser err
    Right val -> return val

-- ************************************************************************ Parser defintions ***************************************************************************

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\"\\nrt"
                  return $ case x of
                    '\\' -> x
                    '\"' -> x
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChars <|> noneOf "\""
                char '"'
                return $ String x

parseChar1 :: Parser Char
parseChar1 = do try $ string "#\\"
                do {c <- anyChar; notFollowedBy alphaNum; return $ c}
               
parseChar2 :: Parser Char
parseChar2 = do try $ string "#\\"
                c <- string "space" <|> string "newline"
                return $ case c of
                    "space" -> ' '
                    "newline" -> '\n'
                
parseCharacter :: Parser LispVal
parseCharacter = do c <- try parseChar2 <|> parseChar1
                    return $ Character c
                
parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    _    -> Atom atom
                    
parseDecimal1 :: Parser LispVal
parseDecimal1 = liftM (Number . read) $ many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   parseDecimal1
   
hex2dig x = fst . head . readHex $ x
oct2dig x = fst . head . readOct $ x
bin2dig x = bin2dig' x 0
bin2dig' "" acc = acc
bin2dig' (x:xs) acc = let old = 2 * acc + (if x == '0' then 0 else 1)
                      in bin2dig' xs old
                   
parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number . hex2dig $ x
              
parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number . oct2dig $ x
              
parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number . bin2dig $ x

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $  readFloat (x ++ "." ++ y))
         
parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2)
                  char '+'
                  y <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)
                    
parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

--exercise 1
parseNumberDo :: Parser LispVal
parseNumberDo = do
    str <- many1 digit
    number <- return . read $ str
    return . Number $ number
   
parseNumberBind :: Parser LispVal 
parseNumberBind = many1 digit >>= return . Number . read
-- End of exercise 1

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
    
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio 
        <|> try parseNumber
        <|> try parseBool
        <|> try parseQuoted
        <|> try parseCharacter
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

-- ************************************************************** Default Operators and Functions ***********************************************************************

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwE $ NumArgs 2 []
numericBinop op singleVal@[_] = throwE $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal --Scheme syntax: (string? "Hi"), is a list
unaryOp f [v] = return $ f v

isSymbol, isString, isNumber, isBool, isList :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False
isString (String _) = Bool True
isString _ = Bool False
isNumber (Number _) = Bool True
isNumber _ = Bool False
isBool (Bool _) = Bool True
isBool _ = Bool False
isList (List _) = Bool True
isList _ = Bool False

symbol2string :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""

string2symbol :: LispVal -> LispVal
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwE $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ head (tail args)
                                     return $ Bool $ op left right

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                              then throwE $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed 
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwE $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwE $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwE $ TypeMismatch "boolean" notBool 

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

stringLen :: [LispVal] -> ThrowsError LispVal
stringLen [(String s)] = return $ Number $ fromIntegral $ length s
stringLen [notString]  = throwE $ TypeMismatch "string" notString
stringLen badArgList   = throwE $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)]
    | length s < k' + 1 = throwE $ Default "Out of bound error"
    | otherwise         = return $ String $ [s !! k']
    where k' = fromIntegral k
stringRef [(String s), notNum] = throwE $ TypeMismatch "number" notNum
stringRef [notString, _]       = throwE $ TypeMismatch "string" notString
stringRef badArgList           = throwE $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwE $ TypeMismatch "pair" badArg
car badArgList            = throwE $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwE $ TypeMismatch "pair" badArg
cdr badArgList            = throwE $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwE $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case runExcept $ eqv [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val --break                                    
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwE $ NumArgs 2 badArgList

--Existential Types extensions, will be probably standardized in Haskell 2020
data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvListPair $ zip arg1 arg2)
    where eqvListPair (x, y) = case runExcept $ equal [x, y] of
                                    Left err -> False
                                    Right (Bool val) -> val
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ [x] ++ xs, List $ [y] ++ ys]
equal [arg1, arg2] = do --loose type comparisons. The magic resides in the unpackX functions and their pattern matching cases
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
    where
        unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
        unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do {unpacked1 <- unpacker arg1;
                                                            unpacked2 <- unpacker arg2;
                                                            return $ unpacked1 == unpacked2} `catchE` (const $ return False) 
equal badArgList = throwE $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp isSymbol),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("bool?", unaryOp isBool),
              ("list?", unaryOp isList),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-length", stringLen),
              ("string-ref", stringRef),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

-- ************************************************************************** Stateful Logic ****************************************************************************

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows action = pmatch (runExcept action) 
    where pmatch (Left err) = throwE err
          pmatch (Right val) = return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef --use another ref to store the environment
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwE $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody --bindVars here updates the environment and saves it to another IORef
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env
apply (IOFunc func) args = func args

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwE $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwE $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef --dereferencing envRef
        writeIORef envRef ((var, valueRef) : env) --update envRef
        return value
    where
        isBound :: Env -> String -> IO Bool
        isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

makeFunction varargs env params body = return $ Func (map showVal params) varargs body env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
           Bool False -> eval env alt
           otherwise  -> eval env conseq
eval env form@(List (Atom "cond" : clauses)) = 
    if null clauses 
        then throwE $ BadSpecialForm "no true clause in cond expression: " form
        else case head clauses of
            List [Atom "else", expr] -> eval env expr
            List [test, expr]        -> eval env $ List [Atom "if", test, expr, List (Atom "cond" : tail clauses)]
            _ -> throwE $ BadSpecialForm "ill-formed cond expression: " form
eval env form@(List (Atom "case" : key : clauses)) = 
    if null clauses
        then throwE $ BadSpecialForm "no true clause in case expression: " form
        else case head clauses of
            List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
            List ((List datums) : exprs) -> do
                mkey <- eval env key
                checks <- mapM (\x -> liftThrows . eqv $ [mkey, x]) datums
                equality <- return $ any (\(Bool x) -> x) checks
                if equality
                    then mapM (eval env) exprs >>= return .last
                    else eval env $ List (Atom "case" : key : tail clauses)
            _ -> throwE $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeFunction Nothing env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeFunction (Just . showVal $ varargs) env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeFunction Nothing env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeFunction (Just . showVal $ varargs) env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeFunction (Just . showVal $ varargs) env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do func <- eval env function
                                       argVals <- mapM (eval env) args
                                       apply func argVals
eval env badForm = throwE $ BadSpecialForm "Unrecognized special form" badForm

-- ************************************************************************ IO Functions ********************************************************************************

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args --sub list
applyProc (func : args)     = apply func args --one list

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . (readOrThrow parseExpr)
--hGetLine :: IO String and readExpr :: String -> ThrowsError LispVal. They must be lifted into the IOThrowsError monad

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . (readOrThrow (endBy parseExpr spaces))

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

-- **********************************************************************************************************************************************************************

primitiveBindings :: IO Env --return the last created reference (with all the primitives loaded)
primitiveBindings = let makeFunc constructor (val, func) = (val, constructor func)
                    in newIORef [] >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)

main :: IO ()
main = do
    args <- getArgs
    if null args 
    then 
        primitiveBindings >>= until_  (== "quit") (putStr ">>>" >> hFlush stdout >> getLine) . evalAndPrint
    else do
        env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
        (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr
    where
        until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
        until_ pred prompt action = do
            result <- prompt
            if pred result
            then return ()
            else action result >> until_ pred prompt action
            
        evalAndPrint :: Env -> String -> IO ()
        evalAndPrint env expr = (runIOThrows $ liftM show $ (liftThrows $ readOrThrow parseExpr expr) >>= eval env) >>= putStrLn
        
        runIOThrows :: IOThrowsError String -> IO String
        runIOThrows action = do
            either <- runExceptT . trapError $ action
            return . f $ either
            where f (Right val) = val
