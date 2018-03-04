{-
module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRectangle
) where
-}

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

--value constructors are function!
circles = map (Circle (Point 10 20)) [4,5,6,7] --lista di cerchi concentrici in (10, 20)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point  0 0) (Point width height)

--Record syntax = syntactic sugar for defining data types
--Automatically constructs all accessor functions
--If derives Show the data is displayed as a struct and not as a vector for readability
data Person = Person { 
  firstName :: String
, lastName ::String
, age ::Int
, height :: Float
, phoneNumber :: String
, flavor :: String
} deriving (Eq, Show, Read) --automatically define show, == and read functions

--NOTE: read by default uses the same notation of show.
pinco = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43, height = 1.78, phoneNumber = \"555000000\", flavor = \"mint\"}" :: Person --type annotation necessary

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

--type = typedef in C
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = elem (name, pnumber) pbook

type AssocList k v = [(k, v)] --parameterized type synonym

data Mylist a = Empty | Cons a (Mylist a) deriving (Show, Read, Eq, Ord) --Cons takes the role of : operator for lists
--remember -> [1,2,3] is the same as 1:(2:(3:[]))

infixr 5 :-: --5 is fixity (precedence order)
data Mylist' a = Empty' | a :-: (Mylist' a) deriving (Show, Read, Eq, Ord)

infixr 5 ++
(.++) :: Mylist' a -> Mylist' a -> Mylist' a
Empty' .++ ys = ys
(x :-: xs) ++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a --for a binary search tree
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) --this a is a value and is different from the type parameter of the signature
    | x == a = Node x left right --no need to handle the duplicates, it is a binary search tree
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

data Maybe' a = Nothing' | Just' a

instance (Eq m) => Eq (Maybe' m) where --use of class constraint for subclassing
    Just' x == Just' y = x == y
    Nothing' == Nothing' = True
    _ == _ = False

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True
    
instance YesNo Bool where
    yesno = id --identity function

instance YesNo (Maybe' a) where
    yesno (Just' _) = True
    yesno Nothing' = False
    
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
    
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True
    
yesnoIf :: (YesNo y) => y -> a -> a -> a --mimics if functionality with YesNo instances
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult  

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

--kinds

class Tofu t where
    tofu :: j a -> t a j
    
data Frank a b = Frank {frankFiled :: b a} deriving (Show)

instance Tofu Frank where  
    tofu x = Frank x   

--p of kind *, a concrete type. k is also a concrete type * so t must be of kind (* -> *)
data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where --remains p to be mapped, the first field of Barry
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
