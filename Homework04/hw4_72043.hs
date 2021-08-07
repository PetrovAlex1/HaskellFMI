main :: IO()
main = do

    print ("Task01")
    print (getFeaturedStars "MGM" 1995 db)
    print (getFeaturedStars "USA Entertainm." 2001 db)

    print (getPresident "Paramount" db)
    print (getPresident "Fox" db)
    print (getPresident "USA Entertainm." db)

    print (getHigherProductions "Calvin Coolidge" db)
    print (getHigherProductions "Stephen Spielberg" db)
    print (getHigherProductions "George Lucas" db)
    print ("Task02")
    print ((toBinaryIndexed t1) == t1Result) 
    print ((toBinaryIndexed t2) == t2Result)

type Name = String
type Title = String
type Address = String
type Year = Int
type Gender = Char
type Length = Int
type ProducerID = Int
type Networth = Integer

data Movie = Movie Title Year Length Name ProducerID deriving Show
data MovieStar = MovieStar Name Gender deriving Show
data StarsIn = StarsIn Name Title deriving Show
data Studio = Studio Name Int deriving Show
data MovieExec = MovieExec Name ProducerID Networth deriving Show

type MovieDB = ([Movie], [MovieStar], [StarsIn], [Studio], [MovieExec])

studios :: [Studio]
studios = [Studio "Disney" 199,
 Studio "USA Entertainm." 222,
 Studio "Fox" 333,
 Studio "Paramount" 123,
 Studio "MGM" 555]

movieExecs :: [MovieExec]
movieExecs = [MovieExec "George Lucas" 555 200000000,
 MovieExec "Ted Turner" 333 125000000,
 MovieExec "Stephen Spielberg" 222 100000000,
 MovieExec "Merv Griffin" 199 112000000,
 MovieExec "Calvin Coolidge" 123 20000000]

movies :: [Movie]
movies = [Movie "Pretty Woman" 1990 119 "Disney" 199,
 Movie "The Man Who Wasn't There" 2001 116 "USA Entertainm." 555,
 Movie "Logan's run" 1976 120 "Fox" 333,
 Movie "Star Wars" 1977 124 "Fox" 555,
 Movie "Empire Strikes Back" 1980 111 "Fox" 555,
 Movie "Star Trek" 1979 132 "Paramount" 222,
 Movie "Star Trek: Nemesis" 2002 116 "Paramount" 123,
 Movie "Terms of Endearment" 1983 132 "MGM" 123,
 Movie "The Usual Suspects" 1995 106 "MGM" 199,
 Movie "Gone With the Wind" 1938 238 "MGM" 123,
 Movie "The Fellowship of the Ring" 2001 178 "USA Entertainm." 222]

stars :: [MovieStar]
stars = [MovieStar "Jane Fonda" 'F',
 MovieStar "Alec Baldwin" 'M',
 MovieStar "Kim Basinger" 'F',
 MovieStar "Harrison Ford" 'M',
 MovieStar "Debra Winger" 'F',
 MovieStar "Jack Nicholson" 'M',
 MovieStar "Sandra Bullock" 'F',
 MovieStar "Orlando Bloom" 'M',
 MovieStar "Cate Blanchett" 'F',
 MovieStar "Liv Tyler" 'F',
 MovieStar "Billy Bob Thornton" 'M',
 MovieStar "Scarlett Johansson" 'F']

starsIn :: [StarsIn]
starsIn = [StarsIn "Kim Basinger" "Star Wars",
 StarsIn "Alec Baldwin" "Star Wars",
 StarsIn "Harrison Ford" "Star Wars",
 StarsIn "Harrison Ford" "Empire Strikes Back",
 StarsIn "Jack Nicholson" "The Usual Suspects",
 StarsIn "Jane Fonda" "Terms of Endearment",
 StarsIn "Jack Nicholson" "Terms of Endearment",
 StarsIn "Sandra Bulloc" "The Usual Suspects",
 StarsIn "Billy Bob Thornton" "The Man Who Wasn't There",
 StarsIn "Scarlett Johansson" "The Man Who Wasn't There",
 StarsIn "Orlando Bloom" "The Fellowship of the Ring",
 StarsIn "Cate Blanchett" "The Fellowship of the Ring",
 StarsIn "Liv Tyler" "The Fellowship of the Ring"]

db :: MovieDB
db = (movies, stars, starsIn, studios, movieExecs)

getFeaturedStars :: Name -> Int -> MovieDB -> [Name]
getFeaturedStars studioName year db = [actorName | 
                                       (Movie movieTitle movieYear _ name _) <- dbGetMovies db, (StarsIn actorName title) <- dbGetStarsIn db, 
                                        name == studioName && movieYear == year && movieTitle == title]

getPresident :: Name -> MovieDB -> Name
getPresident studioName db = head [ prodName | (Studio name prodID) <- dbGetStudio db, (MovieExec prodName id _) <- dbGetMovieExec db, studioName == name && prodID == id ]

getHigherProductions :: Name -> MovieDB -> [String]
getHigherProductions prodName db = [ movieName | (MovieExec name prodID networth) <- dbGetMovieExec db, (Movie movieName _ _ _ id) <- dbGetMovies db, networth > prodNetWorth && prodID == id]
    where
        prodNetWorth = head [ networth | (MovieExec name pordID networth) <- dbGetMovieExec db, prodName == name]

--getters for db

dbGetMovies :: MovieDB -> [Movie]
dbGetMovies (movies, _, _, _, _) = movies

dbGetStarsIn :: MovieDB -> [StarsIn]
dbGetStarsIn (_, _, starsIn, _, _) = starsIn

dbGetStudio :: MovieDB -> [Studio]
dbGetStudio (_, _, _, studios, _) = studios

dbGetMovieExec :: MovieDB -> [MovieExec]
dbGetMovieExec (_, _, _, _, movieExecs) = movieExecs

--Task 02

data BTree a = Nil | Node a (BTree a) (BTree a) deriving (Show, Eq)

t1 :: BTree Char
t1 = Node 'a' (Node 'b' Nil (Node 'd' Nil Nil)) (Node 'c' (Node 'f' (Node 'e' Nil Nil) Nil) Nil) 

t1Result :: BTree (Char, Int)
t1Result = Node ('a', 2) (Node ('b', 0) Nil (Node ('d', 1) Nil Nil)) (Node ('c', 5) (Node ('f', 4) (Node ('e', 3) Nil Nil) Nil) Nil) 
          
t2 :: BTree Int
t2 = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

t2Result :: BTree (Int, Int)
t2Result = Node (10, 5) (Node (5, 2) (Node (3, 1) (Node (1, 0) Nil Nil) Nil) (Node (7, 4) (Node (6, 3) Nil Nil) Nil)) (Node (15, 7) (Node (13, 6) Nil Nil) (Node (18, 8) Nil Nil))

inorder :: BTree (a, Int) -> [(a, Int)]
inorder Nil = []
inorder (Node x lt rt) = (inorder lt) ++ [x] ++ (inorder rt)

numberNodes :: [(a, Int)] -> [((a, Int), Int)]
numberNodes xs = zip xs [0..]

getLevelsTree :: BTree a -> BTree (a, Int)
getLevelsTree tree = helper tree 0
    where
        helper      Nil         _   = Nil 
        helper (Node v lt rt) level = (Node (v, level)) (helper lt (level + 1)) (helper rt (level + 1))

toBinaryIndexed :: (Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed tree = helper (numberNodes $ inorder $ getLevelsTree tree) 0 
    where 
        helper []    _     = Nil
        helper xs curLevel = if null curNode then Nil else (Node (head curNode)) (helper leftList (curLevel + 1)) (helper rightList (curLevel + 1))
             where curNode = [ (c, num)| ((c, lv), num) <- xs, lv == curLevel] 
                   position =  findPosition xs (head curNode) 
                   leftList = take position xs
                   rightList = if (position + 1) > 1 then drop (position + 1) xs else xs 

findPosition :: (Eq a) => [((a, Int), Int)] -> (a, Int) -> Int
findPosition xs (c, _) = head [i | (((ch, _), _), i) <- zip xs [0..], c == ch]