main :: IO()
main = do

    print ("Task 1")
    print (getMoviesLongerThan "Star Wars" db)
    print (getMoviesLongerThan "The Fellowship of the Ring" db)
    print (getMaleActorsIn "Terms of Endearment" db)
    print (getMaleActorsIn "Star Wars" db)
    print (getFemaleActorsFrom 1983 db)
    print (getFemaleActorsFrom 2001 db)
    print ("Task 2")
    print(degr t1 5)
    print(degr t1 6)
    print(degr t1 7)
    print(degr t1 18)
    print("----")
    print(degr t2 's')
    print(degr t2 'k')
    print(degr t2 'l')

--Task 1
type Name = String
type Title = String
type Year = Int
type Gender = Char
type Length = Int

data Movie = Movie Title Year Length deriving Show
data MovieStar = MovieStar Name Gender deriving Show
data StarsIn = StarsIn Name Title deriving Show

type MovieDB = ([Movie], [MovieStar], [StarsIn])

mov1 :: Movie
mov1 = Movie "Logan's run" 1976 120

movies :: [Movie]
movies = [Movie "The Man Who Wasn't There" 2001 116,
 Movie "Logan's run" 1976 120,
 Movie "Star Wars" 1977 124,
 Movie "Empire Strikes Back" 1980 111,
 Movie "Star Trek" 1979 132,
 Movie "Star Trek: Nemesis" 2002 116,
 Movie "Terms of Endearment" 1983 132,
 Movie "The Usual Suspects" 1995 106,
 Movie "Gone With the Wind" 1938 238,
 Movie "The Fellowship of the Ring" 2001 178]

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
db = (movies, stars, starsIn)

getMoviesLongerThan :: Title -> MovieDB -> [Title]
getMoviesLongerThan title db = [fstMovie currMovie | currMovie <- getMovies db, findMoviesLength title (getMovies db) < thdMovie currMovie]
    where findMoviesLength title movies = helper title movies 0
            where helper title movies i    
                    | fstMovie (head movies) == title = thdMovie (head movies)
                    | otherwise                       = helper title (tail movies) (i + 1)

getMaleActorsIn :: Title -> MovieDB -> [Name]
getMaleActorsIn title db = [person | person <- actorsFromFilm, checkGender person 'M' (getMoiveStars db)]
    where actorsFromFilm = [fstStarsIn actor | actor <- getStars db, sndStarsIn actor == title]

getFemaleActorsFrom :: Year -> MovieDB -> [Name]
getFemaleActorsFrom year db = [actorName | actorName <- females, playInMovie actorName filmsFromYear (getStars db)]
    where females = [fstMovieStar female | female <- getMoiveStars db, checkGender (fstMovieStar female) 'F' (getMoiveStars db)]
          filmsFromYear = [fstMovie film | film <- getMovies db, sndMovie film == year]        

playInMovie :: Name -> [Title] -> [StarsIn] -> Bool
playInMovie actorName titles starsIn = helper actorName titles starsIn 0
    where 
        helper actorName titles starsIn i
            | null titles = False   
            | length starsIn == i                                                                      = helper actorName (tail titles) starsIn 0
            | (fstStarsIn (starsIn !! i) == actorName) && (sndStarsIn (starsIn !! i) == (head titles)) = True 
            | otherwise                                                                                = helper actorName titles starsIn (i + 1)

checkGender :: String -> Char -> [MovieStar] -> Bool
checkGender name symbol actors = helper name symbol actors
             where 
                 helper name symbol actors
                         | name == fstMovieStar (head actors) = if (sndMovieStar (head actors) == symbol) then True else False
                         | otherwise                          = helper name symbol (tail actors)

fstMovie :: Movie -> String--get movie name
fstMovie (Movie title _ _) = title

sndMovie :: Movie -> Int--get movie year
sndMovie (Movie _ year _) = year

thdMovie :: Movie -> Int--get movie length
thdMovie (Movie _ _ length) = length

fstMovieStar :: MovieStar -> String -- get actor's name
fstMovieStar (MovieStar name _) = name

sndMovieStar :: MovieStar -> Char-- get actor's gender
sndMovieStar (MovieStar _ gender) = gender

fstStarsIn :: StarsIn -> String -- get actor's name
fstStarsIn (StarsIn name _) = name

sndStarsIn :: StarsIn -> String -- get the name of the film, which actor is playing
sndStarsIn (StarsIn _ film) = film

getMovies :: MovieDB -> [Movie]
getMovies (movies, _, _) = movies

getMoiveStars :: MovieDB -> [MovieStar]
getMoiveStars (_, movieStars, _) = movieStars

getStars :: MovieDB -> [StarsIn]
getStars (_, _, stars) = stars

--Task2
data NTree a = Nil | Node a [NTree a] deriving (Show)

t1 :: NTree Int
t1 = Node 8 [( Node 7 [(Node 4 [Nil]), (Node 5 [Nil])] ), 
             ( Node 6 [(Node 10 [Nil]), (Node 15 [Nil]), (Node 13 [Nil])] ), 
             ( Node 18 [Nil] )]

t2 :: NTree Char
t2 = Node 'l'[( Node 'f' [( Node 'H' [Nil] ), ( Node 'a' [Nil] )] ),
              ( Node 'm' [( Node 'm' [ ( Node 's' [Nil] ) ] ) ] ), 
              ( Node 'i' [( Node 'k' [ (Node 'e' [Nil]), (Node 'l' [Nil]), (Node 'L' [Nil]) ] ) ] )]

degr :: (Eq a) => NTree a -> a -> Int
degr (Node v subTrees) value = if v == value then length subTrees else checkSubTree subTrees value


checkSubTree :: (Eq a) => [NTree a] -> a -> Int
checkSubTree []   _   = 0
checkSubTree ns value = (helper (head ns) value) + (checkSubTree (tail ns) value)
    where
        helper Nil        _             = 0
        helper (Node v subTrees) value = if v == value then correctResult subTrees + 1 else checkSubTree subTrees value

correctResult :: (Eq a) => [NTree a] -> Int
correctResult subTrees = if helper (head subTrees) then 0 else length subTrees
    where
        helper Nil      = True
        helper subTrees = False
