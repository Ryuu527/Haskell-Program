module Main where

import Data.Char (toLower, isSpace, isAlpha, isDigit)
import Data.List (isInfixOf,intercalate)

data Ingredient = Ingredient { name :: String, quantity :: String } deriving (Show)

data CookingMethod = Bake | Fry | Boil | Grill deriving (Show, Read, Eq)

data Recipe = Recipe { title :: String, ingredients :: [Ingredient], method :: CookingMethod } deriving (Show)

type RecipeBook = [Recipe]

addRecipe :: Recipe -> RecipeBook -> Either String RecipeBook
addRecipe newRecipe recipes
  | any (\r -> title r == title newRecipe) recipes = Left "Recipe already exists"
  | otherwise = Right (newRecipe : recipes)

searchByIngredient :: String -> RecipeBook -> RecipeBook
searchByIngredient ingredient = filter (any (\(Ingredient name _) -> map toLower name == map toLower ingredient) . ingredients)

searchByTitle :: String -> RecipeBook -> RecipeBook
searchByTitle titlePart = filter ((isInfixOf (map toLower titlePart).map toLower . title))

shoppingList :: RecipeBook -> [Ingredient]
shoppingList = foldr mergeIngredients [] . concatMap ingredients
    where
        mergeIngredients newIng [] = [newIng]
        mergeIngredients newIng (x:xs)
          |name newIng == name x = Ingredient (name newIng) (quantity newIng ++ ", " ++ quantity x) : xs
          | otherwise = x : mergeIngredients newIng xs

-- Function to let user input and validate ingredients
inputIngredients :: IO [Ingredient]
inputIngredients = do
      putStrLn "Enter ingredients (format: name,quantity), "
      putStrLn "Enter 'done' when finished:"
      inputIngredientsHelper []

isValidQuantity :: String -> Bool
isValidQuantity quantity = all isQuantityChar quantity && any isDigit quantity
  where isQuantityChar c = isDigit c || c `elem` "gmlpkcsMGMLPCSK " 


inputIngredientsHelper :: [Ingredient] -> IO [Ingredient]
inputIngredientsHelper acc = do
    input <- getLine
    if input == "done"
    then return acc
    else let parts = splitOn ',' input
         in if length parts /= 2
            then putStrLn "Invalid format, Please try again: " >> inputIngredientsHelper acc
            else let [name, quantity] = parts
                 in if all (\c -> isAlpha c || isSpace c) name && isValidQuantity quantity
                   then inputIngredientsHelper (Ingredient (trim name) (trim quantity) : acc)
                   else putStrLn "Invalid format, Please try again: " >> inputIngredientsHelper acc

showIngredient :: Ingredient -> String
showIngredient (Ingredient name quantity) = name ++ "," ++ quantity

showMergeIngredients :: RecipeBook -> String
showMergeIngredients  = intercalate "\n" . map showIngredient . shoppingList

searchAndShowIngredient :: String -> RecipeBook -> String
searchAndShowIngredient ingredient = intercalate "\n\n" . map formatRecipe . searchByIngredient ingredient

searchAndShowTitle :: String -> RecipeBook -> String
searchAndShowTitle titlePart = intercalate "\n\n" . map formatRecipe . searchByTitle titlePart

formatRecipe :: Recipe -> String 
formatRecipe (Recipe title ings method) =
    "\nRecipe Name: " ++ title ++ "\nCooking Method: " ++ show method ++ "\nIngredients:\n" ++ (intercalate "\n" . map showIngredient $ ings)

                   
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace 

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = [] : l
                     | otherwise = (c : x) : xs
                    

--main menu to handle user input and perform correspoding features
menu :: RecipeBook -> IO ()
menu recipes = do
    putStrLn "\n"
    putStrLn "====================================="
    putStrLn "    Welcome to Recipe Manager !      "
    putStrLn "====================================="
    putStrLn "-------------------------------------"
    putStrLn "| 1. Add a Recipe                   |"
    putStrLn "| 2. Search by Ingredient           |"
    putStrLn "| 3. Search by Title                |"
    putStrLn "| 4. Show Shopping List             |"
    putStrLn "| 5. Exit                           |"
    putStrLn "-------------------------------------"
    putStrLn " "
    putStrLn "Enter your choice:"

    choice <- getLine
    case choice of 
        "1" -> do
            putStrLn "Enter recipe title: "
            title <- getLine
            putStrLn "Enter cooking method (Bake, Fry, Boil, Grill): "
            methodStr <- getLine
            case reads methodStr :: [(CookingMethod, String)] of
                [(method, "")] -> do
                    ingredients <- inputIngredients
                    case addRecipe (Recipe title ingredients method) recipes of
                        Left errMsg -> putStrLn errMsg >> menu recipes
                        Right newRecipes -> putStrLn "\nRecipe added!" >> menu newRecipes
                _ -> putStrLn "Invalid cooking method. Try again." >> menu recipes  
              
        "2" -> do
          putStrLn "Enter ingredient to search: "
          ingredient <- getLine
          putStrLn $ searchAndShowIngredient ingredient recipes 
          menu recipes 
        "3" -> do
          putStrLn "Enter title to search: "
          title <- getLine
          putStrLn $ searchAndShowTitle title recipes 
          menu recipes
        
        "4" -> do
          putStrLn "\n"
          putStrLn "Your shopping list: "
          putStrLn $ showMergeIngredients recipes 
          menu recipes 
        
        "5" -> putStrLn "Exiting.."
        
        _-> putStrLn "Invalid choice, Please try again!" >> menu recipes


main :: IO ()
main = menu []