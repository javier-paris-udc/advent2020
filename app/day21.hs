module Main where


import           AoC                 (applyInputSWith, blankP, commaSepP)
import           Text.Parsec.String  (Parser)
import           Data.HashMap.Strict (HashMap, foldlWithKey')
import           Text.Parsec         (between
                                     ,char
                                     ,letter
                                     ,many1
                                     ,newline
                                     ,sepBy
                                     ,sepEndBy
                                     ,string)
import qualified Data.HashMap.Strict as Map
import           Data.Foldable       (foldl')
import           Data.List           (delete, intercalate, intersect, sortOn)
import           Data.Function       ((&))
import           Data.Maybe          (isJust, isNothing, fromJust)


type Allergen      = String
type Ingredient    = String
type Food          = [Ingredient]
type AllergenMap   = HashMap Allergen Food
type IngredientMap = HashMap Ingredient (Maybe Allergen)


findAllergens :: AllergenMap -> IngredientMap -> IngredientMap
findAllergens allergMap ingMap
    | Map.null allergMap = ingMap
    | otherwise          = findAllergens newAllergMap newIngMap
  where
    isSingle [_] = True
    isSingle _   = False
    newAllergs   = Map.filter isSingle allergMap
    newIngrs     = concat $ Map.elems newAllergs
    newAllergMap = newAllergs
                 & foldlWithKey' (\m k _ -> Map.delete k m) allergMap
                 & Map.map (\l -> foldl' (flip delete) l newIngrs)
    newIngMap    = Map.foldlWithKey' addIngs ingMap newAllergs
    addIngs ingm alerg ings = 
        case ings of
            [ing] -> Map.insert ing (Just alerg) ingm
            _     -> undefined

solveP2 :: (AllergenMap, IngredientMap, [Food]) -> String
solveP2 (allergMap, ingMap, _) =
      findAllergens allergMap ingMap
    & Map.filter isJust
    & Map.map fromJust
    & Map.toList
    & sortOn snd
    & map fst
    & intercalate ","


solveP1 :: (AllergenMap, IngredientMap, [Food]) -> Int
solveP1 (allergMap, ingMap, foods) =
      findAllergens allergMap ingMap
    & Map.filter isNothing
    & Map.mapWithKey (\i _ -> length $ filter (i `elem`) foods)
    & sum


ingredientsP :: Parser (AllergenMap, IngredientMap, [Food])
ingredientsP = do
    foods         <- foodP `sepEndBy` newline
    let allergenMap   = foods
                      & foldl' addAllergen Map.empty
                      & Map.map (foldl1 intersect)
        ingredientMap = foldl' addIngredients Map.empty (map fst foods)
    return (allergenMap, ingredientMap, map fst foods)
  where
    foodP = do
        ingredients <- nameP `sepEndBy` blankP
        allergens   <- between (char '(') (char ')') allergensP
        return (ingredients, allergens)
    nameP = many1 letter
    allergensP = do
        _ <- string "contains "
        nameP `sepBy` commaSepP
    addAllergen allerMap (ingredients, allergens) =
        foldl' (\m a ->  Map.insertWith (++) a [ingredients] m) allerMap allergens
    addIngredients = foldl' (\m i -> Map.insert i Nothing m)


main :: IO ()
main = applyInputSWith ingredientsP () solveP1 solveP2 print putStrLn