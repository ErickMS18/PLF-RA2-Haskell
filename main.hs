import qualified Data.Map as Map
import Data.Time (UTCTime)

data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)
    
data AcaoLog
    = Add Item
    | Remove String
    | Update Item
    | QueryFail String
    deriving (Show, Read, Eq)

data StatusLog
    = Sucesso String
    | Falha String    
    deriving (Show, Read, Eq)
    
data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read, Eq)
    
data Inventario = Inventario
    { itens :: Map.Map String Item
    } deriving (Show, Read, Eq)
    
{- 
Teste da serialização (read.show)
Pra testar, tira o trecho abaixo de comentario e roda o código

main :: IO ()
main = do
    let item = Item "10" "Suco de Goiaba" 3 "Consumivel"
    let itemSerializado = show item
    putStrLn ("Serializado: " ++ itemSerializado)

    let itemDesserializado = read itemSerializado :: Item
    putStrLn ("Desserializado: " ++ show itemDesserializado)

    putStrLn ("São iguais? " ++ show (item == itemDesserializado))
-}

main :: IO ()
main = do
    -- Criando um inventário de exemplo
    let inv = Inventario (Map.fromList
            [ ("1", Item "1" "Suco de Goiaba" 5 "Consumível")
            , ("2", Item "2" "Colar de Santo" 1 "Amuleto")
            ])

    -- SALVAR o inventário no disco
    writeFile "inventario.txt" (show inv)
    putStrLn "Inventário salvo no arquivo inventario.txt"

    -- CARREGAR o inventário do disco
    conteudo <- readFile "inventario.txt"
    let invCarregado = read conteudo :: Inventario

    putStrLn "\nInventário carregado do arquivo:"
    print invCarregado

