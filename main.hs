import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe (isJust)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)

-------------------------------------------------
-- Dados
-------------------------------------------------

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
    
    
    
-------------------------------------------------------------------------------------
-- Lógica
-------------------------------------------------------------------------------------

-- Definição de tipo
-- O resultado de uma operação vai retornar o inventário e a entrada no log que descreve a operação
type ResultadoOperacao = (Inventario, LogEntry)

-- Adicionar item: cria um novo item no inventário
-- Recebe o horário, um item, e o inventário; retorna uma string em caso de erro ou ResultadoOperacao em caso de successo
-- Retorna erro caso um item com o mesmo ID já existe no inventário ou se a quantidade é negativa
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem horario item (Inventario mapa)
    | Map.member (itemID item) mapa =
        Left "ERRO: o item ja existe no inventario"
    | quantidade item < 0 =
        Left "ERRO: a quantidade nao pode ser negativa"
    | otherwise =
        let novoMapa = Map.insert (itemID item) item mapa
            entradaLog = LogEntry horario
                            (Add item)
                            ("Item adicionado: " ++ nome item)
                            (Sucesso "Item adicionado com sucesso")
        in Right (Inventario novoMapa, entradaLog)


-- Remover item: remove um item completamente do inventário
-- Recebe horário, item, ID do item e inventário; retorna string em caso de erro ou ResultadoOperacao em caso de sucesso
-- Retorna erro se o item não existe no inventário
removeItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao
removeItem horario id (Inventario mapa) =
    case Map.lookup id mapa of
        Nothing ->
            Left "ERRO: item nao existe no inventario"
        Just item ->
            let novoMapa = Map.delete id mapa
                entradaLog = LogEntry horario
                                (Remove id)
                                ("Item removido: " ++ nome item)
                                (Sucesso "Item removido com sucesso")
            in Right (Inventario novoMapa, entradaLog)


-- Reduz a quantide de um item (para cumprir com os requisitos)
-- Consideramos isso uma operação de atualizar a quantidade, portanto retorna a ação Update
-- Recebe horário, ID do item, quantidade e inventário; retorna string em caso de erro ou ResultadoOperacao em caso de sucesso
-- Retorna erro caso a quantidade seja negativa, caso o item não exista, ou caso não exista estoque o suficiente
sellItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
sellItem horario id qtd (Inventario mapa)
    | qtd <= 0 =
        Left "ERRO: a quantidade vendida deve ser positiva."
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "ERRO: item não encontrado no inventário."
            Just item ->
                let estoqueAtual = quantidade item
                    novaQtd = estoqueAtual - qtd
                in if novaQtd < 0
                    then Left "ERRO: estoque insuficiente para a venda."
                    else updateQty horario id novaQtd (Inventario mapa)

-- Atualizar quantidade: muda o estoque do item no inventário
-- Recebe horário, ID do item (string), quantidade (int), e inventário; retorna string em caso de erro e ResultadoOperacao em caso de sucesso
-- Retorna erro se quantidade for negativa ou se o item não for encontrado
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty horario id novaQtd (Inventario mapa)
    | novaQtd < 0 =
        Left "ERRO: quantidade nao pode ser negativa"
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "ERRO: item nao existe no inventario"
            Just item ->
                let itemAtualizado = item { quantidade = novaQtd }
                    novoMapa = Map.insert id itemAtualizado mapa
                    entradaLog = LogEntry horario
                                    (Update itemAtualizado)
                                    ("Quantidade atualizada para " ++ show novaQtd ++ " em: " ++ nome item)
                                    (Sucesso "Quantidade atualizada com sucesso")
                in Right (Inventario novoMapa, entradaLog)




-------------------------------------------------------------------------------------
-- Logs e relatórios
-------------------------------------------------------------------------------------


logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\l -> case status l of Falha _ -> True; _ -> False)

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem =
    filter (\entrada -> case acao entrada of
        Add i       -> itemID i == idItem
        Remove iID  -> iID == idItem
        Update i    -> itemID i == idItem
        QueryFail _ -> ("ID: " ++ idItem) `isInfixOf` detalhes entrada
        )

itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado [] = Nothing
itemMaisMovimentado logs =
    let nomes = [ case acao l of
                    Add i      -> nome i
                    Remove id  ->
                        case [nome i | LogEntry _ (Add i) _ _ <- logs, itemID i == id] of
                            (n:_) -> n
                            []    -> "<desconhecido>"
                    Update i   -> nome i
                    _          -> "" | l <- logs ]

        contagem = Map.fromListWith (+) [(n, 1) | n <- nomes, n /= ""]

    in if Map.null contagem
        then Nothing
        else
            let lista = Map.toList contagem
                (itemMax, _) = foldr1 (\x y -> if snd x > snd y then x else y) lista
            in Just itemMax
            
            
-- mini função auxiliar que cria uma entrada de log
criarLog :: UTCTime -> AcaoLog -> String -> StatusLog -> LogEntry
criarLog horario acao descricao status =
    LogEntry
        { timestamp = horario
        , acao = acao
        , detalhes = descricao
        , status = status
        }

-- funções auxiliares para imprimir entradas de log e ações de forma mais legível
imprimirLogEntry :: LogEntry -> String
imprimirLogEntry (LogEntry timestamp acao detalhes status) =
    unwords
        [ show timestamp
        , "|"
        , imprimirAcao acao
        , "|"
        , detalhes
        , "|"
        , show(status)
        ]

imprimirAcao :: AcaoLog -> String
imprimirAcao (Add item) = "Adição (ID: " ++ itemID item ++ ", nome: " ++ nome item ++ ", quantidade: " ++ show (quantidade item) ++ ", categoria: " ++ categoria item ++ ")"
imprimirAcao (Remove id) = "Remoção (ID: " ++ id ++ ")"
imprimirAcao (Update item) = "Atualização (ID: " ++ itemID item ++ ", nome: " ++ nome item ++ ", quantidade: " ++ show (quantidade item) ++ ", categoria: " ++ categoria item ++ ")"
imprimirAcao (QueryFail msg) = "Erro: " ++ msg

-------------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------------


main :: IO ()
main = do
    putStrLn "=== Sistema de Inventário ==="

    -- Tenta ler o inventário salvo no arquivo, se existir;
    -- caso contrário, inicia com um inventário vazio.
    conteudoInventario <- catch (readFile "Inventario.dat") 
                                (\(_ :: IOException) -> return (show (Inventario Map.empty)))
    let inventario = read conteudoInventario :: Inventario

    -- Garante que o arquivo de log exista e lê o arquivo
    existe <- doesFileExist "Auditoria.log"
    if not existe then writeFile "Auditoria.log" "" else return ()

    -- Inicia o loop principal
    loop inventario


loop :: Inventario -> IO ()
loop inventario = do
    putStrLn "\n=== MENU ==="
    putStrLn "Escolha uma opção: "
    putStrLn "1 - Adicionar item"
    putStrLn "2 - Remover item"
    putStrLn "3 - Atualizar quantidade"
    putStrLn "4 - Report"
    putStrLn "5 - Listar itens do inventário"
    putStrLn "6 - Vender item"
    putStrLn "0 - Sair"
    
    opcao <- getLine
    case opcao of
    
    
    --- apesar do ID ser string aqui é filtrado pra ser numero pra mostrar uma mensagem de erro
    --- q nao seja a "item não encontrado para atualização" (no opção 3 por exemplo), pq o erro nao é q nao foi encontrado, foi q o id nao pode ser uma letra
    --- e dai a quantidade nao é string, é numero e se a pessao digitasse algo q nao fosse numero o programa
    --- parava de rodar, entao é feita uma filtragem pra quando a pessoa digitar ser numero, se nao for, nm chga no read
    --- (é no read q dava erro pq ele tentava ler um valor numerico e era letra e parava de rodar), dai mostra uma mnesagem de erro
    --- e reinicia o loop
    --- fiz isso pro 1,2 e 3
    
        "1" -> do
            putStrLn "\nDigite: id nome quantidade categoria"
            putStrLn "(Atenção: use _ no lugar de espaços e digite números para id e quantidade)"
            entrada <- getLine
            let ws = words entrada
            case ws of
                (id:nome:qtd:cat:_)
                    | all (`elem` "0123456789") id && all (`elem` "0123456789") qtd -> do
                        t <- getCurrentTime
                        let item = Item id nome (read qtd) cat
                        case addItem t item inventario of
                        
                            Left e -> do
                                let logErro = criarLog t (QueryFail "Erro ao adicionar item") ("ID: " ++ id ++ ", Nome: " ++ nome) (Falha e)
                                appendFile "Auditoria.log" (show logErro ++ "\n")
                                putStrLn e
                                loop inventario
                            
                            Right (novo, entradaLog) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show entradaLog ++ "\n")
                                putStrLn "Item adicionado com sucesso"
                                loop novo

                        
                        
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id nome quantidade categoria"
                    putStrLn "Exemplo: 3 batata_frita 90 comida"
                    loop inventario
                    
        "2" -> do
            putStrLn "\nDigite: id"
            putStrLn "(Atenção: o ID deve ser numérico)"
            entrada <- getLine
        
            -- validar input (id numerico)
            if all (`elem` "0123456789") entrada then do
                t <- getCurrentTime
        
                case removeItem t entrada inventario of
                    Left e -> do
                        let logErro = criarLog t (QueryFail "Erro ao remover item") ("ID: " ++ entrada) (Falha e)
                        appendFile "Auditoria.log" (show logErro ++ "\n")
                        putStrLn e
                        loop inventario
        
                    Right (novo, entradaLog) -> do
                        writeFile "Inventario.dat" (show novo)
                        appendFile "Auditoria.log" (show entradaLog ++ "\n")
                        putStrLn "Item removido com sucesso"
                        loop novo
        
            else do
                putStrLn "\nEntrada inválida. O ID deve conter apenas números."
                loop inventario

        "3" -> do
            putStrLn "\nDigite: id novaQuantidade"
            putStrLn "(Atenção: digite nùmeros para id e quantidade)"
            entrada <- getLine
            let ws = words entrada
            case ws of
                [id, novaQtd]
                    | all (`elem` "0123456789") id && all (`elem` "0123456789") novaQtd -> do
                        t <- getCurrentTime
                        let novaQtdNum = read novaQtd
                        
                        case updateQty t id novaQtdNum inventario of
                            Left e -> do
                                let logErro = criarLog t (QueryFail "Erro ao alterar quantidade") ("ID: " ++ id ++ ", quantidade: " ++ novaQtd) (Falha e)
                                appendFile "Auditoria.log" (show logErro ++ "\n")
                                putStrLn e
                                loop inventario
                        
                            Right (novo, entradaLog) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show entradaLog ++ "\n")
                                putStrLn "Quantidade atualizada com sucesso"
                                loop novo

                        
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id novaQuantidade"
                    putStrLn "Exemplo: 2 10"
                    loop inventario

        "4" -> do
            conteudo <- readFile "Auditoria.log"
            let logs = map read (lines conteudo) :: [LogEntry]

            putStrLn "\n--- Logs de Erro ---"
            mapM_ (putStrLn . imprimirLogEntry) (logsDeErro logs)

            putStrLn "\n--- Item mais movimentado ---"
            case itemMaisMovimentado logs of
                Nothing -> putStrLn "Nenhum item movimentado ainda"
                Just nomeItem -> putStrLn nomeItem

            putStrLn "\nDigite o ID do item para ver o histórico:"
            idItem <- getLine
            let historico = historicoPorItem idItem logs
            if null historico
                then putStrLn "Nenhum histórico encontrado para esse item"
                else do
                    putStrLn ("\n--- Histórico do item " ++ idItem ++ " ---")
                    mapM_ (putStrLn . imprimirLogEntry) historico

            loop inventario



        "5" -> do
            putStrLn "\n--- Itens no Inventário ---"
            if Map.null (itens inventario)
                then putStrLn "Inventário vazio"
                else mapM_ (\(k, i) ->
                    putStrLn (k ++ ": " ++ nome i ++ " | Quantidade: " ++ show (quantidade i) ++ " | Categoria: " ++ categoria i)
                    ) (Map.toList (itens inventario))
            loop inventario
            
        "6" -> do
            putStrLn "\nDigite: id quantidade_vendida"
            putStrLn "(Atenção: digite números para id e quantidade)"
            entrada <- getLine
            let ws = words entrada
            case ws of
                [id, qtd]
                    | all (`elem` "0123456789") id && all (`elem` "0123456789") qtd -> do
                        tempo <- getCurrentTime
                        let qtdNum = read qtd
                        case sellItem tempo id qtdNum inventario of
                            Left erro -> do
                                let logE = LogEntry tempo (QueryFail "Erro ao vender item")
                                                    ("ID: " ++ id)
                                                    (Falha erro)
                                appendFile "Auditoria.log" (show logE ++ "\n")
                                putStrLn erro
                                loop inventario
                            Right (novo, logE) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show logE ++ "\n")
                                putStrLn "Venda registrada com sucesso."
                                loop novo
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id quantidade_vendida"
                    putStrLn "Exemplo: 3 5"
                    loop inventario

        "0" -> putStrLn "Encerrando o programa"
        _   -> putStrLn "Opção inválida" >> loop inventario
