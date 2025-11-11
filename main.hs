import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe (isJust)
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

-- Adicionar item
-- Recebe o horário, um item, e o inventário; retorna uma string em caso de erro ou ResultadoOperacao em caso de successo
-- Retorna erro caso um item com o mesmo ID já existe no inventário ou se a quantidade é negativa
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem time item (Inventario mapa)
    | Map.member (itemID item) mapa =
        Left "Erro: item já existe no inventário."
    | quantidade item < 0 =
        Left "Erro: quantidade inicial não pode ser negativa."
    | otherwise =
        let novoMapa = Map.insert (itemID item) item mapa
            entradaLog = LogEntry time
                            (Add item)
                            ("Item adicionado: " ++ nome item)
                            (Sucesso "Operação concluída com sucesso.")
        in Right (Inventario novoMapa, entradaLog)


-- Remover item
-- Recebe horário, item, quantidade (int) e inventário; retorna string em caso de erro ou ResultadoOperacao em caso de sucesso
-- Retorna erro se a quantidade for zero ou negativa, se o item não existe no inventário, ou se a quantidade removida for maior do que o estoque
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time id qtd (Inventario mapa)
    | qtd <= 0 =
        Left "Erro: a quantidade removida deve ser positiva."
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "Erro: item não encontrado no inventário."
            Just item
                | quantidade item < qtd ->
                    Left "Erro: estoque insuficiente."
                | otherwise ->
                    let novaQtd = quantidade item - qtd
                        novoMapa =
                            if novaQtd == 0
                                then Map.delete id mapa
                                else Map.insert id (item { quantidade = novaQtd }) mapa
                        logE = LogEntry time
                                        (Remove id)
                                        ("Removidas " ++ show qtd ++ " unidades de " ++ nome item)
                                        (Sucesso "Remoção concluída com sucesso.")
                    in Right (Inventario novoMapa, logE)


-- Atualizar quantidade
-- Recebe horário, ID do item (string), quantidade (int), e inventário; retorna string em caso de erro e ResultadoOperacao em caso de sucesso
-- Retorna erro se quantidade for negativa ou se o item não for encontrado
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time id novaQtd (Inventario mapa)
    | novaQtd < 0 =
        Left "Erro: quantidade não pode ser negativa."
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "Erro: item não encontrado para atualização."
            Just item ->
                let itemAtualizado = item { quantidade = novaQtd }
                    novoMapa = Map.insert id itemAtualizado mapa
                    logE = LogEntry time
                                    (Update itemAtualizado)
                                    ("Quantidade atualizada para " ++ show novaQtd ++ " em " ++ nome item)
                                    (Sucesso "Atualização concluída com sucesso.")
                in Right (Inventario novoMapa, logE)




-------------------------alunos 3 e 4: eu ceci eba--------------------


logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\l -> case status l of Falha _ -> True; _ -> False)

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem = filter (\logE -> case acao logE of
    Add i       -> itemID i == idItem
    Remove iID  -> iID == idItem
    Update i    -> itemID i == idItem
    _           -> False)

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

-----------------------------main e loop----------------------------


main :: IO ()
main = do
    putStrLn "=== Sistema de Inventário ==="

    -- Tenta ler o inventário salvo no arquivo, se existir;
    -- caso contrário, inicia com um inventário vazio.
    conteudoInventario <- catch (readFile "Inventario.dat") 
                                (\(_ :: IOException) -> return (show (Inventario Map.empty)))
    let inventario = read conteudoInventario :: Inventario

    -- Garante que o arquivo de log exista
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
                                appendFile "Auditoria.log" (show (LogEntry t (QueryFail e) e (Falha e)) ++ "\n")
                                putStrLn e
                                loop inventario
                            
                            Right (novo, logE) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show logE ++ "\n")
                                putStrLn "Operação realizada com sucesso."
                                loop novo

                        
                        
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id nome quantidade categoria"
                    putStrLn "Exemplo: 3 batata_frita 90 comida"
                    loop inventario
                    
        "2" -> do
            putStrLn "\nDigite: id quantidade"
            putStrLn "(Atenção: digite nùmeros para id e quantidade)"
            entrada <- getLine
            let ws = words entrada
            case ws of
                [id, qtd]
                    | all (`elem` "0123456789") id && all (`elem` "0123456789") qtd -> do
                        t <- getCurrentTime
                        let qtdNum = read qtd
                        
                        case removeItem t id qtdNum inventario of
                            Left e -> do
                                appendFile "Auditoria.log" (show (LogEntry t (QueryFail e) e (Falha e)) ++ "\n")
                                putStrLn e
                                loop inventario
                        
                            Right (novo, logE) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show logE ++ "\n")
                                putStrLn "Operação realizada com sucesso."
                                loop novo
                        
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id quantidade"
                    putStrLn "Exemplo: 4 500"
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
                                appendFile "Auditoria.log" (show (LogEntry t (QueryFail e) e (Falha e)) ++ "\n")
                                putStrLn e
                                loop inventario
                        
                            Right (novo, logE) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show logE ++ "\n")
                                putStrLn "Operação realizada com sucesso."
                                loop novo

                        
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id novaQuantidade"
                    putStrLn "Exemplo: 2 10"
                    loop inventario

        "4" -> do
            conteudo <- readFile "Auditoria.log"
            let logs = map read (lines conteudo) :: [LogEntry]

            putStrLn "\n--- Logs de Erro ---"
            mapM_ print (logsDeErro logs)

            putStrLn "\n--- Item mais movimentado ---"
            case itemMaisMovimentado logs of
                Nothing -> putStrLn "Nenhum item movimentado ainda."
                Just nomeItem -> putStrLn nomeItem

            putStrLn "\nDigite o ID do item para ver o histórico:"
            idItem <- getLine
            let historico = historicoPorItem idItem logs
            if null historico
                then putStrLn "Nenhum histórico encontrado para esse item."
                else do
                    putStrLn ("\n--- Histórico do item " ++ idItem ++ " ---")
                    mapM_ print historico

            loop inventario



        ----“Executar um comando de ‘listar’ (a ser criado)":  
        "5" -> do
            putStrLn "\n--- Itens no Inventário ---"
            if Map.null (itens inventario)
                then putStrLn "Inventário vazio."
                else mapM_ (\(k, i) ->
                    putStrLn (k ++ ": " ++ nome i ++ " | Quantidade: " ++ show (quantidade i) ++ " | Categoria: " ++ categoria i)
                    ) (Map.toList (itens inventario))
            loop inventario

        "0" -> putStrLn "Encerrando o programa."
        _   -> putStrLn "Opção inválida." >> loop inventario

