import System.CPUTime
import Data.List

-- Função para realizar a filtragem recursiva dos dados
filtrarDados :: [[String]] -> Int -> String -> [[String]]
filtrarDados [] _ _ = []
filtrarDados (linha:resto) coluna valor
    | linha !! coluna == valor = filtrarDados resto coluna valor
    | otherwise = linha : filtrarDados resto coluna valor

-- Função para realizar o mapeamento dos dados
mapearDados :: [[String]] -> Int -> (String -> String) -> [[String]]
mapearDados [] _ _ = []
mapearDados (linha:resto) coluna funcao = (map (\(i, valor) -> if i == coluna then funcao valor else valor) (zip [0..] linha)) : mapearDados resto coluna funcao

-- Função para reduzir os dados
reduzirDados :: [[String]] -> Int -> (Float -> Float -> Float) -> Float
reduzirDados [] _ _ = error "Lista vazia"
reduzirDados [linha] coluna _ = read (linha !! coluna)
reduzirDados (linha:resto) coluna funcao = funcao (read (linha !! coluna)) (reduzirDados resto coluna funcao)

-- Função para ordenar os dados
ordenarDados :: [[String]] -> Int -> [[String]]
ordenarDados dados coluna = sortBy (\a b -> compare (a !! coluna) (b !! coluna)) dados

-- Função para carregar os dados de um arquivo CSV
carregarDadosArquivo :: FilePath -> IO [[String]]
carregarDadosArquivo nomeArquivo = do
    conteudo <- readFile nomeArquivo
    let linhas = map (splitOn ',') (lines conteudo)
    return linhas

-- Função auxiliar para dividir uma string em uma lista de strings usando um delimitador
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [""] where
    f c l@(x:xs) | c == delimiter = "":l
                 | otherwise = (c:x):xs

-- Exemplo de uso do código
main :: IO ()
main = do
    t1_start <- getCPUTime
    dados <- carregarDadosArquivo "dados.csv" -- UTILIZE QUALQUER CSV que se adeque com o problema.
    t1_stop <- getCPUTime

    let dados_copia = dados

    t2_start <- getCPUTime
    let dados_filtrados = filtrarDados dados_copia 1 "30"
    t2_stop <- getCPUTime

    let dados_filtrados_copia = dados_filtrados

    t3_start <- getCPUTime
    let dados_mapeados = mapearDados dados_filtrados_copia 2 (\x -> show (read x * 1.1))
    t3_stop <- getCPUTime

    let dados_mapeados_copia = dados_mapeados

    t4_start <- getCPUTime
    let media_salarios = reduzirDados dados_mapeados_copia 2 (+) / fromIntegral (length dados_mapeados)
    t4_stop <- getCPUTime

    t5_start <- getCPUTime
    let dados_ordenados = ordenarDados dados_mapeados_copia 0
    t5_stop <- getCPUTime

    putStrLn "Dados Filtrados:"
    print dados_filtrados

    putStrLn "Dados Mapeados:"
    print dados_mapeados

    putStrLn "Média dos Salários:"
    print media_salarios

    putStrLn "Dados Ordenados:"
    print dados_ordenados

    putStrLn $ "Tempo de leitura do arquivo: " ++ show (fromIntegral (t1_stop - t1_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de filtragem dos dados: " ++ show (fromIntegral (t2_stop - t2_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de mapeamento dos dados: " ++ show (fromIntegral (t3_stop - t3_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de redução dos dados: " ++ show (fromIntegral (t4_stop - t4_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de ordenação dos dados: " ++ show (fromIntegral (t5_stop - t5_start) / (10^12 :: Double)) ++ " segundos"
