import System.CPUTime
import Control.DeepSeq
import Data.List

-- Função para realizar a filtragem dos dados
filtrarDados :: [[String]] -> Int -> String -> [[String]]
filtrarDados dados coluna valor = filter (\linha -> linha !! coluna == valor) dados

-- Função para realizar o mapeamento dos dados
mapearDados :: [[String]] -> Int -> (String -> String) -> [[String]]
mapearDados dados coluna funcao = map (\linha -> [if i == coluna then funcao (linha !! coluna) else valor | (i, valor) <- zip [0..] linha]) dados

-- Função para reduzir os dados
reduzirDados :: [[String]] -> Int -> (Float -> Float -> Float) -> Float
reduzirDados dados coluna funcao = foldl (\acc linha -> funcao acc (read (linha !! coluna)) ) (read (head (dados !! 1)) !! coluna) (tail dados)

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
    let dados_filtrados = filtrarDados dados 1 "30"
    
    t2_start <- getCPUTime
    let dados_mapeados = mapearDados dados_filtrados 2 (\x -> show (read x * 1.1))
    t2_stop <- getCPUTime
    
    t3_start <- getCPUTime
    let media_salarios = reduzirDados dados_mapeados 2 (+) / fromIntegral (length dados_mapeados)
    t3_stop <- getCPUTime
    
    t4_start <- getCPUTime
    let dados_ordenados = ordenarDados dados_mapeados 0
    t4_stop <- getCPUTime

    putStrLn "Dados Filtrados:"
    deepseq dados_filtrados (print dados_filtrados)
    
    putStrLn "Dados Mapeados:"
    deepseq dados_mapeados (print dados_mapeados)
    
    putStrLn "Média dos Salários:"
    deepseq media_salarios (print media_salarios)
    
    putStrLn "Dados Ordenados:"
    deepseq dados_ordenados (print dados_ordenados)
    
    putStrLn $ "Tempo de leitura do arquivo: " ++ show (fromIntegral (t1_stop - t1_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de filtragem dos dados: " ++ show (fromIntegral (t2_stop - t2_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de mapeamento dos dados: " ++ show (fromIntegral (t3_stop - t3_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de ordenação dos dados: " ++ show (fromIntegral (t4_stop - t4_start) / (10^12 :: Double)) ++ " segundos"
