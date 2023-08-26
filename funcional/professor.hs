{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List

-- Função para realizar a filtragem dos dados
filtrarDados :: [[String]] -> Int -> String -> [[String]]
filtrarDados dados coluna valor = filter (\linha -> linha !! coluna /= valor) dados

-- Função para realizar o mapeamento dos dados
mapearDado :: String -> Float
mapearDado x = read x * 1.1

mapearDados :: [[String]] -> Int -> (String -> String) -> [[String]]
mapearDados dados coluna funcao = [[if i == coluna then funcao (linha !! coluna) else valor | (i, valor) <- zip [0..] linha] | linha <- dados]

-- Função para realizar a redução dos dados
reduzirDados :: [[String]] -> Int -> (Float -> Float -> Float) -> Float
reduzirDados dados coluna funcao = foldl' (\acc linha -> funcao acc (read (linha !! coluna))) 0 dados

-- Função para ordenar os dados
ordenarDados :: [[String]] -> Int -> [[String]]
ordenarDados dados coluna = sortBy (\a b -> compare (a !! coluna) (b !! coluna)) dados

-- Função para carregar os dados de um arquivo CSV
carregarDadosArquivo :: FilePath -> IO [[String]]
carregarDadosArquivo nomeArquivo = do
    conteudo <- TIO.readFile nomeArquivo
    let linhas = map (T.splitOn ",") (T.lines conteudo)
    return $ map T.unpack <$> linhas

main :: IO ()
main = do
    t5_start <- getCPUTime
    dados <- carregarDadosArquivo "dados.csv" -- UTILIZE QUALQUER CSV que se adeque com o problema.
    t5_stop <- getCPUTime
    let dados2 = dados

    putStrLn $ "Número de linhas carregadas: " ++ show (length dados2)

    t1_start <- getCPUTime
    let dados_filtrados = filtrarDados dados2 1 "30"
    t1_stop <- getCPUTime

    putStrLn $ "Número de linhas após filtragem: " ++ show (length dados_filtrados)

    t2_start <- getCPUTime
    let dados_mapeados = mapearDados dados_filtrados 2 (show . mapearDado)
    t2_stop <- getCPUTime

    t3_start <- getCPUTime
    let media_salarios = reduzirDados dados_mapeados 2 (+) / fromIntegral (length dados_mapeados)
    t3_stop <- getCPUTime

    t4_start <- getCPUTime
    let dados_ordenados = ordenarDados dados_mapeados 0
    t4_stop <- getCPUTime

    putStrLn "Dados Filtrados:"
    mapM_ print dados_filtrados

    putStrLn "Dados Mapeados:"
    mapM_ print dados_mapeados

    putStrLn "Média dos Salários:"
    print media_salarios

    putStrLn "Dados Ordenados:"
    mapM_ print dados_ordenados

    putStrLn $ "Tempo de execução para carregar os dados: " ++ show (fromIntegral (t5_stop - t5_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de execução para filtrar os dados: " ++ show (fromIntegral (t1_stop - t1_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de execução para mapear os dados: " ++ show (fromIntegral (t2_stop - t2_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de execução para reduzir os dados: " ++ show (fromIntegral (t3_stop - t3_start) / (10^12 :: Double)) ++ " segundos"
    putStrLn $ "Tempo de execução para ordenar os dados: " ++ show (fromIntegral (t4_stop - t4_start) / (10^12 :: Double)) ++ " segundos"
