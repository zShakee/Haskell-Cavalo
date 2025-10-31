

movimentosPossiveis x y linhas colunas = 
    let moves = [ (-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]
        todosMovimentos = map (\(dx, dy) -> (x + dx, y + dy)) moves
    in filter (\(nx, ny) -> nx >= 0 && nx < linhas && ny >= 0 && ny < colunas) todosMovimentos 
ehAdjacente (x, y) (x_inicial,y_inicial) = --Verifica se duas posições estão a um movimento de cavalo de distância
    (abs(x - x_inicial), abs(y - y_inicial)) `elem` [(2, 1), (1, 2)]

--grau visitados 
grau visitados linhas colunas x y =
    length [ (nx, ny) | (nx, ny) <- movimentosPossiveis x y linhas colunas, not ((nx, ny) `elem` visitados) ]

-- Aplica o algoritmo de Warnsdorff
warnsdorff :: Int -> Int -> (Int, Int) -> [(Int, Int)]
warnsdorff linhas colunas inicio = go [inicio]
  where
    total = linhas * colunas

    go caminho
        | length caminho == total = reverse caminho
        | null candidatos         = []  -- Travou
        | otherwise               = go (proximo : caminho)
      where
        (x, y) = head caminho
        possiveis = [(nx, ny) | (nx, ny) <- movimentosPossiveis x y linhas colunas, not ((nx, ny) `elem` caminho)]
        candidatos = ordenaMenorGrau possiveis caminho
        proximo = head candidatos

        ordenaMenorGrau [] _ = []
        ordenaMenorGrau (p:ps) caminho =
            let menores = [x | x <- ps, grau caminho linhas colunas (fst x) (snd x) <= grau caminho linhas colunas (fst p) (snd p)]
                maiores = [x | x <- ps, grau caminho linhas colunas (fst x) (snd x) > grau caminho linhas colunas (fst p) (snd p)]
            in ordenaMenorGrau menores caminho ++ [p] ++ ordenaMenorGrau maiores caminho

main :: IO ()
main = do
    conteudo <- readFile "entrada.txt"
    let valores = map read (words conteudo) :: [Int]
    let processar [] = return ()
        processar (linhas:colunas:x:y:resto) = do
            putStrLn $ "\nTabuleiro " ++ show linhas ++ "x" ++ show colunas ++ ", início em (" ++ show x ++ "," ++ show y ++ ")"
            let caminho = warnsdorff linhas colunas (x, y)
            if null caminho then
                putStrLn "Nenhum caminho encontrado."
            else if ehAdjacente (head caminho) (last caminho) then
                putStrLn "Caminho fechado (não é aberto)."
            else do
                putStrLn $ "Caminho aberto encontrado com " ++ show (length caminho) ++ " movimentos:"
                mapM_ print (caminho)
            processar resto
    processar valores