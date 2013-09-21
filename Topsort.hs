{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<*>))
import Data.List (partition,filter, nub)

-- Una dipendenza è una coppia di valori dello stesso tipo 
type Dipendenza a = (a,a)

-- il primo valore della coppia è quello dipendente
dipendente (x,y) = x

-- il secondo valore della coppia è quello della dipendenza
dipendenza (x,y) = y

-- calcola un ordinamento topologico quando possibile dato un insieme di dipendenze e un insieme di indipendenti
topsort 
        :: Eq a           -- i valori di tipo a devono essere confrontabile con l'uguaglianza
        => [Dipendenza a] -- l'insieme delle dipendenze
        -> [a]            -- insieme iniziale di indipendenti
        -> Maybe [a]      -- un possibile ordinamento, dai meno dipendenti ai più

-- primo caso entrambi gli insiemi sono vuoti, risultato raggiunto, ritorno un insieme vuoto nel contenitore Just che indica successo
topsort [] [] = Just []   
-- secondo caso , sono avanzate dipendenze e quindi il grafo è ciclico e non si può avere l'ordinamento
topsort  _  [] = Nothing
-- ultimo caso ricorsivo, gli insiemi sono entrambi non vuoti, smonto il primo elemento dell'insieme degli indipendenti n, gli ys sono i restanti
topsort  zs (n:ys)  =  let
        -- divido l'insieme delle dipendenze in 2 parti 'rs' sono le dipendenze dove n è una dipendenza, zs' sono i restanti
        (rs,zs') = partition ((== n) . dipendenza) zs
        -- estraggo i dipendenti da rs e li filtro tenendo solo quelli che non appaiono più tra le restanti dipendenze
        ms' = filter (\x -> not $ x `elem` (map dipendente zs')) $ map dipendente rs
        -- ricorro topsort sull'insieme zs' con l'unione dei rimasti indipendenti ys con i nuovi ms' e aggiungo il valore n al risultato della ricorsione
        in fmap (n:) $ topsort  zs' (ys ++ ms') 


-- estrazione degli indipendenti iniziali
independent     :: forall a. Eq a       -- i valori di tipo a devono essere confrontabile con l'uguaglianza
                =>  [Dipendenza a]      -- l'insieme delle dipendenze
                -> [a]                  -- gli indipendenti iniziali

independent xs = let 
        -- giudica positivo quando tutti i dipendenti nell'insieme sono diversi dalla dipendenza di x :: Dipendenza
        judge :: Dipendenza a -> Bool
        judge x = all ((/=) (dipendenza x) . dipendente)   xs
        -- filtro le dipendenze con il giudice e mi tengo solo i valori dipendenza ed elimino i duplicati (nub)
        in nub . map dipendenza . filter judge $ xs


-- topsort chiamato con i dipendenti iniziali
tsort :: (Show a, Eq a) => [Dipendenza a] -> Maybe [a]
tsort = topsort <*> independent


