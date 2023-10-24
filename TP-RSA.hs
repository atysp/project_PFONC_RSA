import Data.Char (ord,chr)
data Message = Mess [Integer] deriving Show

stringToMessage::String->Message
stringToMessage "" = Mess []
stringToMessage str = Mess $ map (fromIntegral.ord) str

messageToString::Message->String
messageToString (Mess []) = ""
messageToString (Mess (x:xs)) = (chr (fromIntegral x)) : (messageToString (Mess xs))

pad::Int->Message->Message
pad lbloc (Mess mess) = let  grandn = length mess `mod` lbloc in 
                         if grandn == 0 
                          then Mess ( mess ++ replicate (fromIntegral lbloc) (fromIntegral lbloc))
                          else Mess (mess ++ replicate (fromIntegral grandn) (fromIntegral grandn))

unpad::Int->Message->Message
unpad lbloc (Mess mess) = let nb = mess!!((length mess)-1) in Mess $ take (length mess-fromIntegral nb)  mess

nongroupBytes::[Integer]->Integer
nongroupBytes [] = 0
nongroupBytes (x:xs) = x + 256* nongroupBytes xs

groupBytes::[Integer]->Integer
groupBytes = nongroupBytes.reverse

ungroupBytes::Int->Integer->[Integer]
ungroupBytes lbloc n = reverse $ map snd (take lbloc (tail (iterate (\(q,x)->(q `div` 256, q `mod` 256)) (fromIntegral n,fromIntegral n`mod`256))))

groupN::Int->[Integer]->[[Integer]]
groupN _ [] = []
groupN bsize l = (take bsize l) : (groupN bsize $ drop bsize l)

makeBlocks::Int->Message->Message
makeBlocks bsize (Mess m)= Mess (map groupBytes $ groupN bsize m)

--Indice : on pourra utiliser la fonction concat
splitBlocks::Int->Message->Message
splitBlocks bsize (Mess m)=Mess (concat $ map (ungroupBytes bsize) (map fromIntegral m))

--fonction de selection des potentiels diviseurs de n
selection x
    | x == 2 = True 
    | x == 3 = True
    | x `mod` 6 == 1 = True 
    | x `mod` 6 == 5 = True
    | otherwise = False
--on crée une liste des diviseurs allant de 1 à sqrt n 
aff n = filter selection [x |x<-[2..floor (sqrt $ fromIntegral n)]]
--on regarde si la longeurs de la liste des diviseurs premiers est non nulle
prime::Integer->Bool
prime n = if length (filter diviseur (aff n))==0 then True else False
    where diviseur x
            | n `mod` x == 0 = True
            | otherwise = False

choosePrime::Integer->Integer
choosePrime n = head $ dropWhile (<n) [p | p<-[(n+1)..], prime p] -- pk depart de 1 et dropwhile??

euclide' r u v 0 u' v' = (r,u,v)
euclide' r u v r' u' v' = 
    let rnew = r `mod` r' in 
    let qnew = r `div` r' in 
    let unew =  (u- qnew*u') in 
    let vnew = (v- qnew*v') in  euclide' r' u' v' rnew unew vnew 

euclid::Integer->Integer->(Integer,Integer,Integer)
euclid a b  = euclide' a 1 0 b 0 1
--Attention : le résultat doit être positif--
second::(Integer,Integer,Integer)->Integer
second (a,b,c) = b
modInv::Integer->Integer->Integer
modInv e n =  
    if second (euclid e n)>0 then second (euclid e n) else error "non inversible"
--si le coefficient de Bézout devant e est négati
--alors e n'est pas inversible module n ie il n'existe pas q tel que (eq) congru 1 mod n

--Rappel:pourn≥1,xn =(x2)n2 sinestpairetxn =x∗xn−1 sinon
-- modExp::Integer->Integer->Integer->Integer
-- modExp m 0 n = 1
-- modExp m e n = if (e `mod` 2) == 0
--                 then (modExp m (e `div` 2) n*modExp m (e `div` 2) n) `mod` n
--                 else m*(modExp m (e `div` 2) n*modExp m (e `div` 2) n) `mod` n

modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp m e n
  | even e    = (r*r) `mod` n
  | otherwise = (m*r*r) `mod` n
  where
    r = modExp m (e `div` 2) n

--Écrire une fonction encrypt qui à partir d’une clé (publique) (e, n), d’une longueur
--de bloc, et d’une chaîne de caractères renvoit un message chiffré par bloc, dans lequel chaque bloc a été chiffré par RSA ;
encrypt::(Integer,Integer)->Int->String->Message
encrypt (e,n) bsize listecara = 
    let (Mess mess) = stringToMessage(listecara) in
    let (Mess newMess) = pad bsize (Mess mess) in 
    let (Mess messEnBlock)= makeBlocks bsize (Mess newMess) in 
        Mess (map (\x-> modExp x e n) (map fromIntegral messEnBlock))

--  Écrire une fonction decrypt qui à partir d’une clé (privée) (d, n), d’une longueur de bloc,
--   et d’un message chiffré par bloc par RSA renvoit la chaîne de caractères déchiffrée.
decrypt::(Integer,Integer)->Int->Message->String
decrypt (d,n) bsize (Mess m) = 
    let (Mess messDechiffre) = Mess (map (\x-> modExp x d n) (map fromIntegral m)) in 
    let (Mess messSansBlock) = splitBlocks bsize (Mess messDechiffre) in 
    let (Mess newMess) = unpad bsize (Mess messSansBlock) in messageToString(Mess newMess)

decrypt' p bsize (Mess m)=
    let e = 65537 in 
    let q = choosePrime p in 
    let phin = (p-1)*(q-1) in
    let d = modInv e phin in
    let n = p*q in 
    let (Mess messDechiffre) = Mess (map (\x-> modExp x d n) (map fromIntegral m)) in
    let (Mess messSansBlock) = splitBlocks bsize (Mess messDechiffre) in 
    let (Mess newMess) = unpad bsize (Mess messSansBlock) in messageToString(Mess newMess)

-- Écrire une fonction principale permettant de tester tout le processus.
--on rappelle que n doit être plus grand que l’entier le plus grand qui encode un bloc, soit 256^b, où b est la taille du bloc

fonctionPrincipale::Int->Integer->String->String
fonctionPrincipale bsize p l = 
    let e = 65537 in 
    let q = choosePrime p in 
    if (prime p == True)&&((p-1)*(q-1)>256^bsize)
     then 
        let phin = (p-1)*(q-1) in
        let n = p*q in
        let (Mess messCrypt) = encrypt (e,n) bsize l in
        let d = modInv e phin in decrypt (d,n) bsize (Mess messCrypt)
     else error "n n'est pas assez grand veuiller rechoisir p ..."

main = do
     print $ fonctionPrincipale 3 4999 "abdefghijklmnoprqtyvwbebfbzvbb zdnajhncheiaub hello world hello kevin"

