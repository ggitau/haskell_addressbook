menu = "1.Add address\n2.List addresses\n3.Exit"
main = do
    prompt []
    
data Address=Address String String String    
    deriving Show

prompt :: [Address] -> IO()
prompt addrs=  do 
    putStrLn menu
    choice <- getLine
    inteprate addrs choice

inteprate :: [Address]->String -> IO()
inteprate addrs "1" = do
    putStr "Names:"
    names <- getLine

    putStr "Phone:"
    phone <- getLine

    putStr "Email:"
    email <- getLine

    
    putStrLn "Address added"
    prompt (addAddr(getAddr names phone email) addrs)

inteprate addrs "2" = do 
    printAddrs addrs
    prompt addrs
    

inteprate addrs "3" = putStrLn "Good bye"
inteprate _ _ = putStrLn "Don't know what to do with ya!"

getAddr ::String->String->String->Address
getAddr names phone email = Address names phone email

addAddr::Address->[Address]->[Address]
addAddr addr addrs = addr:addrs

printAddrs::[Address]->IO()
printAddrs addrs= putStrLn (fmtAddresses (tail addrs) (head addrs) "")

fmtAddresses::[Address]->Address->String->String
fmtAddresses addrs (Address name phone email) str
  | (length addrs==0) = currStr
  | (length addrs /=0 ) = fmtAddresses (tail addrs) (head addrs) currStr 
  where currStr = str++"Names:"++"Phone:"++phone++"Email:"++email++"\n"





