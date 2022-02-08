import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import System.IO



type Cliente = (String,String,Integer)

clienteToString :: Cliente -> String
clienteToString (nome,rua,num) = nome ++ ", " ++ rua ++ ", " ++ show num 

fulano :: Cliente
fulano = ("Fulano", "Rua a", 9999)
ciclano :: Cliente
ciclano = ("Ciclano", "Rua b", 8888)

clientes :: [Cliente]
clientes = [fulano, ciclano]

type Date = (Integer, Int, Int)
date :: IO (Integer, Int, Int)

date = getCurrentTime >>= return . toGregorian . utctDay

datapedido= (10,10,2020) -- :: (Data Pedido )
dataRecebido= (10,10,2020) -- :: (Data Entrega)


expAtrasado :: PExpress
expAtrasado = (555, 20.0*1.2, ciclano, datapedido, dataRecebido)

pedidosExp = [expAtrasado]




p1="Pedidos"
p2="Clientes"
p3="Expresso"

dateToString :: Date -> String
dateToString (day,month,year) = show day ++ "-" ++ show month ++ "-" ++ show year
 
type Pedido = (Integer, Double, Cliente, Date)
pedidos :: [Pedido]
pedidos = [(123, 20.0, fulano, datapedido)]

pedidoToString :: Pedido -> String
pedidoToString (num,preco,cliente,date) = show num ++ ", " ++ show  preco ++ ", " ++ clienteToString cliente ++ ", " ++ dateToString date

type PExpress = (Integer, Double, Cliente, Date, Date)

pedidoExpToString :: PExpress -> String
pedidoExpToString (num,preco,cliente,date, entrega) = pedidoToString (num,preco,cliente,date) ++ "\n" ++ foiEntregue (entregueNoPrazo (num,preco,cliente,date, entrega))

entregueNoPrazo :: PExpress -> Bool
entregueNoPrazo (_, _, _, date, dateEntrega) = date == dateEntrega

foiEntregue :: Bool -> String
foiEntregue x
            | x = "[Sem Atraso]"
            | otherwise = "[Com Atraso]"


escreverPedido :: [Pedido] -> IO ()
escreverPedido pedidos = do
    arq <- openFile "pedido.txt" AppendMode
    hPrint arq p1
    hPrint arq pedidos
    hPrint arq p2
    hPrint arq clientes
    hPrint arq p3
    hPrint arq pedidosExp
    hFlush  arq
    hClose arq



main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    let (year, month, day) = toGregorian $ localDay zoneNow
    putStrLn $ "Data Atual " ++ show day ++ "-" ++ show month ++ "-" ++ show year 
    print (map clienteToString clientes)
    print (map pedidoToString pedidos)
    print (foiEntregue (entregueNoPrazo expAtrasado))
    putStrLn (pedidoExpToString expAtrasado)
    
  
    escreverPedido pedidos
   
    
