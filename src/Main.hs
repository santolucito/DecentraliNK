module Main where
 
import Network.Socket

socketPortNum = 4242 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet socketPortNum iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2 -- set a max of 2 queued connections
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    msg <- recv sock 10
    print msg
    close sock
