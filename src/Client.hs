module Main where
 
import Network.Socket
import qualified Network.Socket.ByteString as B (recv,send) 
import Data.ByteString

-- Client
-- this listens for a messag from the local system that the usb was mounted
-- then sends a tcp message to the server over http
socketPortNum = "4242"
main :: IO ()
main = do
    addr:_ <- getAddrInfo 
                (Just defaultHints {addrSocketType = Stream }) 
                (Just "127.0.0.1") 
                (Just socketPortNum)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock $ addrAddress addr   -- listen on TCP port 4242.
    listen sock 2 -- set a max of 2 queued connections
    listenForUsbMount sock

-- this will handle the local socket message sent by the /etc/udev/rules.d when a usb is mounted
-- this can be simulated by running 'echo "message" | netcat localhost 4242'
listenForUsbMount :: Socket -> IO ()
listenForUsbMount sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    listenForUsbMount sock  -- repeat
 
--extract the mountTarget from the usb mount and pass along
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    mountTarget <- B.recv sock 10
    requestServer mountTarget
    close sock

-- request a scp action from server to copy directly to the mounted usb device
requestServer :: ByteString -> IO()
requestServer mountTarget = do
    addr:_ <- getAddrInfo 
                (Just defaultHints {addrSocketType = Stream }) 
                (Just "127.0.0.1") 
                (Just "http")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    bytesSent <- B.send sock mountTarget
    print bytesSent
    close sock

