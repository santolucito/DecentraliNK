module Main where
 
import Network.Socket
import qualified Network.Socket.ByteString as B (recv) 
import System.Process
import System.Exit

maxAttempts = 3

-- The server listens on a socket for client requests to scp
main :: IO ()
main = do
    addr:_ <- getAddrInfo 
                (Just defaultHints {addrSocketType = Stream }) 
                (Just "127.0.0.1") 
                (Just "http")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock $ addrAddress addr   -- listen on TCP port 80.
    listen sock 2 -- set a max of 2 queued connections
    listenForClientReq sock

-- this will handle the local socket message sent by the /etc/udev/rules.d when a usb is mounted
-- this can be simulated by running 'echo "message" | netcat localhost 4242'
listenForClientReq :: Socket -> IO ()
listenForClientReq sock = do
    conn <- accept sock      -- accept a connection and handle it
    runConn (fst conn) 0           -- run our server's logic
    listenForClientReq sock  -- repeat
 
--extract the mountTarget from the usb mount and pass along
runConn :: Socket -> Int -> IO ()
runConn sock attempts =
    if attempts == maxAttempts
    then do
      mountTarget <- B.recv sock 10
      print ("got request to send to: "++(show mountTarget))
      scpExitCode <- system "scp /home/mark/test.txt mark@127.0.0.1:/home/mark/test2.txt"
      case scpExitCode of
        -- 0: Success
        ExitSuccess -> return ()
        -- 1: Undetermined error in file copy
        ExitFailure 1 -> return ()
        -- 2: Destination is not directory, but it should be
        ExitFailure 2 -> return ()
        -- 3: Connecting to host failed
        ExitFailure 3 -> runConn sock (attempts+1)
      close sock
    else
      close sock
