module Network.HTTP.Client

import Data.Buffer.Indexed
import Data.ByteString
import Data.IORef
import Network.HTTP.Connection
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.HTTP.Request
import Network.HTTP.Response
import Network.HTTP.URL
import Network.Socket


Host : Type
Host = String


public export
record Address where
  constructor TCP
  host : Host
  port : Port


public export
data ClientError : Type where
  ClientConnectError : Int -> ClientError
  ClientProtocolError : ProtocolError -> ClientError
  ClientSocketError : SocketError -> ClientError


connectionPort : URL -> Port
connectionPort url =
  case url.port of
    Just port => port
    Nothing => case url.scheme of
      "http" => 80
      "https" => 443
      "ws" => 80
      "wss" => 443
      _ => 80


newConnection : Address -> IO (Either ClientError Connection)
newConnection addr = do
  Right sock <- socket AF_INET Stream 0
  | Left err => pure $ Left $ ClientSocketError err
  connectResult <- connect sock (Hostname addr.host) addr.port
  if connectResult /= 0
    then pure $ Left $ ClientConnectError connectResult
    else do
      connection <- newConnection sock
      pure $ Right connection


public export
request : Connection -> Request ByteString ->
          IO (Either ClientError (Response Connection))
request connection req = do
  MkConnectionBuffer _ sock <- readIORef connection
  Right sendResult <- send sock $ http1Request req
  | Left err => pure $ Left $ ClientSocketError err
  if sendResult /= 0
    then pure $ Left $ ClientSocketError sendResult
    else do
      connection <- newConnection sock
      Right response <- readResponseHeaders connection
      | Left (ConnectionSocketError err) => pure $ Left $ ClientSocketError err
      | Left (ConnectionProtocolError err) => pure $ Left $ ClientProtocolError err
      pure $ Right response
