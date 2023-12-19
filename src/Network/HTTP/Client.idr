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


public export
data Result : Type -> Type -> Type where
  Ok : a -> Result error a
  Error : error -> Result error a

public export
Functor (Result error) where
  map f (Ok a) = Ok (f a)
  map _ (Error error) = Error error

public export
Applicative (Result error) where
  pure = Ok
  (<*>) (Ok f) (Ok a) = Ok (f a)
  (<*>) (Error error) _ = Error error
  (<*>) _ (Error error) = Error error

public export
Monad (Result error) where
  (>>=) (Ok a) f = f a
  (>>=) (Error error) _ = Error error


ClientResult : Type -> Type
ClientResult = Result ClientError


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


newConnection : Address -> IO (ClientResult Connection)
newConnection addr = do
  Right sock <- socket AF_INET Stream 0
  | Left err => pure $ Error $ ClientSocketError err
  connectResult <- connect sock (Hostname addr.host) addr.port
  if connectResult /= 0
    then pure $ Error $ ClientConnectError connectResult
    else do
      connection <- newConnection sock
      pure $ Ok connection


public export
makeRequest : Connection -> Request ByteString -> IO (ClientResult (Response Connection))
makeRequest connection req = do
  MkConnectionBuffer _ sock <- readIORef connection
  Right sendResult <- send sock $ http1Request req
  | Left err => pure $ Error $ ClientSocketError err
  if sendResult /= 0
    then pure $ Error $ ClientSocketError sendResult
    else do
      connection <- newConnection sock
      Right response <- readResponseHeaders connection
      | Left (ConnectionSocketError err) => pure $ Error $ ClientSocketError err
      | Left (ConnectionProtocolError err) => pure $ Error $ ClientProtocolError err
      pure $ Ok response
