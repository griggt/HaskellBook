{-# LANGUAGE LambdaCase #-}

module DotLang.Unparser
  ( unparseGraph
  , testUnparse
  , testRoundtrip
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intercalate)
import Text.Trifecta.Result

import DotLang.Parser
import qualified DotLang.Xml.Unparser as XML

------------------------------------------------------
unparseGraph :: Graph -> String
unparseGraph = graph

testUnparse :: (MonadIO m) => FilePath -> m ()
testUnparse fp = do
  r <- parseGraphFile fp
  do case r of
      Success g -> liftIO $ putStrLn (graph g)
      Failure e -> liftIO $ putStrLn (show e)  -- TODO pretty print errors

testRoundtrip :: (MonadIO m) => FilePath -> m ()
testRoundtrip fp = do
  r <- parseGraphFile fp
  do case r of
      Failure e -> liftIO $ putStrLn (show e)
      Success g -> do 
        case (parseGraphString (graph g)) of
          Failure e' -> liftIO $ putStrLn (show e')
          Success g' ->
            if g == g'
              then liftIO $ putStrLn "Success"
              else do
                liftIO $ putStrLn "Fail"
                liftIO $ putStrLn (show g)
                liftIO $ putStrLn (show g')

------------------------------------------------------

graph :: Graph -> String
graph (Graph typ mId isStrict stmts) =
  concat [
    if isStrict then "strict " else ""
  , (graphType typ) ++ " "
  , maybe "" identifier mId
  ] ++ " {\n" ++ (statementList stmts) ++ "}"

graphType :: GraphType -> String
graphType = \case UndirectedGraph -> "graph"
                  DirectedGraph   -> "digraph"

statement :: Statement -> String
statement (NodeStatement n) = (node n)
statement (EdgeStatement e) = (edge e)
statement (AttributeStatement a) = (attribute a)
statement (AssignmentStatement a) = (assignment a)
statement (SubgraphStatement s) = (subgraph s)

statementList :: [Statement] -> String
statementList xs = concatMap (\x -> (statement x) ++ ";\n") xs

attributeType :: AttributeType -> String
attributeType = \case GraphAttribute -> "graph"
                      NodeAttribute  -> "node"
                      EdgeAttribute  -> "edge"

attribute :: Attribute -> String
attribute (Attribute typ attrs) = (attributeType typ) ++ (attributeList attrs)

attributeList :: AttrList -> String
attributeList [] = ""
attributeList xs = intercalate " " $ map bracketAssignList xs
  where bracketAssignList xs = " [" ++ (assignList xs) ++ "]"

assignList :: [Assignment] -> String
assignList xs = intercalate ", " $ assignment <$> xs

assignment :: Assignment -> String
assignment (Assignment x y) = (identifier x) ++ '=' : (identifier y)

edge :: Edge -> String
edge (Edge ep rhsList attrs) = (endpoint ep) ++ (concatMap edgeRhs rhsList) ++ (attributeList attrs)

edgeRhs :: EdgeRhs -> String
edgeRhs (EdgeRhs op end) = (edgeOp op) ++ (endpoint end)

endpoint :: Endpoint -> String
endpoint (NodeEndpoint n) = nodeId n
endpoint (SubgraphEndpoint s) = subgraph s

subgraph :: Subgraph -> String
subgraph (Subgraph Nothing stmts) = "{\n" ++ (statementList stmts) ++ "}"
subgraph (Subgraph (Just name) stmts) =
  "subgraph " ++ (identifier name) ++ " {\n" ++ (statementList stmts) ++ "}"

node :: Node -> String
node (Node nId attrs) = (nodeId nId) ++ (attributeList attrs)

nodeId :: NodeId -> String
nodeId (NodeId nId mPort) = (identifier nId) ++ (maybe "" port mPort)

identifier :: Identifier -> String
identifier (AlphaNumId x) = x
identifier (NumericId x) = either show show x
identifier (QuotedId x) = '"' : x ++ ['"']
identifier (HtmlId e) = '<' : (XML.unparseElement e) ++ ">"

edgeOp :: EdgeOp -> String
edgeOp = \case EdgeOpDirected -> " -> "
               EdgeOpUndirected -> " -- "

port :: Port -> String
port (Port mId mDir) =
  case mId of
    Nothing   -> ':' : (maybe "" compassPt mDir)
    Just name -> ':' : (identifier name) ++ (maybe "" ((:) ':' . compassPt) mDir)

compassPt :: CompassPt -> String
compassPt (CompassPt x) = x
