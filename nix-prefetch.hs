{-# LANGUAGE OverloadedStrings #-}
import System.Exit
import System.Environment
import Control.Applicative
import Control.Applicative.Combinators
import qualified Nix.Expr as N
import qualified Nix.Pretty as N
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.List (intercalate, find)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import System.Process.Typed
import qualified Data.ByteString.Lazy as BL
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Megaparsec.Error (parseErrorPretty)

type Parser = P.Parsec () String


newtype SHA = SHA String

(<||>) = liftA2 (||)
(<&&>) = liftA2 (&&)
(<++>) = liftA2 (++)
(<:>) = liftA2 (:)

-- Based on git-ref-check-format(1)
refName :: Parser String
refName = refPathComponent <++> (P.char '/' <:> (intercalate "/" <$> refPathComponent `P.sepBy1` P.char '/'))
  where

    -- Is character trivially rejectable?
    neverAccept :: Char -> Bool
    neverAccept = isControl <||> isSpace <||> flip elem ("~^:?*[\\" :: String)

    -- Is character trivially acceptable?
    simpleChar :: Char -> Bool
    simpleChar = not . (neverAccept <||> flip elem ("/@." :: String))

    dot :: Parser Char
    dot = P.char '.' <* P.lookAhead (P.satisfy ((/= '.') <&&> (not . neverAccept))) <* P.notFollowedBy (P.string "lock")

    at :: Parser Char
    at = P.char '@' <* P.lookAhead (P.satisfy ((/= '{') <&&> (not . neverAccept)))

    refPathComponent :: Parser String
    refPathComponent = (P.satisfy simpleChar <|> at) <:> many (P.satisfy simpleChar <|> dot <|> at)

sha :: Parser SHA
sha = SHA <$> count 40 (P.oneOf (['0'..'9'] ++ ['a'..'f'] :: String))

lsRemoteLine :: Parser (String, SHA)
lsRemoteLine = flip (,) <$> (sha <* P.char '\t') <*> (P.string "HEAD" <|> (P.string "refs/" <++> refName)) <* P.newline

refOptions :: String -> [String]
refOptions s = [s, "refs/tags/" ++ s, "refs/heads/" ++ s]

getRef :: String -> String -> IO SHA
getRef remote symbolicRef = do
  (out, _) <- readProcess_ $ proc "git" ["ls-remote", remote, symbolicRef]
  case P.parse (P.some lsRemoteLine <* P.eof) "git output" (TL.unpack $ TL.decodeUtf8 out) of
    (Left err) -> do putStr $ parseErrorPretty err; exitFailure
    (Right results) -> do
      let (Just match) = find isJust (($ results) . lookup <$> refOptions symbolicRef)
      let (Just sha) = match
      return sha

instance P.ShowErrorComponent () where
  showErrorComponent = const ""

class Fetchable a where
  fetchExpr :: a -> IO N.NExpr

data GitHubRepo = GitHubRepo
  { ghOwner :: Text
  , ghRepo :: Text
  , ghRev :: Text
  }

instance Fetchable GitHubRepo where
  fetchExpr (GitHubRepo owner repo rev) =
    do
      let repoUrl = "https://github.com/" <> owner <> "/" <> repo
      commitSha_ <- getRef (T.unpack repoUrl) (T.unpack rev)
      let (SHA commitSha) = commitSha_
      let downloadUrl = repoUrl <> "/archive/" <> T.pack commitSha <> ".tar.gz"

      (storePathHash_, _) <- readProcess_ $ proc "nix-prefetch-url" ["--unpack", T.unpack downloadUrl]
      let (Just storePathHash) = BL.stripSuffix "\n" storePathHash_
      return $ N.mkApp (N.mkSym "fetchFromGitHub") $
        N.mkNonRecSet
          [ N.NamedVar [N.StaticKey "owner"] (N.mkStr owner)
          , N.NamedVar [N.StaticKey "repo"] (N.mkStr repo)
          , N.NamedVar [N.StaticKey "rev"] (N.mkStr $ T.pack commitSha)
          , N.NamedVar [N.StaticKey "sha256"] (N.mkStr $ TL.toStrict $ TL.decodeUtf8 storePathHash)
          ]

data GitLabRepo = GitLabRepo
  { glBase :: Text
  , glOwner :: Text
  , glRepo :: Text
  , glRev :: Text
  }

instance Fetchable GitLabRepo where
  fetchExpr (GitLabRepo base owner repo rev) =
    do
      let repoUrl = base <> owner <> "/" <> repo
      commitSha_ <- getRef (T.unpack repoUrl) (T.unpack rev)
      let (SHA commitSha) = commitSha_
      let downloadUrl = repoUrl <> "/repository/" <> T.pack commitSha <> "/archive.tar.bz2"

      (storePathHash_, _) <- readProcess_ $ proc "nix-prefetch-url" ["--unpack", T.unpack downloadUrl]
      let (Just storePathHash) = BL.stripSuffix "\n" storePathHash_
      return $ N.mkApp (N.mkSym "fetchurl") $
        N.mkNonRecSet
          [ N.NamedVar [N.StaticKey "url"] (N.mkStr downloadUrl)
          , N.NamedVar [N.StaticKey "sha256"] (N.mkStr $ TL.toStrict $ TL.decodeUtf8 storePathHash)
          ]
main :: IO ()
main = do
  owner:repo:args <- getArgs
  let rev = case args of [] -> "master"
                         (x:_) -> x
  expr <- fetchExpr $ GitHubRepo (T.pack owner) (T.pack repo) (T.pack rev)
  print $ N.prettyNix expr
  return ()
