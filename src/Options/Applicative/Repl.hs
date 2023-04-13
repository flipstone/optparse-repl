module Options.Applicative.Repl
  ( ReplConfig
      ( ReplConfig
      , runCommand
      , echo
      , welcomeMessage
      , history
      )
  , HistoryConfig
    ( HistoryConfig
    , historyFile
    , historyMaxEntries
    )
  , repl
  , commandParser
  ) where

import qualified Control.Exception.Safe as SafeEx
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Char as Char
import qualified Options.Applicative as Opt
import qualified System.Console.Isocline as Isocline

data ReplConfig command m = ReplConfig
  { prompt :: String
  , runCommand :: command -> m ()
  , echo :: String -> m ()
  , welcomeMessage :: Maybe String
  , history :: Maybe HistoryConfig
  }

data HistoryConfig = HistoryConfig
  { historyFile :: FilePath
  , historyMaxEntries :: Int
  }

commandParser ::
  (MIO.MonadIO m, SafeEx.MonadCatch m) =>
  ReplConfig command m ->
  Opt.ParserInfo command ->
  Opt.Parser (m ())
commandParser config replInfo =
  pure (repl config replInfo)

repl ::
  (MIO.MonadIO m, SafeEx.MonadCatch m) =>
  ReplConfig command m ->
  Opt.ParserInfo command ->
  m ()
repl config replInfo =
  let
    completion =
      replCompletion replInfo

    prepare = do
      case history config of
        Nothing -> pure ()
        Just hc ->
          MIO.liftIO (Isocline.setHistory (historyFile hc) (historyMaxEntries hc))

      traverse
        (echo config)
        (welcomeMessage config)

    run = do
      mbLine <- readlineWithCompletion (prompt config) completion
      case mbLine of
        Nothing -> pure ()
        Just line -> do
          runParsedCommand config (parseReplCommand line replInfo)
          run
  in
    prepare >> run

replCompletion ::
  Opt.ParserInfo a ->
  Isocline.CompletionEnv ->
  String ->
  IO ()
replCompletion parserInfo completionEnv line = do
  completions <- completeReplCommand line parserInfo
  _okToKeepAdding <- Isocline.addCompletions completionEnv completions
  pure ()

runParsedCommand ::
  SafeEx.MonadCatch m =>
  ReplConfig command m ->
  Opt.ParserResult command ->
  m ()
runParsedCommand config parserResult =
  case parserResult of
    Opt.Success command -> do
      result <- SafeEx.tryAny (runCommand config command)
      case result of
        Right () -> pure ()
        Left exception -> echo config (SafeEx.displayException exception)
    Opt.Failure failure ->
      let
        (help, _exitCode) =
          Opt.renderFailure failure ""
      in
        echo config help
    Opt.CompletionInvoked _completion -> do
      echo config "Completion not currently supported"

parseReplCommand :: String -> Opt.ParserInfo a -> Opt.ParserResult a
parseReplCommand line parserInfo =
  let
    args = words line
  in
    Opt.execParserPure
      replParserPrefs
      parserInfo
      args

completeReplCommand :: String -> Opt.ParserInfo a -> IO [Isocline.Completion]
completeReplCommand line parserInfo =
  let
    lineWords =
      strictWords line

    index =
      length lineWords

    -- optparse-applicative ignores the first word for the purposes of
    -- completion because it's assumed to be the program name on the command
    -- line
    args =
      "repl" : lineWords

    completionArgs =
      ( "--bash-completion-index"
          : show index
          : concatMap (\a -> ["--bash-completion-word", a]) args
      )

    prefixLength =
      lengthOfLast lineWords

    mkIsolineCompletion bashCompletion =
      Isocline.Completion
        { Isocline.replacement = drop prefixLength bashCompletion
        , Isocline.display = bashCompletion
        , Isocline.help = ""
        }
  in
    case Opt.execParserPure replParserPrefs parserInfo completionArgs of
      Opt.Success _ -> pure []
      Opt.Failure _failure -> pure []
      Opt.CompletionInvoked completionResult -> do
        bashCompletionOutput <- Opt.execCompletion completionResult "repl"
        pure (map mkIsolineCompletion (lines bashCompletionOutput))

lengthOfLast :: [String] -> Int
lengthOfLast options =
  case options of
    [] -> 0
    [lastWord] -> length lastWord
    (_ : rest) -> lengthOfLast rest

strictWords :: String -> [String]
strictWords input =
  case input of
    "" -> []
    _ ->
      case dropWhile Char.isSpace input of
        "" ->
          [""]
        nonSpace ->
          let
            (prefix, rest) =
              break Char.isSpace nonSpace
          in
            prefix : strictWords rest

replParserPrefs :: Opt.ParserPrefs
replParserPrefs =
  Opt.prefs $
    Opt.showHelpOnEmpty
      <> Opt.showHelpOnError

readlineWithCompletion ::
  MIO.MonadIO m =>
  String ->
  (Isocline.CompletionEnv -> String -> IO ()) ->
  m (Maybe String)
readlineWithCompletion replPrompt completer =
  MIO.liftIO (Isocline.readlineExMaybe replPrompt (Just completer) Nothing)
