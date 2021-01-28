module SynthesisMainUtil (synthesis) where

import           REPL
import           System.Console.GetOpt (getOpt, usageInfo, ArgDescr(OptArg)
                                      , ArgOrder(Permute), OptDescr(..))
import           Err
import           Syntax
import qualified System.Console.Haskeline as HL
import           Eval
import           Name
import           EnvLike as E
import           Syntax.Typed as TD
import           Value
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as State (gets, modify)
import           Data.IORef
import           System.Directory (getCurrentDirectory, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)
import           DataDecl
import           Typing
import           Synthesis.SynthesisMain (Example(Example)
                                        , synthesisMain) -- (fillHoleTest, synthesisTest, Example)
import           Syntax.Parser
import           System.IO
import           SrcLoc (noLoc)
import           Synthesis.PureExpression (convertToExp, rename)

startREPL_ :: VerbosityLevel -> FilePath -> Name -> [Name] -> FilePath -> IO ()
startREPL_ v funs_path root nonRoots examples_path = do
  conf <- initConf
  let conf' = conf { verbosity = v }
  ref <- newIORef conf'
  let setting = HL.setComplete (replCompletion ref) HL.defaultSettings
  homeDir <- getHomeDirectory
  let histfilepath = homeDir </> ".HOBiT_history"
  let setting' = setting { HL.historyFile = Just histfilepath }
  HL.runInputT setting'
    $ runREPL
      (procLoadSynthesis_ funs_path (root:nonRoots)
       >> procLoadExamples2 root funs_path examples_path
       >> procSynthesis_ (root:nonRoots) root)
      ref

procLoadSynthesis_ :: String -> [Name] -> REPL ()
procLoadSynthesis_ filepath fs = do
  -- load synthesis program
  v <- State.gets verbosity
  loadProgram
    v
    (trimSpace filepath)
    (State.modify (\conf -> conf { currentFilePath = Just filepath })
     >> error "")
    $ \env' tenv' syn' -> State.modify
      (\conf -> conf { currentEnv = env'
                     , currentTyEnv = tenv'
                     , currentTySyn = syn'
                     , currentFilePath = Just filepath
                     })
  tenv <- State.gets currentTyEnv
  syn <- State.gets currentTySyn
  ref <- State.gets uniqSupply
  denv <- State.gets currentDataEnv
  let path = filepath
  loaded <- liftIO $ loadDecls path
  case loaded of
    Bad s -> do
      liftIO $ putStrLn s
      liftIO $ putStrLn $ filepath ++ "cannot load."
      error ""
    Ok (tyDecls, decls) -> do
      State.modify (\conf -> conf { currentDataEnv = addDataEnv denv tyDecls })
      denv <- State.gets currentDataEnv
      liftIO $ putStrLn "\nCurrent Data Env ::"
      liftIO $ mapM_ print denv
      liftIO $ putStrLn "\nLoaded Functions ::"
      mapM_
        (\(Decl name mty lexp) -> do
           errTExp <- liftIO
             $ inferDeclToTExp (Decl name mty lexp) (tenv, syn) ref
           case errTExp of
             Bad s   -> liftIO $ putStrLn s
             Ok texp -> do
               if name `elem` fs
                 then do
                   tdenv <- State.gets currentTExpEnv
                   State.modify
                     (\conf -> conf { currentTExpEnv = insert name texp tdenv })
                 else do
                   tdenv <- State.gets currentNonBXTExpEnv
                   State.modify
                     (\conf
                      -> conf { currentNonBXTExpEnv = insert name texp tdenv })
               liftIO $ putStr $ show name ++ " = "
               liftIO $ putStrLn $ showTExp texp
               liftIO $ putStrLn "")
        decls

      -- State.modify
      --   (\conf -> conf { currentTyEnv = E.deleteAll fs tenv
      --                  , currentEnv = E.deleteAll fs env
      --                  })
loadProgramForExamples
  :: Int
  -> FilePath
  -> FilePath
  -> REPL ()
  -> (Env -> TyEnv -> Synonyms -> REPL ())
  -> REPL ()
loadProgramForExamples vlevel fp_funs fp_example kfail ksucc = do
  curDir <- State.gets currentDir
  r <- liftIO
    $ catchAny
      (go2 curDir fp_funs fp_example defaultEnv defaultTyEnv defaultSynonyms)
      (\e -> print e >> return Nothing)
  case r of
    Just (env, tenv, syn) -> liftIO (putStrLn "Ok.") >> ksucc env tenv syn
    Nothing -> liftIO (putStrLn "Error(s) occurred.") >> kfail
  where
    go2 curDir fp1 fp2 env tenv syn =
      let f [] pc env tenv syn = return $ Just (env, tenv, syn)
          f (fp:fps) pc env tenv syn = do
            r <- go curDir fp pc env tenv syn
            case r of
              Just (env', tenv', syn') -> f fps (fp:pc) env' tenv' syn'
              Nothing -> return Nothing
      in f [fp1, fp2] [] env tenv syn

    go curDir fp proced env tenv syn
      | fp `elem` proced = return $ Just (env, tenv, syn)
    go curDir fp proced env tenv syn = do
      str <- readFile (curDir </> fp)
      case parseProgram fp str of
        Bad errs -> do
          putStrLn errs
          return Nothing
        Ok (fps, tdecls, prog) -> do
          let f [] pc env tenv syn = return $ Just (env, tenv, syn)
              f (fp:fps) pc env tenv syn = do
                r <- go curDir fp pc env tenv syn
                case r of
                  Just (env', tenv', syn') -> f fps (fp:pc) env' tenv' syn'
                  Nothing -> return Nothing
          r <- f fps proced env tenv syn
          case r of
            Nothing -> return Nothing
            Just (env', tenv', syn') -> do
              let (tenv'', syn'') = toTyEnv (tenv', syn') tdecls
              res <- inferDecls prog (tenv'', syn'')
              case res of
                Bad errs   -> do
                  putStrLn errs
                  return Nothing
                Ok tenv''' -> do
                  return (Just (toEnv env' prog, tenv''', syn''))

lsvsType = TyForAll [a, b] $ TyTup [tl, ta, tb, ta]
  where
    tl = ta `TyArr` tb

    a = BoundTv (Name "a")

    b = BoundTv (Name "b")

    ta = TyVar a

    tb = TyVar b

procLoadExamples2 :: Name -> String -> String -> REPL ()
procLoadExamples2 root fp_funs fp_examples = do
  v <- State.gets verbosity
  tenv0 <- State.gets currentTyEnv
  let Just ty_root = E.lookup root tenv0
  loadProgramForExamples
    v
    (trimSpace fp_funs)
    (trimSpace fp_examples)
    (error "")
    $ \env' tenv' syn' -> do
      let envList = toList env'
          examples_name = map fst
            $ filter
              (\(name, _) -> case name of
                 Name ('e':'x':_) -> True
                 _ -> False)
              envList
      mapM_
        (\ex -> case evalAsUsual (noLoc $ Syntax.EVar ex) env' of
           Bad s -> do
             liftIO $ putStrLn ("example " ++ show ex ++ " cannot reduced.")
             liftIO $ putStrLn $ fp_examples ++ " cannot load."
             liftIO $ putStrLn s
             error ""
           Ok (VCon NTup [s, v, s']) -> do
             let Just ty_ex = E.lookup ex tenv'
             unifiable <- liftIO
               $ checkTypeUnifiable tenv' syn' (convType ty_root) ty_ex
             if unifiable
               then do
                 examples <- State.gets currentExamples
                 State.modify
                   (\conf -> conf { currentExamples = (Example s v s'):examples
                                  })
               else do
                 liftIO $ putStrLn $ fp_examples ++ " cannot load."
                 liftIO
                   $ putStrLn
                   $ "the type of example " ++ show ex ++ " is incorrect"
                 error ""
           Ok _ -> do
             liftIO $ putStrLn $ fp_examples ++ "cannot load."
             liftIO $ putStrLn $ "example " ++ show ex ++ " is not a 3-tuple."
             error "")
        examples_name
  where
    convType :: Ty -> Ty
    convType (TyForAll abc ty) = TyForAll abc (convType ty)
    convType (TyArr s t) = TyTup [s, t, s]

-- procLoadExamples_ :: String -> REPL ()
-- procLoadExamples_ filepath = do
--   env <- State.gets currentEnv
--   let path = filepath
--   loaded <- liftIO $ loadDecls path
--   case loaded of
--     Bad s -> do
--       liftIO $ putStrLn s
--       liftIO $ putStrLn $ filepath ++ "cannot load."
--       askCommand
--     Ok (tyDecls, decls) -> do
--       mapM_
--         (\(Decl name mty lexp) -> do
--            case evalAsUsual lexp env of
--              Bad s -> do
--                liftIO $ putStrLn s
--                liftIO $ putStrLn $ filepath ++ "cannot load."
--                return ()
--              Ok (VCon NTup [s, v, s']) -> do
--                examples <- State.gets currentExamples
--                State.modify
--                  (\conf -> conf { currentExamples = (Example s v s'):examples })
--              Ok _ -> do
--                liftIO $ putStrLn $ filepath ++ "cannot load."
--                liftIO
--                  $ putStrLn
--                  $ "example " ++ show name ++ " is not a 3-tuple."
--                return ())
--         decls
procSynthesis_ :: [Name] -> Name -> REPL ()
procSynthesis_ fs f0 = do
  env <- State.gets currentNonBXTExpEnv
  tyenv <- State.gets currentTyEnv
  syn <- State.gets currentTySyn
  ref <- State.gets uniqSupply
  tdenv <- State.gets currentTExpEnv
  denv <- State.gets currentDataEnv
  examples <- State.gets currentExamples
  liftIO $ putStrLn "Current NonBX Env ::"
  liftIO $ putStrLn $ showEnvLike env
  liftIO $ putStrLn "Current TExp Env ::"
  liftIO $ putStrLn $ showEnvLike tdenv
  case E.lookup f0 tdenv of
    Just e  -> liftIO
      $ do
        let ret = synthesisMain
              tyenv
              syn
              denv
              ref
              (E.toList env)
              (f0, e)
              (E.toList $ E.delete f0 tdenv)
              examples
        putStrLn "\nresult:\n"
        putStrLn (showResult (map fst (toList env) ++ fs) ret)
    Nothing -> liftIO $ putStrLn $ "We cannot find " ++ show f0
  where
    showResult _ Nothing = "We cannot find solution."
    showResult names (Just l) = foldr
      (\(name, ty, e) str -> show name
       ++ " :: "
       ++ show ty
       ++ "\n"
       ++ show (PairNameExp name (convertToExp (rename names e)))
       ++ "\n\n"
       ++ str)
      ""
      l

    envv :: TExpEnv -> [Name]
    envv env = map fst (toList env)

options :: [OptDescr Int]
options = [ Option
              ['v']
              ["verbose"]
              (OptArg
                 (\s -> let r = maybe 1 (\s -> max (read s :: Int) 0) s
                        in r)
                 "INT")
              "Verbosity level [default = 1]"]

procOpts :: [String] -> (Int, Maybe FilePath)
procOpts argv = case getOpt Permute options argv of
  (o, n, [])   -> (last (1:o), foldr (const . Just) Nothing n)
  (_, _, errs) -> error (concat errs ++ usageInfo header options)
  where
    header = "Usage: HiBX [OPTION...] file"

synthesis :: FilePath -> IO ()
synthesis fp = do
  curDir <- getCurrentDirectory
  spec_str <- readFile (curDir </> fp)
  case parseSpec spec_str of
    Bad errs -> putStrLn errs
    Ok (funs_p, root_name, nonRoot_names, examples_p) -> do
      let funs_path = (takeDirectory (curDir </> fp)) </> funs_p
      let examples_path = (takeDirectory (curDir </> fp)) </> examples_p
      putStrLn $ "#funs = " ++ show funs_path
      putStrLn $ "#root = " ++ show root_name
      putStrLn $ "#nonRoots = " ++ show nonRoot_names
      putStrLn $ "#examples = " ++ show examples_path
      putStrLn ""
      startREPL_ verbosity funs_path root_name nonRoot_names examples_path
  where
    verbosity = 1
