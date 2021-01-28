{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Synthesis.GenExp where

import           Control.Applicative (Alternative((<|>), empty))
import           Control.Monad (guard, MonadPlus(mzero))
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Sharing
import           Data.Monadic.List
import           Data.Monadic.Util (UniqSupply)
import           DataDecl (DataEnv)
import qualified DataDecl as D
import           Name
import           Syntax.Type as T
import           Synthesis.BxFrame (convertSnEnvToMTEnv)
import           Synthesis.BxTyping as BT
import qualified Synthesis.LazyEnv as LE
import           Synthesis.LazyExp
import qualified Synthesis.LazySetOrd as LS
import qualified Synthesis.PureExpression as P
import qualified Typing as TP
import qualified Data.Set as S
import qualified EnvLike as EL
import qualified Debug.Trace as DT
import           Data.Char (chr, ord, toUpper)

-- import           Debug.Trace
data Env m =
  Env { denv :: DataEnv
      , depth :: Int
      , used :: S.Set Name
      , pen :: Int
      , loc :: Loc
      , tyenv :: m (MTyEnv m)
      , vs :: m (TyVarSet m)
        -- , strEnv :: [String]
        -- , charEnv :: [Char]
      , boolFuncion :: Maybe BoolFunction
      , canUseAND :: Bool
      , canUseOR :: Bool
      , canUseNOT :: Bool
      }

initGenEnv :: (MonadPlus m, Sharing m)
           => DataEnv
           -> Int
           -> Loc
           -> m (MTyEnv m)
           -> m (TyVarSet m)
           -> Bool
           -> Bool
           -> Bool
           -> m (Env m)
initGenEnv
  denv
  pen
  loc
  tyenv_
  vs_
  -- strEnv_
  -- charEnv_
  canUseAND
  canUseOR
  canUseNOT = do
  tyenv <- share tyenv_
  vs <- share vs_
  return
    $ Env { denv = denv
          , used = S.empty
          , pen = pen
          , loc = loc
          , tyenv = tyenv
          , vs = vs
            -- , charEnv = head (filter (`notElem` charEnv_) ['a' .. 'z']):charEnv_
            -- , strEnv = strEnv_
          , boolFuncion = Nothing
          , canUseAND = canUseAND
          , canUseOR = canUseOR
          , canUseNOT = canUseNOT
          , depth = 0
          }

-- type Loc = Int
type Depth = Int

data MExpTy m = MExpTy (m (MExp m)) (m (MTy m))

type TyVarSet m = LS.Set m TyVar

newtype Loc = Loc Int

type UsedNames m = LS.Set m Name

data BoolFunction = AND
                  | OR
                  | NOT

weight :: Int
weight = 100

instance Monad m => Shareable m (MExpTy m) where
  shareArgs f (MExpTy e ty) = do
    e' <- f e
    ty' <- f ty
    return $ MExpTy e' ty'

-- test
genExpTest
  :: TP.TyEnv -> TP.Synonyms -> DataEnv -> UniqSupply -> T.Ty -> IO [P.Exp]
genExpTest tyenv syn denv ref ty = do
  putStrLn $ "Generate terms whose type are " ++ show ty
  putStrLn $ "Env: " ++ show tyenv
  resultListIO
    ((do
        ty' <- share $ convert ty
        env <- share
          $ convertSnEnvToMTEnv (EL.delete (Name "orElse") tyenv) denv
        ty'' <- ty'
        polyGenExp denv 20 (Loc 0) env LS.empty ty'' True True True)
     >>= convert)

genExpTest3
  :: TP.TyEnv -> TP.Synonyms -> DataEnv -> UniqSupply -> T.Ty -> IO [P.Exp]
genExpTest3 tyenv syn denv ref ty = do
  putStrLn $ "Generate terms whose type are " ++ show ty
  putStrLn $ "Env: " ++ show tyenv
  resultListIO
    ((do
        ty' <- share $ convert ty
        env <- share
          $ convertSnEnvToMTEnv (EL.delete (Name "orElse") tyenv) denv
        ty'' <- ty'
        let e = polyGenExp denv 1000 (Loc 0) env LS.empty ty'' True True True
        return $ MECon False (return False) (return $ CName ",") (consN 20 e))
        --polyGenExp denv 20 (Loc 0) env LS.empty ty'' True True True)
     >>= convert)
  where
    consN :: Monad m => Int -> m a -> m (List m a)
    consN n a = if n == 0
                then nil
                else cons a (consN (n - 1) a)

instance (Monad m, Convertible m a b)
  => Convertible m (m a, m a, m a) (b, b, b) where
  convert (ma, mb, mc) = do
    a <- ma
    b <- mb
    c <- mc
    a' <- convert a
    b' <- convert b
    c' <- convert c
    return (a', b', c')

-- test
genTypeTest :: TP.TyEnv -> TP.Synonyms -> DataEnv -> UniqSupply -> IO [Ty]
genTypeTest tyenv syn denv ref = do
  putStrLn "Generate typs"
  resultListIO
    ((do
        env <- share $ convertSnEnvToMTEnv tyenv denv
        genType 20 denv empty)
     >>= convert)

genType :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
        => Int
        -> DataEnv
        -> m (TyVarSet m) -- もし使いたい型変数があったら使える
        -> m (MTy m)
genType pen denv vars = (do
                           ma <- share $ LS.select vars
                           MTyVar <$> ma)
  <|> foldr
    (\(D.TyData name abc _) ty -> return
       (MTyApp
          False
          name
          (mapToML (\a -> delay pen $ genType (pen * weight) denv vars) abc))
     <|> ty)
    mzero
    denv
  <|> (do
         n <- aux pen 0
         guard (n /= 1)
         let ts = delay pen
               $ mapToML
                 (\_ -> genType (pen * weight) denv vars)
                 (replicate n 0)
         return $ MTyApp False (TName "(,..,)") ts)
  <|> (do
         let ty1 = delay pen $ genType (pen * weight * weight) denv vars -- あまり -> を生成することは無いと思う
         let ty2 = delay pen $ genType (pen * weight * weight) denv vars
         return $ MTyApp False (TName "->") (cons ty1 (cons ty2 nil)))
  where
    aux pen n = return n <|> delay pen (aux (pen * weight) (n + 1))

mapToML :: Monad m => (a -> m b) -> [a] -> m (List m b)
mapToML f = foldr (cons . f) nil

polyGenExp :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
           => DataEnv
           -> Int
           -> Loc
           -> m (MTyEnv m) -- share禁止
           -> m (TyVarSet m) -- 基本的にはmzeroで呼ぶ
           -> MTy m -- forallが先頭, shareされている前提
           -> Bool
           -> Bool
           -> Bool
           -> m (MExp m)
polyGenExp
  denv
  pen
  loc
  tyenv
  vs
  (MTyForAll _ abc ty)
  canUseAND
  canUseOR
  canUseNOT = do
  genEnv <- initGenEnv denv pen loc tyenv vs canUseAND canUseOR canUseNOT
  genExp genEnv =<< ty

genExp :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
       => Env m
       -> MTy m -- forallなし, 先頭は->でない
       -> m (MExp m)
genExp env reqty   -- DT.trace ("debug: depth = " ++ show (depth env))
  = do
    let env' = env   -- pen = pen env * weight
          { boolFuncion = Nothing, depth = 1 + depth env }
    case reqty of
      MTyApp _ (TName "Bool") _
        -> (case boolFuncion env of
              Nothing -> return
                (MECon False (return False) (return (CName "True")) nil)
              _       -> mzero)
        <|> (genExpBoolLiteral env' <|> genExpOr env' <|> genExpAnd env')
      MTyApp _ (TName "Int") _ -> return (MENum False (return 0))
        <|> genExp_ env' reqty
      MTyApp _ (TName "Char") _ -> return (MEChar False chars)
        <|> genExp_ env' reqty
        where
          chars = chars_ (ord 'a') (pen env)

          chars_ i p =
            if i > ord 'z'
            then mzero
            else return (chr i)
              <|> delay p (return (toUpper (chr i)))
              <|> delay (p * weight) (chars_ (i + 1) (p * weight * weight))
                                                  -- where
        --   chars = foldr (\c cs -> return c <|> cs) mzero (['a' .. 'z']++['A'..'Z'])
      -- MTyApp _ (TName "Char") _ -> return (MEChar False chars)
      --   <|> genExp_ env' reqty
      --   where
      --     chars = foldr (\c cs -> return c <|> cs) mzero (charEnv env)
      -- MTyApp _ (TName "[]") mreqty' -> do
      --   b <- isChar mreqty'
      --   if b
      --     then strs (strEnv env) <|> genExp_ env' reqty
      --     else genExp_ env' reqty
      --   where
      --     isChar :: Monad m => m (List m (MTy m)) -> m Bool
      --     isChar ml = do
      --       l <- ml
      --       case l of
      --         Nil        -> return False
      --         Cons mty _ -> do
      --           ty <- mty
      --           case ty of
      --             MTyApp _ (TName "Char") _ -> return True
      --             _ -> return False
          -- strs :: MonadPlus m => [String] -> m (MExp m)
          -- strs [] = mzero
          -- strs (s:ss) = convertString s <|> strs ss
          -- convertString :: MonadPlus m => String -> m (MExp m)
          -- convertString [] = return
          --   $ MECon False (return False) (return NNil) nil
          -- convertString (c:cs) = return
          --   $ MECon
          --     False
          --     (return False)
          --     (return NCons)
          --     (cons
          --        (return (MEChar False (return c)))
          --        (cons (convertString cs) nil))
          --      -- case ty of
      _ -> genExp_ env' reqty

genExp_ :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
        => Env m
        -> MTy m -- forallなし
        -> m (MExp m)
genExp_ env_ reqty =
  let bool_funciton = boolFuncion env_ -- 一旦退避
      env = env_ { boolFuncion = Nothing }
  in case reqty of
       MTyApp _ (TName "->") ty1ty2 -> do
         (ty1, ty2) <- split2 ty1ty2
         let Loc loc' = loc env
         let x = SName loc'
         tyenv' <- share $ LE.insert x ty1 (tyenv env)
         vs' <- share $ collectVar ty1 (vs env)
         let env' = env { loc = Loc (loc' + 1)
                        , tyenv = tyenv'
                        , vs = vs'
                        , pen = pen env * weight
                        }
         let e1 = delay (pen env) $ genExp env' =<< ty2
         return $ MEAbs False (return x) e1
         where
           collectVar :: (Monad m, Sharing m, Delay m)
                      => m (MTy m)
                      -> m (TyVarSet m)
                      -> m (TyVarSet m)
           collectVar mty vs = do
             ty <- mty
             case ty of
               MTyApp _ _ l -> foldrML collectVar vs l
               MTyVar x     -> LS.add x vs
       MTyApp _ (TName "(,..,)") tys
         -> (do
               let es = mapML
                     (\ty -> delay (pen env)
                      $ genExp env { pen = pen env * weight } =<< ty)
                     tys
               return $ MECon False (return False) (return NTup) es)
         <|> genExp' env reqty
       MTyApp _ name tys -> genExp' (env { boolFuncion = bool_funciton }) reqty
         <|> case (name, bool_funciton) of
           (TName "Bool", Just _) -> mzero -- NOT, OR, ANDの直下では True，Falseは使わない
           (TName "Bool", Nothing)    -- return
             ->
             --(MECon (return False) (return (CName "True")) nil)
             -- <|>
             return (MECon False (return False) (return (CName "False")) nil)
           (_, Nothing) -> delay
             (pen env)
             (case D.lookup name (denv env) of
                Nothing -> mzero
                Just (D.TyData name vars consts) -> do
                  let sig = map2ML (curry return) vars tys
                  foldr
                    (\const e -> go env (subst sig . convert) const <|> e)
                    mzero
                    consts)
         where
           subst :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
                 => m (List m (TyVar, m (MTy m)))
                 -> m (MTy m)
                 -> m (MTy m)
           subst env mty = do
             ty <- mty
             case ty of
               MTyApp _ tname l
                 -> return $ MTyApp False tname $ mapML (subst env) l
               MTyVar a         -> do
                 look <- LE.lookup' a env
                 case look of
                   Nothing  -> return $ MTyVar a
                   Just ty' -> ty'

           go :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
              => Env m
              -> (Ty -> m (MTy m))
              -> (Name, [Ty])
              -> m (MExp m)
           go env sigma (name, tys) = return
             $ MECon False (return False) (return name)
             $ foldr
               (\ty l -> cons
                  (delay (pen env)
                   $ genExp env { pen = pen env * weight } =<< sigma ty)
                  l)
               nil
               tys
       _ -> genExp' env reqty

split2 :: (MonadFail m) => m (List m a) -> m (m a, m a)
split2 l = do
  Cons a1 rest <- l
  Cons a2 _ <- rest
  return (a1, a2)

-- apply専門
genExp' :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
        => Env m
        -> MTy m -- forallなし, 先頭は->でない
        -> m (MExp m)
genExp' env reqty = do
  (name, ty) <- LE.select (tyenv env)
  ty <- share ty
  let argTys = unifyLast env ty reqty
  let env' = env { used = S.insert name (used env), pen = pen env * weight }
  let args = mapML (\argTy -> delay (pen env) $ genExp env' =<< argTy) argTys
  delay
    (if S.member name (used env)
     then pen env
     else 0)
    $ foldrML
      (\arg e -> return $ MEApp False e arg)
      (return $ MEVar False (return name))
      (reverseML args)
  where
    unifyLast :: (MonadPlus m, Sharing m, MonadFail m, Delay m)
              => Env m
              -> m (MTy m)
              -> MTy m
              -> m (List m (MTy m))
    unifyLast env mpolyTy reqty = do
      (monoTy, abc) <- BT.instantiate_ mpolyTy
      (args, last) <- splitLast =<< (arrowToList =<< monoTy)
      let sig1 = foldr
            (\a sig -> LE.insert a (genType (pen env) (denv env) (vs env)) sig)
            LE.empty
            abc
      maybeSig <- BT.unifyL last (return reqty)
      case maybeSig of
        Nothing   -> mzero
        Just sig' -> do
          sig2 <- share $ LE.insertAll sig' sig1
          mapML (\t -> BT.subst sig2 =<< t) args

    splitLast :: Monad m => List m a -> m (m (List m a), m a)
    splitLast l = case l of
      Cons x mxs -> do
        xs <- mxs
        case xs of
          Nil -> return (nil, x)
          _   -> do
            (xs', last) <- splitLast xs
            return (cons x xs', last)

    arrowToList :: Monad m => MTy m -> m (List m (MTy m))
    arrowToList (MTyApp _ (TName "->") l) = do
      (t1, t2) <- listToPair l
      let t2' = arrowToList =<< t2
      cons t1 t2'
    arrowToList ty = cons (return ty) nil

    listToPair :: Monad m => m (List m a) -> m (m a, m a)
    listToPair ml = do
      l <- ml
      case l of
        Cons a mrest -> do
          rest <- mrest
          case rest of
            Cons b _ -> return (a, b)

genExpAnd
  :: (MonadPlus m, Sharing m, MonadFail m, Delay m) => Env m -> m (MExp m)
genExpAnd env =
  if canUseAND env
  then do
    let env' = env { pen = pen env * weight
                   , boolFuncion = Just AND
                   , depth = 1 + depth env
                   }
    let arg1 = delay (pen env) (genExpBoolLiteral env')
          <|> delay (pen env) (genExpOr env')
          <|> delay (pen env) (genExpAnd env')
    let arg2 = delay (pen env) (genExpBoolLiteral env')
          <|> delay (pen env) (genExpOr env')
    return
      $ MEApp
        False
        (return
         $ MEApp False (return $ MEVar False $ return (Name "andAlso")) arg1)
        arg2
  else mzero

genExpOr
  :: (MonadPlus m, Sharing m, MonadFail m, Delay m) => Env m -> m (MExp m)
genExpOr env =
  if canUseOR env
  then do
    let env' = env { pen = pen env * weight
                   , boolFuncion = Just OR
                   , depth = 1 + depth env
                   }
    let arg1 = delay (pen env) (genExpBoolLiteral env')
          <|> delay (pen env) (genExpOr env')
    let arg2 = delay (pen env) $ genExpBoolLiteral env'
    return
      $ MEApp
        False
        (return
         $ MEApp False (return $ MEVar False $ return (Name "orElse")) arg1)
        arg2
  else mzero

genExpBoolLiteral
  :: (MonadPlus m, Sharing m, MonadFail m, Delay m) => Env m -> m (MExp m)
genExpBoolLiteral env =
  if canUseNOT env
  then do
    let env' = env { pen = pen env * weight
                   , boolFuncion = Just NOT
                   , depth = 1 + depth env
                   }
    let literal = genExp_ env (MTyApp False (TName "Bool") nil)
    let not_literal = delay (pen env)
          $ return
          $ MEApp
            False
            (return $ MEVar False $ return (Name "not"))
            (genExp_ env' (MTyApp False (TName "Bool") nil))
    literal <|> not_literal
  else genExp_ env (MTyApp False (TName "Bool") nil)
