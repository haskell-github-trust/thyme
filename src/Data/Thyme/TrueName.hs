{-# LANGUAGE CPP #-}

-- | Refer to <https://github.com/liyang/true-name/blob/master/test/sanity.hs these examples>.

module Data.Thyme.TrueName (summon, truename) where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import Data.List (nub)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

conNames :: Con -> [Name]{- {{{ -}
conNames con = case con of
    NormalC name _ -> [name]
    RecC name vbts -> name : [ fname | (fname, _, _) <- vbts ]
    InfixC _ name _ -> [name]
    ForallC _ _ con' -> conNames con'

#if MIN_VERSION_template_haskell(2,11,0)
    GadtC names _ typ -> names ++ typNames typ
    RecGadtC names vbts typ -> names ++ typNames typ
         ++ [ fname | (fname, _, _) <- vbts]
#endif
{- }}} -}

decNames :: Dec -> [Name]{- {{{ -}
decNames dec = case dec of
    FunD _ _ -> []
    ValD _ _ _ -> []
    TySynD _ _ typ -> typNames typ
    ClassD _ _ _ _ decs -> decNames =<< decs
#if MIN_VERSION_template_haskell(2,11,0)
    InstanceD _ cxt typ decs ->
#else
    InstanceD cxt typ decs ->
#endif
        (predNames =<< cxt) ++ typNames typ ++ (decNames =<< decs)
    SigD name typ -> name : typNames typ

#if MIN_VERSION_template_haskell(2,16,0)
    KiSigD name kind -> name : typNames kind
#endif

    ForeignD frgn -> case frgn of
        ImportF _ _ _ name t -> name : typNames t
        ExportF _ _ name t -> name : typNames t
    PragmaD _ -> []

#if MIN_VERSION_template_haskell(2,11,0)
    DataD _ _ _ _ cons _ -> conNames =<< cons
    NewtypeD _ _ _ _ con _ -> conNames con
#else
    DataD _ _ _ cons _ -> conNames =<< cons
    NewtypeD _ _ _ con _ -> conNames con
#endif

#if MIN_VERSION_template_haskell(2,12,0)
    PatSynD _name _args _dir _pat -> []
    PatSynSigD _name typ -> typNames typ
#endif

#if MIN_VERSION_template_haskell(2,22,0)
    InfixD _ _ _ -> []
#elif MIN_VERSION_template_haskell(2,8,0)
    InfixD _ _ -> []
#endif

#if MIN_VERSION_template_haskell(2,12,0)
    DataInstD cxt _name _typs _kind cons derivs   ->
        datatypeNames cxt cons  ++ derivNames derivs
    NewtypeInstD cxt _name _typs _kind con derivs ->
        datatypeNames cxt [con] ++ derivNames derivs
#elif MIN_VERSION_template_haskell(2,11,0)
    DataInstD cxt _ _ _ cons derivs ->
        datatypeNames cxt cons  ++ (predNames =<< derivs)
    NewtypeInstD cxt _ _ _ con derivs ->
        datatypeNames cxt [con] ++ (predNames =<< derivs)
#else
    DataInstD cxt _ _ cons derivs   -> datatypeNames cxt cons  ++ derivs
    NewtypeInstD cxt _ _ con derivs -> datatypeNames cxt [con] ++ derivs
#endif

#if MIN_VERSION_template_haskell(2,11,0)
    DataFamilyD _ _ _ -> []
    OpenTypeFamilyD _ -> []
#else
    FamilyD _ _ _ _ -> []
#endif

#if MIN_VERSION_template_haskell(2,11,0)
    ClosedTypeFamilyD _ tses -> tseNames =<< tses
#elif MIN_VERSION_template_haskell(2,9,0)
    ClosedTypeFamilyD _ _ _ tses -> tseNames =<< tses
#endif

#if MIN_VERSION_template_haskell(2,15,0)
    TySynInstD tse -> tseNames tse
#elif MIN_VERSION_template_haskell(2,9,0)
    TySynInstD _ tse -> tseNames tse
#else
    TySynInstD _ ts t -> (typNames =<< ts) ++ typNames t
#endif

#if MIN_VERSION_template_haskell(2,9,0)
    RoleAnnotD _ _ -> []
#endif

#if MIN_VERSION_template_haskell(2,12,0)
    StandaloneDerivD _strat cxt typ -> (predNames =<< cxt) ++ typNames typ
#elif MIN_VERSION_template_haskell(2,10,0)
    StandaloneDerivD cxt typ -> (predNames =<< cxt) ++ typNames typ
#endif

#if MIN_VERSION_template_haskell(2,10,0)
    DefaultSigD _ _ -> []
#endif

#if MIN_VERSION_template_haskell(2,15,0)
    ImplicitParamBindD _ _ -> []
#endif

#if MIN_VERSION_template_haskell(2,22,0)
    TypeDataD _ _ _ _ -> []
    DefaultD _ -> []
#endif

{- }}} -}

datatypeNames :: Cxt -> [Con] -> [Name]
datatypeNames cxt cons = (conNames =<< cons) ++ (predNames =<< cxt)

#if MIN_VERSION_template_haskell(2,12,0)
derivNames :: [DerivClause] -> [Name]
derivNames derivs = predNames =<<
    [ p | DerivClause _strat cxt <- derivs, p <- cxt ]
#endif

tseNames :: TySynEqn -> [Name]
#if MIN_VERSION_template_haskell(2,15,0)
tseNames (TySynEqn _ l r) = typNames l ++ typNames r
#elif MIN_VERSION_template_haskell(2,9,0)
tseNames (TySynEqn ts t) = (typNames =<< ts) ++ typNames t
#endif

predNames :: Pred -> [Name]{- {{{ -}
#if MIN_VERSION_template_haskell(2,10,0)
predNames = typNames
#else
predNames p = case p of
    ClassP n ts -> n : (typNames =<< ts)
    EqualP s t -> typNames s ++ typNames t
#endif
{- }}} -}

typNames :: Type -> [Name]{- {{{ -}
typNames typ = case typ of
    ForallT _ c t -> (predNames =<< c) ++ typNames t
    AppT s t -> typNames s ++ typNames t
    SigT t _ -> typNames t
    VarT _ -> []
    ConT name -> [name]
    TupleT _ -> []
    UnboxedTupleT _ -> []
    ArrowT -> []
    ListT -> []

#if MIN_VERSION_template_haskell(2,8,0)
    PromotedT _ -> []
    PromotedTupleT _ -> []
    PromotedNilT -> []
    PromotedConsT -> []
    StarT -> []
    ConstraintT -> []
    LitT _ -> []
#endif

#if MIN_VERSION_template_haskell(2,10,0)
    EqualityT -> []
#endif

#if MIN_VERSION_template_haskell(2,11,0)
    InfixT s n t -> n : typNames s ++ typNames t
    UInfixT s n t -> n : typNames s ++ typNames t
    ParensT t -> typNames t
    WildCardT -> []
#endif

#if MIN_VERSION_template_haskell(2,12,0)
    UnboxedSumT _arity -> []
#endif

#if MIN_VERSION_template_haskell(2,15,0)
    AppKindT k t -> typNames k ++ typNames t
    ImplicitParamT _ t -> typNames t
#endif

#if MIN_VERSION_template_haskell(2,16,0)
    ForallVisT _ t -> typNames t
#endif

#if MIN_VERSION_template_haskell(2,17,0)
    MulArrowT -> []
#endif

#if MIN_VERSION_template_haskell(2,19,0)
    PromotedInfixT s n t -> n : typNames s ++ typNames t
    PromotedUInfixT s n t -> n : typNames s ++ typNames t
#endif
{- }}} -}

infoNames :: Info -> [Name]{- {{{ -}
infoNames info = case info of
    ClassI dec _ -> decNames dec
    TyConI dec -> decNames dec
    FamilyI _ decs -> decNames =<< decs
    PrimTyConI _ _ _ -> []
    TyVarI _ typ -> typNames typ

#if MIN_VERSION_template_haskell(2,11,0)
    ClassOpI _ typ _ -> typNames typ
    DataConI _ typ parent -> parent : typNames typ
    VarI _ typ _ -> typNames typ
#else
    ClassOpI _ typ _ _ -> typNames typ
    DataConI _ typ parent _ -> parent : typNames typ
    VarI _ typ _ _ -> typNames typ
#endif

#if MIN_VERSION_template_haskell(2,12,0)
    PatSynI _name typ -> typNames typ
#endif
{- }}} -}

{- {{{ -}
-- | Summons a 'Name' using @template-haskell@'s 'reify' function.
--
-- The first argument is a 'String' matching the 'Name' we want: either its
-- 'nameBase', or qualified with its module. The second argument gives the
-- 'Name' to 'reify'.
--
-- If no match is found or there is some ambiguity, 'summon' will fail with
-- a list of 'Name's found, along with the output of 'reify' for reference.
--
-- Suppose we are given a module @M@ that exports a function @s@, but not
-- the type @T@, the constrcutor @C@, nor the field @f@:
--
-- > module M (s) where
-- > newtype T = C { f :: Int }
-- > s :: T -> T
-- > s = C . succ . f
--
-- In our own module we have no legitimate way of passing @s@ an argument of
-- type @T@. We can get around this in a type-safe way with 'summon':
--
-- >{-# LANGUAGE TemplateHaskell #-}
-- >module Main where
-- >import Language.Haskell.TH.Syntax
-- >import Unsafe.TrueName
-- >import M
-- >
-- >type T = $(fmap ConT $ summon "T" 's)
-- >mkC :: Int -> T; unC :: T -> Int; f :: T -> Int
-- >mkC = $(fmap ConE $ summon "C" =<< summon "T" 's)
-- >unC $(fmap (`ConP` [VarP $ mkName "n"]) $ summon "C" =<< summon "T" 's) = n
-- >f = $(fmap VarE $ summon "f" =<< summon "T" 's)
-- >
-- >main :: IO ()
-- >main = print (unC t, n) where
-- >    t = s (mkC 42 :: T)
-- >    n = f (s t)
--
-- Note that 'summon' cannot obtain the 'Name' for an unexported function,
-- since GHC <http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:VarI does not currently return the RHS of function definitons>.
-- The only workaround is to copypasta the definition. D:
{- }}} -}
summon :: String -> Name -> Q Name{- {{{ -}
summon name thing = do
    info <- reify thing
    let ns = nub (infoNames info)
    case filter (\ n -> name == nameBase n || name == show n) ns of
        [n] -> return n
        _ -> fail $ "summon: you wanted " ++ show name ++ ", but I have:\n"
            ++ unlines ((++) "        " . namespace <$> ns)
            ++ "    reify " ++ show thing ++ " returned:\n"
            ++ show (nest 8 $ ppr info)
  where
    namespace n@(Name _ flavour) = show n ++ case flavour of
        NameG VarName _ _ -> " (var)"
        NameG DataName _ _ -> " (cons)"
        NameG TcClsName _ _ -> " (type)"
        _ -> " (?)"
{- }}} -}

{- {{{ -}
-- | A more convenient 'QuasiQuoter' interface to 'summon'.
--
-- The first space-delimited token gives the initial 'Name' passed to
-- 'summon': it must be ‘quoted’ with a @'@ or @''@ prefix to indicate
-- whether it should be interpreted in an expression or a type context,
-- as per <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/template-haskell.html#th-syntax the usual TH syntax>.
-- Subsequent tokens correspond to the 'String' argument of 'summon', and
-- are iterated over. Thus
--
-- > [truename| ''A B C D |]
--
-- is roughly equivalent to:
--
-- > summon "D" =<< summon "C" =<< summon "B" ''A
--
-- but with the resulting 'Name' wrapped up in 'ConE', 'VarE', 'ConP', or
-- 'ConT', depending on the context. (There is no 'quoteDec'.)
--
-- Variable bindings are given after a @|@ token in a 'Pat' context:
--
-- > [truename| ''Chan Chan | chanR chanW |] <- newChan
--
-- These may be prefixed with @!@ or @~@ to give the usual semantics.
-- A single @..@ token invokes @RecordWildCards@ in 'Pat' contexts, and for
-- record construction in 'Exp' contexts.
-- Nested or more exotic patterns are not supported.
--
-- With this, the example from 'summon' may be more succinctly written:
--
-- >{-# LANGUAGE QuasiQuotes #-}
-- >module Main where
-- >import Unsafe.TrueName
-- >import M
-- >
-- >type T = [truename| 's T |]
-- >mkC :: Int -> T; unC :: T -> Int; f :: T -> Int
-- >mkC = [truename| 's T C |]
-- >unC [truename| 's T C | n |] = n
-- >f = [truename| 's T f |]
-- >
-- >main :: IO ()
-- >main = print (unC t, n) where
-- >    t = s (mkC 42 :: T)
-- >    n = f (s t)
{- }}} -}
truename :: QuasiQuoter{- {{{ -}
truename = QuasiQuoter
    { quoteExp = makeE <=< nameVars
    , quotePat = makeP <=< nameVars
    , quoteType = makeT <=< nameVars
    , quoteDec = \ _ -> err "I'm not sure how this would work"
    } where
    err = fail . (++) "truename: "
    noPat = err . (++) "unexpected pattern variables: " . unwords

    makeT (name, vars) = ConT name <$ unless (null vars) (noPat vars)
    makeE (name@(Name occ flavour), vars) = case flavour of
        NameG VarName _ _ -> VarE name <$ unless (null vars) (noPat vars)
        NameG DataName _ _ -> case vars of
            [] -> return (ConE name)
            [".."] -> RecConE name . capture VarE <$> recFields name
            _ -> noPat vars
        _ -> err $ occString occ ++ " has a strange flavour"
    makeP (name, vars) = if vars == [".."]
            then RecP name . capture VarP <$> recFields name
            else
#if MIN_VERSION_template_haskell(2,18,0)
              return $ ConP name [] (map pat vars) where
#else
              return $ ConP name (map pat vars) where
#endif
        pat n = case n of
            "_" -> WildP
            '!' : ns -> BangP (pat ns)
            '~' : ns -> TildeP (pat ns)
            _ -> VarP (mkName n)
    capture v = map $ \ f -> (f, v (mkName $ nameBase f))

    recFields :: Name -> Q [Name]
    recFields name = do
        parent <- reify name >>= \ info -> case info of
#if MIN_VERSION_template_haskell(2,11,0)
            DataConI _ _ p -> return p
#else
            DataConI _ _ p _ -> return p
#endif
            _ -> err $ show name ++ " is not a data constructor"
        dec <- reify parent >>= \ info -> case info of
            TyConI d -> return d
            _ -> err $ "parent " ++ show parent ++ " is not a plain type"
        case dec of
#if MIN_VERSION_template_haskell(2,11,0)
            DataD _ _ _ _ cs _ -> return (fields =<< cs)
            NewtypeD _ _ _ _ c _ -> return (fields c)
#else
            DataD _ _ _ cs _ -> return (fields =<< cs)
            NewtypeD _ _ _ c _ -> return (fields c)
#endif
            _ -> err $ "parent " ++ show parent ++ " neither data nor newtype"
      where
        fields :: Con -> [Name]
        fields con = case con of
            NormalC _ _ -> []
            RecC n vbts -> if n /= name then [] else [ v | (v, _, _) <- vbts ]
            InfixC _ _ _ -> []
            ForallC _ _ c -> fields c
#if MIN_VERSION_template_haskell(2,11,0)
            GadtC _ _ _ -> []
            RecGadtC ns vbts _ -> if name `notElem` ns then []
                else [ v | (v, _, _) <- vbts ]
#endif

    lookupThing :: String -> Q Name
    lookupThing s0 = case s0 of
        '\'' : s1 -> case s1 of
            '\'' : s2 -> hmm s2 "lookupTypeName" =<< lookupTypeName s2
            _ -> hmm s1 "lookupValueName" =<< lookupValueName s1
        _ -> err $ "please specify either '" ++ s0 ++ " or ''" ++ s0
      where
        hmm s l = maybe (err $ unwords [l, show s, "failed"]) return

    nameVars :: String -> Q (Name, [String])
    nameVars spec = case words spec of
        [] -> err "expecting at least one token"
        start : rest -> do
            thing <- lookupThing start
            let (names, vars) = break ("|" ==) rest
            name <- foldM (flip summon) thing names
            return (name, dropWhile ("|" ==) vars)
{- }}} -}
