{-# LANGUAGE LambdaCase #-}

module Main where

import Conf
import ParABS (myLexer, pProgram)
import qualified AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Pretty (prettyPrint)
import ErrM

import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), replaceExtension)
import Control.Monad (when, liftM)
import Data.List (intersperse, nub, union, findIndices, (\\))
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

import qualified Data.Map as M

data ModuleST = ModuleST {
      filePath :: FilePath,
      moduleName :: ABS.QualType,
      hierarchy :: M.Map ABS.TypeIdent [ABS.QualType], -- Interface -> Extends
      methods :: M.Map ABS.TypeIdent [ABS.Ident]       -- Interface -> Methods
    } deriving (Show)


parseABSFiles :: FilePath -> IO [(FilePath, ABS.Program)]
parseABSFiles fileOrDir = do
  isdir <- doesDirectoryExist fileOrDir
  if isdir
    then do
      -- TODO: goes only 1 level deep in the directory, maybe FIX later
      contents <- getDirectoryContents fileOrDir
      let absFiles = filter (isSuffixOf ".abs") contents
      mapM (\ relativeFile -> parseABSFile (fileOrDir </> relativeFile)) absFiles
    else liftM return $ parseABSFile fileOrDir

parseABSFile :: FilePath -> IO (FilePath, ABS.Program)
parseABSFile absFilePath = do
  isfile <- doesFileExist absFilePath
  when (not isfile) $ error "ABS file does not exist"
  absSource <- readFile absFilePath
  let parseABS = pProgram $ myLexer absSource
  case parseABS of
    Ok res -> do
      writeFile (replaceExtension absFilePath ".ast") (show  res)
      return (absFilePath, res)
    Bad _errorString -> error "Error in parsing" -- TODO: move to exceptions

main :: IO ()
main = do
  let Conf {files = inputFilesOrDirs} = conf
  asts <- liftM concat $ mapM parseABSFiles inputFilesOrDirs
  let 
    symbolTable :: [ModuleST]
    symbolTable = map collectInterfs asts

    collectInterfs :: (FilePath, ABS.Program) -> ModuleST
    collectInterfs (fp , (ABS.Prog (ABS.ModuleDecl mName _ _ decls _))) = ModuleST {
                                                                        filePath = fp,
                                                                        moduleName = mName,
                                                                        hierarchy = foldl insertInterfs M.empty decls,
                                                                        methods = foldl insertMethods M.empty decls
                                                                      }
                where 
                  insertInterfs :: M.Map ABS.TypeIdent [ABS.QualType] -> ABS.Decl -> M.Map ABS.TypeIdent [ABS.QualType]
                  insertInterfs acc (ABS.ExtendsDecl tident extends msigs) = M.insertWith (const $ const $ error "duplicate interface declaration") tident extends acc
                  insertInterfs acc _ = acc

                  insertMethods :: M.Map ABS.TypeIdent [ABS.Ident] -> ABS.Decl -> M.Map ABS.TypeIdent [ABS.Ident]
                  insertMethods acc (ABS.InterfDecl tident msigs) = insertMethods acc (ABS.ExtendsDecl tident [] msigs)  -- normalization
                  insertMethods acc (ABS.ExtendsDecl tident extends msigs) = 
                      {- TODO it could generate a compilation error because of duplicate method declaration -}
                      M.insertWith (const $ const $ error "duplicate interface declaration") tident (collectMethods msigs) acc
                  insertMethods acc _ = acc
                  collectMethods :: [ABS.MethSig] -> [ABS.Ident]
                  collectMethods = map (\ (ABS.MethSig _ ident _) -> ident)

    prettyProgram (absFilePath, translation) = do
         let haskellFilePath = replaceExtension absFilePath ".hs"
         writeFile haskellFilePath (prettyPrint translation)

    tProgram :: (FilePath, ABS.Program) -> (FilePath, HS.Module)
    tProgram (absFilePath, (ABS.Prog (ABS.ModuleDecl moduleName exports imports decls maybeMain))) = (absFilePath,
               HS.Module noLoc (if (absFilePath == main_is conf)
                                then HS.ModuleName "Main"
                                else tModuleName moduleName) 
                     [HS.LanguagePragma noLoc [HS.Ident "Rank2Types", 
                                               HS.Ident "NoImplicitPrelude",
                                               -- HS.Ident "ImpredicativeTypes",
                                               -- HS.Ident "LiberalTypeSynonyms",
                                               HS.Ident "ExistentialQuantification",
                                               HS.Ident "MultiParamTypeClasses"
                                                 
                     ]] 
                     Nothing 
                     Nothing 
                     [
                      HS.ImportDecl {HS.importLoc = noLoc, HS.importModule = HS.ModuleName "Control.Monad.Trans.RWS", HS.importQualified = True, HS.importSrc = False, HS.importPkg = Nothing, HS.importAs = Just (HS.ModuleName "RWS"), HS.importSpecs = Nothing},
                      HS.ImportDecl {HS.importLoc = noLoc, 
                                     HS.importModule = HS.ModuleName "Prim", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
                                     HS.importSpecs = Nothing
                                    },
                      HS.ImportDecl {HS.importLoc = noLoc, 
                                     HS.importModule = HS.ModuleName "ABSPrelude", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
                                     HS.importSpecs = Nothing
                                    }] 
                     (tDecls decls ++ (tMain maybeMain))
                                                                                                     )
    tDecls :: [ABS.Decl] -> [HS.Decl]
    tDecls = concatMap tDecl 

    -- can return more than 1 decl, because of creating accessors for records
    -- or putting type signatures
    tDecl :: ABS.Decl -> [HS.Decl]

    -- TODO pass the type variables env , change tType to tTypeOrTyVar
    tDecl (ABS.TypeDecl (ABS.TypeIdent tid) typ) = [HS.TypeDecl noLoc (HS.Ident tid) [{- TODO: type variables lhs -}] (tType typ)]

    tDecl (ABS.DataDecl tid constrs) =  tDecl (ABS.ParDataDecl tid [] constrs) -- just parametric datatype with empty list of type variables

    tDecl (ABS.ParDataDecl (ABS.TypeIdent tid) tyvars constrs) =
        -- create the data declaration
        HS.DataDecl noLoc HS.DataType [] (HS.Ident tid) (map (\ (ABS.TypeIdent varid) -> HS.UnkindedVar $ HS.Ident $ headToLower $  varid) tyvars)
           (map (\case
                 ABS.UnaryConstr (ABS.TypeIdent cid) -> HS.QualConDecl noLoc [] [] (HS.ConDecl (HS.Ident cid) []) -- no constructor arguments
                 ABS.MultConstr (ABS.TypeIdent cid) args -> HS.QualConDecl noLoc [] [] (HS.ConDecl (HS.Ident cid) (map (HS.UnBangedTy . tTypeOrTyVar tyvars . typOfConstrType) args))) constrs)
           [(HS.UnQual $ HS.Ident $ "Eq", [])]
           :
        -- create record accessors
        map (\ (ABS.Ident fname, consname, idx, len) ->  HS.FunBind [HS.Match noLoc (HS.Ident fname) ([HS.PApp (HS.UnQual (HS.Ident consname)) (replicate idx HS.PWildCard ++ [HS.PVar (HS.Ident "a")] ++ replicate (len - idx - 1) HS.PWildCard)]) Nothing (HS.UnGuardedRhs (HS.Var (HS.UnQual (HS.Ident "a")))) (HS.BDecls [])]) (
             concatMap (\case
               ABS.UnaryConstr _ -> []
               ABS.MultConstr (ABS.TypeIdent cid) args -> -- taking the indices of fields
                                         let len = length args
                                         in
                                            foldl (\ acc (field, idx) ->  case field of
                                                                            ABS.EmptyConstrType _ -> acc
                                                                            ABS.RecordConstrType _ fid -> (fid, cid, idx, len):acc) [] (zip args [0..])
              ) constrs
                                                                                                                                                                                                                                                                                                                                  )

    -- empty interface extends automatically from Object
    tDecl (ABS.InterfDecl tid ms) = tDecl (ABS.ExtendsDecl tid [ABS.QualType $ [ABS.QualTypeIdent $ ABS.TypeIdent "Object_"]]  ms) 

    tDecl (ABS.ExtendsDecl (ABS.TypeIdent tname) extends ms) = HS.ClassDecl 
                                                              noLoc 
                                                              (map (\ (ABS.QualType e) -> HS.ClassA (HS.UnQual $ HS.Ident $ joinQualTypeIds e ++ "_") [HS.TyVar (HS.Ident "a")]) extends)
                                                              (HS.Ident $ tname ++ "_") 
                                                              [HS.UnkindedVar (HS.Ident "a")]
                                                              [] -- no fundeps
                                                              (map (tMethodSig tname) ms)
       : 
        -- type synonym for Objects typed by the interface
        -- data Interf1 = forall a. Interf1_ a => Interf1 (ObjectRef a)

        HS.DataDecl noLoc HS.DataType [] (HS.Ident tname) [] [HS.QualConDecl noLoc [HS.UnkindedVar $ HS.Ident "a"] [HS.ClassA (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyVar (HS.Ident "a")]] (HS.ConDecl (HS.Ident tname) [HS.UnBangedTy (HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyVar $ HS.Ident "a"))])] []
                                                              
       : generateSubSelf tname
       : generateSub tname (ABS.QualType [ABS.QualTypeIdent $ ABS.TypeIdent "AnyObject"]) -- root class
       : generateSubs tname (filter (\ (ABS.QualType qids) -> qids /= [ABS.QualTypeIdent $ ABS.TypeIdent "Object_"])  extends) 

       ++ (concatMap (\ (ABS.MethSig _ (ABS.Ident mid) pars) -> let 
                          parspvars = map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar $ HS.Ident pid) pars
                          parsvars = map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.Var $ HS.UnQual $ HS.Ident pid) pars
                     in
                                                                 [
                -- the sync call for each method: method1_sync
                HS.FunBind [HS.Match noLoc (HS.Ident $ mid ++ "_sync" ) (parspvars ++ [HS.PParen (HS.PAsPat (HS.Ident "__wrapped") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard]))])))]) Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__hereCOG")) (HS.Var (HS.UnQual (HS.Ident "thisCOG"))),HS.Generator noLoc (HS.PVar (HS.Ident "__obj1")) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "readIORef"))) (HS.Var (HS.UnQual (HS.Ident "__ioref")))))))),HS.Generator noLoc (HS.PVar (HS.Ident "otherCOG")) (HS.App (HS.Var (HS.UnQual (HS.Ident "whereis"))) (HS.Var (HS.UnQual (HS.Ident "__obj1")))),HS.Qualifier (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "when"))) (HS.Paren (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__hereCOG"))) (HS.QVarOp (HS.UnQual (HS.Symbol "/="))) (HS.Var (HS.UnQual (HS.Ident "otherCOG")))))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "error"))) (HS.Lit (HS.String "Sync Call on a different COG detected"))))),HS.Qualifier (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "mapMonad"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "withReaderT"))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (foldl HS.App (HS.Var (HS.UnQual (HS.Ident mid))) parsvars) (HS.Var (HS.UnQual (HS.Ident "__wrapped"))))))])) (HS.BDecls [])]
                -- the async call for each method: method1_async
              , HS.FunBind [HS.Match noLoc (HS.Ident $ headToLower mid ++ "_async") (parspvars ++ [HS.PParen (HS.PAsPat (HS.Ident "__wrapped") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard]))])))]) Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__obj1")) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "readIORef"))) (HS.Var (HS.UnQual (HS.Ident "__ioref")))))))),HS.Generator noLoc (HS.PVar (HS.Ident "__loc")) (HS.App (HS.Var (HS.UnQual (HS.Ident "whereis"))) (HS.Var (HS.UnQual (HS.Ident "__obj1")))),HS.Generator noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.UnQual (HS.Ident "newEmptyMVar")))))),HS.Generator noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aThread")) (HS.PVar (HS.Ident "__tid")),HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PVar (HS.Ident "cog"))]) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "ask")))),HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "get")))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "put"))) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__tid")))) (HS.Var (HS.UnQual (HS.Ident "cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "writeChan"))) (HS.Var (HS.UnQual (HS.Ident "__loc")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (foldl HS.App (HS.Var (HS.UnQual (HS.Ident mid))) parsvars) (HS.Var (HS.UnQual (HS.Ident "__wrapped")))))))))))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Var (HS.UnQual (HS.Ident "__f"))))])) (HS.BDecls [])]
                      ]
       ) ms)


    tDecl (ABS.Fun fReturnTyp fid params body) = tDecl (ABS.ParFun fReturnTyp fid [] params body) -- no type variables

    tDecl (ABS.ParFun fReturnTyp (ABS.Ident fid) tyvars params body) = 
       -- adds an explicit type signature
       -- commented out, will be inferred
       [-- HS.TypeSig noLoc [HS.Ident fid] (foldr
       --                                (\ tpar acc -> HS.TyFun (tTypeOrTyVar tyvars tpar) acc)
       --                                (tTypeOrTyVar tyvars fReturnTyp)
       --                                (map (\ (ABS.Par typ _) -> typ) params))
       -- ,
        HS.FunBind [HS.Match noLoc (HS.Ident fid) (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) params)  Nothing (tBody body)  (HS.BDecls [])]
       ]

    -- normalizing class declarations
    tDecl (ABS.ClassDecl tident fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] [] fdecls maybeBlock mdecls)
    tDecl (ABS.ClassParamDecl tident params fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident params [] fdecls maybeBlock mdecls)
    tDecl (ABS.ClassImplements tident imps fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] imps fdecls maybeBlock mdecls)
    tDecl (ABS.ClassParamImplements (ABS.TypeIdent clsName) params imps ldecls maybeBlock rdecls) = -- TODO add check for imps, if a method is not implemented
       -- the record-ADT of the ABS class
        HS.DataDecl noLoc HS.DataType [] (HS.Ident clsName) [] 
              [HS.QualConDecl noLoc [] [] $ HS.RecDecl (HS.Ident clsName) (([HS.Ident $ headToLower clsName ++ "_loc"],
                                                                            -- maybe it should be banged for the fields of the class
                                                                           HS.UnBangedTy (HS.TyForall Nothing [HS.ClassA (HS.UnQual (HS.Ident "Object__")) [HS.TyVar (HS.Ident "o")]] (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual (HS.Ident "ABS"))) (HS.TyVar (HS.Ident "o")))  (HS.TyCon (HS.UnQual (HS.Ident "COG")))))
                                                                           ):map (\ (t,(ABS.Ident i)) -> ([HS.Ident $ headToLower clsName ++ "_" ++ i], HS.UnBangedTy (tType t)))  allFields)]  []
        :

        -- the smart constructor
        HS.FunBind [HS.Match noLoc (HS.Ident $ "__" ++ headToLower clsName)
                    (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) params) Nothing 
                    (HS.UnGuardedRhs $ HS.RecConstr (HS.UnQual $ HS.Ident clsName) 
                           (map (\ (ABS.Par _ (ABS.Ident pid)) -> 
                                     HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ pid) (HS.Var $ HS.UnQual $ HS.Ident pid)) params)
                    )
                    (HS.BDecls [])
                   ]
       :
        -- the Object instance
        HS.InstDecl noLoc [{- empty context for now, may need to fix later -}] 
              (HS.UnQual $ HS.Ident "Object__") -- interface name
              [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName]
              (
               -- the new method
               HS.InsDecl (HS.FunBind [HS.Match noLoc (HS.Ident "new") [HS.PVar $ HS.Ident "__cont"] Nothing
                                       (HS.UnGuardedRhs $ HS.Do (
                                        -- chan <- lift $ lift $ newChan
                                        (HS.Generator noLoc (HS.PVar $ HS.Ident "__chan") $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") (HS.Var $ HS.UnQual $ HS.Ident "newChan"))
                                        :
                                        -- let __field = initialized_value
                                        fieldInits
                                        ++
                                        -- update the passed class ADT
                                        -- let __c = cont { class1_field1 = __field1, ..., class1_loc = (return (R __chan)) }
                                        [HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__c") Nothing 
                                                                   (HS.UnGuardedRhs $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "__cont")
                                                                      (foldr (\ fdecl acc -> (case fdecl of
                                                                                              ABS.FieldDeclAss _t (ABS.Ident fid) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
                                                                                              ABS.FieldDecl _t (ABS.Ident fid) ->  
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
                                                                                              ABS.MethDecl _ _ _ _ ->  (case maybeBlock of
                                                                                                                         ABS.NoBlock -> acc
                                                                                                                         ABS.JustBlock _ ->  error "Second parsing error: Syntactic error, no method declaration accepted here")
                                                                             )) 
                                                                       -- class1_loc = (return (R __chan))
                                                                       [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                                                          (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                                                                             (HS.Var $ HS.UnQual $ HS.Ident "__chan"))]
                                                                       ldecls)) (HS.BDecls [])]
                                         -- __ioref <- lift $ lift $ newIORef __c
                                        , HS.Generator noLoc (HS.PVar $ HS.Ident "__ioref") $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") $
                                                           (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift")
                                                                  (HS.App (HS.Var $ HS.UnQual $ HS.Ident "newIORef")
                                                                         (HS.Var $ HS.UnQual $ HS.Ident "__c")))
                                        -- let __obj = ObjectRef __ioref 0
                                        , HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__obj") Nothing 
                                                                  (HS.UnGuardedRhs (HS.App 
                                                                                          (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ObjectRef")
                                                                                                 (HS.Var $ HS.UnQual $ HS.Ident "__ioref"))
                                                                                           (HS.Lit $ HS.Int 0))) (HS.BDecls [])]
                                        -- lift $ lift $ spawnCOG __chan
                                        , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift")
                                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "spawnCOG") (HS.Var $ HS.UnQual $ HS.Ident "__chan"))

                                        -- init_async __obj
                                        , HS.Qualifier (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.UnQual (HS.Ident "newEmptyMVar")))))),HS.Generator noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aThread")) (HS.PVar (HS.Ident "__tid")),HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PVar (HS.Ident "cog"))]) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "ask")))),HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "get")))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "put"))) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__tid")))) (HS.Var (HS.UnQual (HS.Ident "cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "writeChan"))) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__init"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))))))))])

                                        -- run_async __obj
                                        , HS.Qualifier (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.UnQual (HS.Ident "newEmptyMVar")))))),HS.Generator noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aThread")) (HS.PVar (HS.Ident "__tid")),HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PVar (HS.Ident "cog"))]) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "ask")))),HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "get")))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "put"))) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__tid")))) (HS.Var (HS.UnQual (HS.Ident "cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.Paren (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "writeChan"))) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__run"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))))))))])

                                        -- return $ __obj
                                        , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Var $ HS.UnQual $ HS.Ident "__obj")

                                        ])) (HS.BDecls [])])

               :
               -- the new local method
               HS.InsDecl (HS.FunBind [HS.Match noLoc (HS.Ident "new_local") [HS.PVar $ HS.Ident "__cont"] Nothing
                                       (HS.UnGuardedRhs $ HS.Do (
                                         fieldInits
                                         ++

                                         [
                                         -- let __c = cont { class1_field1 = __field1, ..., class1_loc = thisCOG }
                                         HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__c") Nothing 
                                                                   (HS.UnGuardedRhs $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "__cont")
                                                                      (foldr (\ fdecl acc -> (case fdecl of
                                                                                              ABS.FieldDeclAss _t (ABS.Ident fid) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
                                                                                              ABS.FieldDecl _t (ABS.Ident fid) ->  
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc

                                                                                              ABS.MethDecl _ _ _ _ -> (case maybeBlock of
                                                                                                                         ABS.NoBlock -> acc
                                                                                                                         ABS.JustBlock _ ->  error "Second parsing error: Syntactic error, no method declaration accepted here")
                                                                             )) 
                                                                       -- class1_loc = thisCOG)
                                                                       [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                                                          (HS.Var $ HS.UnQual $ HS.Ident "thisCOG")
                                                                             ]
                                                                       ldecls)) (HS.BDecls [])]                                          
                                         -- __ioref <- lift $ lift $ newIORef __c
                                        , HS.Generator noLoc (HS.PVar $ HS.Ident "__ioref") $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") $ (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift")
                                                                                                                      (HS.App (HS.Var $ HS.UnQual $ HS.Ident "newIORef")
                                                                                                                             (HS.Var $ HS.UnQual $ HS.Ident "__c")))
                                         --       __astate@(AState {aCounter = __counter})  <- lift $ RWS.get
                                         , HS.Generator noLoc (HS.PAsPat (HS.Ident "__astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "get"))))

                                         -- lift $ RWS.put (__astate {aCounter = __counter + 1})
                                         , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") $ (HS.App (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "put"))) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "__astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))])))
                                         -- let __obj = ObjectRef __ioref __counter
                                         , HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__obj") Nothing 
                                                                  (HS.UnGuardedRhs (HS.App 
                                                                                          (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ObjectRef")
                                                                                                 (HS.Var $ HS.UnQual $ HS.Ident "__ioref"))
                                                                                           (HS.Var $ HS.UnQual $ HS.Ident "__counter"))) (HS.BDecls [])]

                                         -- __init_sync __obj
                                         , HS.Qualifier (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "mapMonad"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "withReaderT"))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__init"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))

                                         -- __run_sync __obj
                                         , HS.Qualifier (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "mapMonad"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "withReaderT"))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__run"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))
                                        
                                         -- return $ __obj
                                         , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                                                            (HS.Var $ HS.UnQual $ HS.Ident "__obj")
                                         ])) (HS.BDecls [])])
                                        
               :
               -- the whereis method
               HS.InsDecl (HS.FunBind [HS.Match noLoc (HS.Ident "whereis") [] Nothing
                                             (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                             (HS.BDecls [])])
               :
               -- the init method (optional)
               -- normalize to a method decl with name __init
               (case maybeBlock of
                  ABS.JustBlock b -> [tMethDecl $ ABS.MethDecl (error "compiler implementation") (ABS.Ident "__init") [] b]
                  ABS.NoBlock -> []
               ) 
               -- the run method does not need a special case, since it is generated as a normal method
              )

       :

       (concatMap (\ ((t, ABS.Ident i), fieldNumber) ->
                 [
                  -- adds an explicit type signature for setters
                  HS.TypeSig noLoc [HS.Ident $ "set_" ++ headToLower clsName ++ "_" ++ i]
                                      (HS.TyFun (tType t)
                                         (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual $ HS.Ident "ABS"))
                                                        (HS.TyCon $ HS.UnQual $ HS.Ident clsName))
                                          (HS.TyCon $ HS.Special $ HS.UnitCon)))

                  ,
                  HS.FunBind [HS.Match noLoc (HS.Ident $ "set_" ++ headToLower clsName ++ "_" ++ i) [HS.PVar $ HS.Ident "v" ] Nothing
                                   (HS.UnGuardedRhs $ HS.Do
                                    [
                                     -- (AConf this@(ObjectRef ioref _) thisCOG _) <- lift $ RWS.ask
                                     HS.Generator noLoc (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AConf")) [HS.PAsPat (HS.Ident "this") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "ioref"),HS.PWildCard])),HS.PVar (HS.Ident "thisCOG"),HS.PWildCard])) (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "ask"))))
                                     -- astate@(AState _ om) <- lift $ RWS.get
                                    ,HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AState")) [HS.PWildCard,HS.PVar (HS.Ident "om")]))) (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "get"))))
                                     -- lift $ lift $ modifyIORef' ioref (\ c -> c {class1_p1 = v})      -- update the field value
                                    ,HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "modifyIORef'"))) (HS.Var (HS.UnQual (HS.Ident "ioref")))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "c")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "c"))) [HS.FieldUpdate (HS.UnQual (HS.Ident $ headToLower clsName ++ "_" ++ i )) (HS.Var (HS.UnQual (HS.Ident "v")))]))))))
                                     -- let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (Object this, 0) om
                                     ,HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PTuple HS.Boxed [HS.PVar (HS.Ident "maybeWoken"),HS.PVar (HS.Ident "om'")]) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "updateLookupWithKey"))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "k"),HS.PVar (HS.Ident "v")] (HS.Con (HS.UnQual (HS.Ident "Nothing")))))) (HS.Tuple HS.Boxed [HS.App (HS.Con (HS.UnQual (HS.Ident "AnyObject"))) (HS.Var (HS.UnQual (HS.Ident "this"))),HS.Lit (HS.Int fieldNumber)])) (HS.Var (HS.UnQual (HS.Ident "om"))))) (HS.BDecls [])])

                                     -- maybe (return ()) (\ woken -> lift $ lift $ writeList2Chan thisCOG woken) maybeWoken
                                     ,HS.Qualifier (HS.App (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "maybe"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Con (HS.Special HS.UnitCon))))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "woken")] (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") (HS.App (HS.Var (HS.UnQual (HS.Ident "lift"))) (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "writeList2Chan"))) (HS.Var (HS.UnQual (HS.Ident "thisCOG")))) (HS.Var (HS.UnQual (HS.Ident "woken"))))))))) (HS.Var (HS.UnQual (HS.Ident "maybeWoken"))))

                                     -- lift $ RWS.put $ astate {aSleepingO = om'}
                                     ,HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "lift") (HS.App (HS.Var (HS.Qual (HS.ModuleName "RWS") (HS.Ident "put"))) (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aSleepingO")) (HS.Var (HS.UnQual (HS.Ident "om'")))])))
                                    ]
                                   )
                                   (HS.BDecls [])]])
            ) (zip allFields [0..])

       ++

       -- create the typeclass-instances
       map (\ (ABS.TypeIdent interf, imdecls) -> 
                HS.InstDecl noLoc [{- empty context for now, may need to fix later -}] 
                      (HS.UnQual $ HS.Ident $ interf ++ "_") -- interface name
                      [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName] -- the haskell instance / abs class name
                      (map tMethDecl imdecls)
           )
                 (M.toList scanInterfs)

       where
         allFields :: [(ABS.Type, ABS.Ident)] -- order matters, because the fields are indexed
         allFields = map (\ (ABS.Par t i) -> (t,i)) params ++ mapMaybe (\case
                                                                       ABS.FieldDecl t i -> Just (t,i)
                                                                       ABS.FieldDeclAss t i _ -> Just (t,i)
                                                                       ABS.MethDecl _ _ _ _ -> Nothing
                                                                      ) ldecls
         fieldInits = foldr (\ fdecl acc -> (case fdecl of
                                                ABS.FieldDeclAss _t (ABS.Ident fid) pexp -> 
                                                    (HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident $ "__" ++ fid) Nothing 
                                                                                   (HS.UnGuardedRhs $ tPureExp pexp (M.fromList (map swap allFields))) (HS.BDecls [])]) : acc
                                                ABS.FieldDecl t (ABS.Ident fid) -> (if isInterface t symbolTable
                                                                                  then  (HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident $ "__" ++ fid) Nothing
                                                                                                                       (HS.UnGuardedRhs $ tPureExp (ABS.ELit ABS.LNull) (M.fromList (map swap allFields))) (HS.BDecls [])])
                                                                                  else error "A field must be initialised if it is not of a reference type"
                                                                                  )
                                                                                      : acc
                                                ABS.MethDecl _ _ _ _ -> (case maybeBlock of
                                                                          ABS.NoBlock -> acc
                                                                          ABS.JustBlock _->  error "Second parsing error: Syntactic error, no method declaration accepted here")
                               )) [] ldecls

         tMethDecl (ABS.MethDecl _ (ABS.Ident mident) mparams (ABS.Block block)) = HS.InsDecl $ 
                      HS.FunBind [HS.Match noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) mparams ++ [HS.PVar $ HS.Ident "this"])
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName (map (\ (_, ABS.Ident fname) -> fname) allFields) (M.fromList (map swap allFields)) )  (HS.BDecls [])]
         tMethDecl _ = error "Second parsing error: Syntactic error, no field declaration accepted here"
         -- TODO, can be optimized
         scanInterfs :: M.Map ABS.TypeIdent [ABS.BodyDecl] -- assoc list of interfaces to methods
         scanInterfs = M.mapMaybeWithKey (\ interfName methodNames -> let mdecls' = filter (\case
                                                                                           ABS.MethDecl _ mname _ _  -> mname `elem` methodNames
                                                                                           _ -> False
                                                                                          )  (case maybeBlock of
                                                                                                ABS.NoBlock ->  ldecls
                                                                                                ABS.JustBlock _ -> rdecls
                                                                                             )
                                                                     in if null mdecls'
                                                                        then if (ABS.QualType [ABS.QualTypeIdent interfName]) `elem` imps
                                                                             then Just [] -- implements an empty interface
                                                                             else Nothing
                                                                        else Just mdecls') $ M.unions (map methods symbolTable)

    generateSubSelf :: String -> HS.Decl
    generateSubSelf iname = HS.InstDecl noLoc [] 
                            (HS.UnQual $ HS.Ident "Sub")
                            [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident iname] -- instance Sup Interf1 Interf1
                            [   -- the upcasting method
                                -- is id in this case
                                HS.InsDecl $ HS.FunBind $ [HS.Match noLoc (HS.Ident "up") [HS.PVar $ HS.Ident "x"] Nothing 
                                                           (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident "x") (HS.BDecls [])]
                            ]

                                                      

    generateSubs :: String -> [ABS.QualType] -> [HS.Decl]
    generateSubs iname extends = map (generateSub iname) (nub $ collectSubs extends)
        where
          collectSubs :: [ABS.QualType] -> [ABS.QualType]
          collectSubs extends = extends ++ concat (mapMaybe (\ (ABS.QualType eqids) -> liftM collectSubs (M.lookup (case last eqids of
                                                                                                              ABS.QualTypeIdent eid ->  eid
                                                                                                             )
                                                                                                  interfMap
                                                                                                  )) extends)


          interfMap = M.unions (map hierarchy symbolTable)

    generateSub iname (ABS.QualType sup) =  HS.InstDecl noLoc [] 
                              (HS.UnQual $ HS.Ident "Sub")
                              [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup] -- instance Sup Interf1 Interf1
                              [   -- the upcasting method
                                  -- is id in this case
                                  HS.InsDecl $ HS.FunBind $ [HS.Match noLoc (HS.Ident "up") [HS.PApp (HS.UnQual $ HS.Ident iname) [HS.PVar $ HS.Ident "a"]] Nothing 
                                                                             (HS.UnGuardedRhs $ HS.App (HS.Con $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup)
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "a")) (HS.BDecls [])]
                                        ]




    tBody :: ABS.FunBody -> HS.Rhs
    tBody ABS.Builtin = HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident "undefined" -- builtin turned to undefined
    tBody (ABS.PureBody exp) = HS.UnGuardedRhs $ tPureExp exp M.empty -- no scope, rely on haskell

    tPureExp :: ABS.PureExp -> Scope -> HS.Exp
    tPureExp (ABS.If predE thenE elseE) fscope = HS.If (tPureExp predE fscope) (tPureExp thenE fscope) (tPureExp elseE fscope)

    tPureExp (ABS.Let (ABS.Par _ (ABS.Ident pid)) eqE inE) fscope = 
                                              (HS.App -- apply the created lamdba to the equality expr
                                                  (HS.Lambda noLoc
                                                   -- ignore the type of the param because ABS let is monomorphic anyway, it can infer it
                                                   [HS.PVar $ HS.Ident pid] -- bound variable
                                                   (tPureExp inE fscope))
                                                  (tPureExp eqE fscope)
                                              )

    tPureExp (ABS.Case matchE branches) fscope = HS.Case (tPureExp matchE fscope) (map 
                                                                 (\ (ABS.CBranch pat exp) -> HS.Alt noLoc (tFunPat pat) (HS.UnGuardedAlt (tPureExp exp fscope)) (HS.BDecls []))
                                                                 branches)

    tPureExp (ABS.ECall (ABS.Ident cid) args) fscope = foldl 
                                            (\ acc nextArg -> HS.App acc (tPureExp nextArg fscope))
                                            (HS.Var $ HS.UnQual $ HS.Ident cid)
                                            args

    tPureExp (ABS.ENaryCall (ABS.Ident cid) args) fscope = HS.App 
                                                (HS.Var $ HS.UnQual $ HS.Ident cid)
                                                (HS.List (map (\ arg -> tPureExp arg fscope) args))

    -- be careful to parenthesize infix apps
    tPureExp (ABS.EOr left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual $ HS.Symbol "||")  (tPureExp right fscope)

    tPureExp (ABS.EAnd left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&")  (tPureExp right fscope)

    tPureExp (ABS.EEq left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")  (tPureExp right fscope)

    tPureExp (ABS.ENeq left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "/=")  (tPureExp right fscope)

    tPureExp (ABS.ELt left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<")  (tPureExp right fscope)

    tPureExp (ABS.ELe left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<=")  (tPureExp right fscope)

    tPureExp (ABS.EGt left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">")  (tPureExp right fscope)

    tPureExp (ABS.EGe left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">=")  (tPureExp right fscope)

    tPureExp (ABS.EAdd left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "+")  (tPureExp right fscope)

    tPureExp (ABS.ESub left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "-")  (tPureExp right fscope)

    tPureExp (ABS.EMul left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "*")  (tPureExp right fscope)

    tPureExp (ABS.EDiv left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "/")  (tPureExp right fscope)

    tPureExp (ABS.EMod left right) fscope = HS.Paren $ HS.InfixApp (tPureExp left fscope) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "%")  (tPureExp right fscope)

    tPureExp (ABS.ELogNeg e) fscope = HS.Paren $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "not") (tPureExp e fscope)

    tPureExp (ABS.EIntNeg e) fscope = HS.Paren $ HS.NegApp (tPureExp e fscope)

    tPureExp (ABS.EUnaryConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "Nil")])) _ = HS.Con $ HS.Special HS.ListCon -- for the translation to []

    tPureExp (ABS.EUnaryConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "EmptyMap")])) _ = HS.Var $ HS.UnQual $ HS.Ident "empty" -- for the translation to Data.Map

    tPureExp (ABS.EUnaryConstr (ABS.QualType qids)) _ = let mids = init qids
                                                  in HS.Con $ (if null mids 
                                                               then HS.UnQual 
                                                               else HS.Qual (HS.ModuleName $ joinQualTypeIds mids)
                                                              ) $ HS.Ident $ (\ (ABS.QualTypeIdent (ABS.TypeIdent cid)) -> cid) (last qids)

    tPureExp (ABS.EMultConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "Triple")]) pexps) fscope | length pexps == 3 = HS.Tuple HS.Boxed (map (\ pexp -> tPureExp pexp fscope) pexps) -- for the translation to tuples
                                                                                                 | otherwise = error "wrong number of arguments to Triple"
    tPureExp (ABS.EMultConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "Pair")]) pexps) fscope | length pexps == 2  = HS.Tuple HS.Boxed (map (\ pexp -> tPureExp pexp fscope) pexps) -- for the translation to tuples
                                                                                               | otherwise = error "wrong number of arguments to Pair"
    tPureExp (ABS.EMultConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "Cons")]) [pexp1, pexp2]) fscope =  -- for the translation to pexp1:pexp2
                                                                                                           HS.Paren (HS.InfixApp 
                                                                                                                         (tPureExp pexp1 fscope)
                                                                                                                         (HS.QConOp $ HS.Special $ HS.Cons)
                                                                                                                         (tPureExp pexp2 fscope))
    tPureExp (ABS.EMultConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "Cons")]) _) _ = error "wrong number of arguments to Cons"
    tPureExp (ABS.EMultConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "InsertAssoc")]) [pexp1, pexp2]) fscope = HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "insertAssoc") (tPureExp pexp1 fscope)) (tPureExp pexp2 fscope)

    tPureExp (ABS.EMultConstr (ABS.QualType [ABS.QualTypeIdent (ABS.TypeIdent "InsertAssoc")]) _) _ = error "wrong number of arguments to InsertAssoc"
    tPureExp (ABS.EMultConstr qids args) fscope = foldl
                                       (\ acc nextArg -> HS.App acc (tPureExp nextArg fscope))
                                       (tPureExp (ABS.EUnaryConstr qids) fscope)
                                       args


    tPureExp (ABS.EVar var@(ABS.Ident pid)) fscope = case M.lookup var fscope of
                                                       -- TODO: this should be turned into warning
                                                       --  pure expressions don't have a scope, because they rely in haskell for scoping
                                                       Nothing -> HS.Var $ HS.UnQual $ HS.Ident pid 
                                                       Just t -> HS.Paren $ ((if isInterface t symbolTable
                                                                            then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") -- upcasting if it is of a class type
                                                                            else id)
                                                                                     (HS.Var $ HS.UnQual $ HS.Ident pid))

    tPureExp (ABS.ELit lit) _ = case lit of
                                         (ABS.LStr str) ->  HS.Lit $ HS.String str
                                         (ABS.LInt i) ->  HS.Lit $ HS.Int i
                                         ABS.LThis -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "this")
                                         ABS.LNull -> (HS.Var $ HS.UnQual $ HS.Ident "null")


    -- this is a trick for sync_call and async_call TODO: error "Cannot compile object accesses in mathematically pure expressions"
    tPureExp (ABS.EThis (ABS.Ident ident)) _ = HS.Var $ HS.UnQual $ HS.Ident ("__" ++ ident)

    isInterface :: ABS.Type -> [ModuleST] -> Bool
    isInterface (ABS.TypeVar (ABS.QualType [ABS.QualTypeIdent iid])) sts = iid `M.member` (M.unions (map methods sts))
    isInterface _ _ = False


    tFunPat :: ABS.Pattern -> HS.Pat
    tFunPat (ABS.PIdent (ABS.Ident pid)) = HS.PVar $ HS.Ident $ pid
    tFunPat (ABS.PUnaryConstr (ABS.TypeIdent "Nil")) = HS.PList []
    tFunPat (ABS.PUnaryConstr (ABS.TypeIdent tid)) = HS.PApp (HS.UnQual $ HS.Ident tid) []
    tFunPat (ABS.PMultConstr (ABS.TypeIdent "Triple") subpats) | length subpats == 3 = HS.PTuple HS.Boxed (map tFunPat subpats)
                                                               | otherwise = error "wrong number of arguments to Triple"
    tFunPat (ABS.PMultConstr (ABS.TypeIdent "Pair") subpats) | length subpats == 2 = HS.PTuple HS.Boxed (map tFunPat subpats)
                                                             | otherwise = error "wrong number of arguments to Pair"
    tFunPat (ABS.PMultConstr (ABS.TypeIdent "Cons") [subpat1, subpat2]) = HS.PParen (HS.PInfixApp 
                                                                          (tFunPat subpat1)
                                                                          (HS.Special $ HS.Cons)
                                                                          (tFunPat subpat2))
    tFunPat (ABS.PMultConstr (ABS.TypeIdent "Cons") _) = error "wrong number of arguments to Cons"
    tFunPat (ABS.PMultConstr (ABS.TypeIdent "InsertAssoc") _) = error "InsertAssoc is unsafe, you should avoid it."
    tFunPat (ABS.PMultConstr (ABS.TypeIdent tid) subpats) = HS.PApp (HS.UnQual $ HS.Ident tid) (map tFunPat subpats)
    tFunPat ABS.PUnderscore = HS.PWildCard
    tFunPat (ABS.PLit lit) = HS.PLit $ case lit of
                                         (ABS.LStr str) ->  HS.String str
                                         (ABS.LInt i) ->  HS.Int i
                                         _ -> error "this or null are not allowed in pattern syntax"

    tMethodSig :: String -> ABS.MethSig -> HS.ClassDecl
    tMethodSig ityp (ABS.MethSig tReturn (ABS.Ident mname) pars)  = HS.ClsDecl $
       HS.TypeSig noLoc [HS.Ident mname] (foldr  -- function application is right-associative
                                     (\ tpar acc -> HS.TyFun (tType tpar) acc)
                                     (HS.TyFun 
                                      (HS.TyCon $ HS.UnQual $ HS.Ident ityp)
                                      (HS.TyApp ((HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ABS") 
                                                              (HS.TyVar $ HS.Ident "a")) )
                                                (tType tReturn))
                                     )
                                     (map (\ (ABS.Par typ _) -> typ) pars))


    tThisExp :: ABS.PureExp -> String -> [Scope] -> HS.Exp
    tThisExp texp cls scopes = let thisTerms = collect texp
                    in if null thisTerms
                       then (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (tPureExp texp fscope)) --  rhs  
                       else
                           HS.Paren $ HS.InfixApp 
                                 (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                                 (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
                                 (HS.Lambda noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                                    map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                                 (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisTerms))
                                                  ] (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (tPureExp texp fscope)))
        where fscope = M.unions scopes

    tType :: ABS.Type -> HS.Type
    tType t = tTypeOrTyVar [] t     -- no type variables in scope

    tTypeOrTyVar :: [ABS.TypeIdent] -> ABS.Type -> HS.Type
    tTypeOrTyVar _ ABS.TyUnit  = HS.TyCon $ HS.Special $ HS.UnitCon
    tTypeOrTyVar _ ABS.TyInt  = HS.TyCon $ HS.UnQual $ HS.Ident "Int"
    tTypeOrTyVar _ ABS.TyRat  = HS.TyCon $ HS.UnQual $ HS.Ident "Rational"
    tTypeOrTyVar _ (ABS.TyFut par) = HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "Fut") (tType par)
    tTypeOrTyVar tyvars (ABS.TypeVar (ABS.QualType qtids))  = let joinedTid = joinQualTypeIds qtids    
                                                          in if (ABS.TypeIdent joinedTid) `elem` tyvars -- type variable
                                                             then HS.TyVar $ HS.Ident $ headToLower joinedTid
                                                             else HS.TyCon $ if length qtids == 1 
                                                                             then HS.UnQual $ HS.Ident joinedTid -- unqual
                                                                             else HS.Qual (HS.ModuleName (joinQualTypeIds (init qtids))) -- qual
                                                                                   (HS.Ident $ (\ (ABS.QualTypeIdent (ABS.TypeIdent tid)) -> tid) (last qtids))

    tTypeOrTyVar tyvars (ABS.ArgType qtyp tyargs) = foldl (\ tyacc tynext -> HS.TyApp tyacc (tTypeOrTyVar tyvars tynext)) (tType (ABS.TypeVar qtyp)) tyargs


    tMain :: ABS.MaybeBlock -> [HS.Decl]
    tMain ABS.NoBlock = []
    tMain (ABS.JustBlock (ABS.Block block)) = 
       -- main can only return with: return Unit;
       HS.PatBind noLoc (HS.PVar (HS.Ident "mainABS")) Nothing (HS.UnGuardedRhs $ tBlockWithReturn block
                                                                      ("Top")
                                                                      (error "No context for this")
                                                                      M.empty
                                                               ) (HS.BDecls [])
                                      :
                                      [HS.PatBind noLoc (HS.PVar (HS.Ident "main")) Nothing 
                                             (HS.UnGuardedRhs (HS.App (HS.Var (HS.UnQual (HS.Ident "main_is")))
                                                                      (HS.Var (HS.UnQual (HS.Ident "mainABS"))))) (HS.BDecls [])]

    tBlockWithReturn :: [ABS.Stm] -> String -> [String] -> Scope -> HS.Exp
    -- method block or main block
    -- can return and also pushes a new scope
    tBlockWithReturn stmts cls clsFields scope = tBlock stmts True cls clsFields [scope]

    tBlock :: [ABS.Stm] -> Bool -> String -> [String] -> [Scope] -> HS.Exp
    tBlock [] _canReturn _ _ _ = eReturnUnit
    tBlock stmts canReturn cls clsFields scopes = HS.Do $ tStmts stmts canReturn cls clsFields (M.empty:scopes) ++
                                 -- if the last stmt is an assignment, then add a return (R ())
                                 -- 
                                 (case last stmts of
                                    ABS.SAss _ _ -> [HS.Qualifier eReturnUnit]
                                    ABS.SExp _ ->  [HS.Qualifier eReturnUnit] -- although an expression has a value, we throw it away, since it must explicitly be returned
                                    ABS.SFieldAss _ _ -> [HS.Qualifier eReturnUnit]
                                    ABS.SDecAss _ _ _ ->  [HS.Qualifier eReturnUnit]
                                    _ -> []
                                 )
                                            

                                          

    -- tail-recursive tStmts
    tStmts :: [ABS.Stm] -> Bool -> String -> [String] -> [Scope] -> [HS.Stmt]
    tStmts [] _canReturn _ _ _ = []
    tStmts (stmt:rest) canReturn cls clsFields scopes = case stmt of
                       ABS.SExp eexp -> HS.Qualifier (tEffExp eexp cls scopes) -- have to force to WHNF
                                                           : tStmts rest canReturn cls clsFields scopes
                       ABS.SSuspend -> HS.Qualifier (HS.Var $ HS.UnQual $ HS.Ident "suspend") : tStmts rest canReturn cls clsFields scopes
                       ABS.SBlock stmts -> HS.Qualifier (tBlock stmts False cls clsFields scopes) : tStmts rest canReturn cls clsFields scopes
                       ABS.SSkip ->  HS.Qualifier (HS.Var (HS.UnQual $ HS.Ident "skip")) : tStmts rest canReturn cls clsFields scopes
                       ABS.SReturn e -> if canReturn
                                       then if null rest
                                            then [HS.Qualifier $ case e of
                                                                   ABS.ExpE eexp -> tEffExp eexp cls scopes
                                                                   ABS.ExpP texp -> tThisExp texp cls scopes
                                                 ]
                                            else error "Return must be the last statement"
                                       else error "Return must be the last statement" -- maybe differentiate between these two errors
                       ABS.SIf texp stm -> HS.Qualifier (HS.App 
                                                              (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenM") (tThisExp texp cls scopes))
                                                        (tBlock [stm] False cls clsFields scopes)) : tStmts rest canReturn cls clsFields scopes
                       ABS.SIfElse texp stm_then stm_else -> HS.Qualifier (HS.App
                                                                                (HS.App
                                                                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenelseM") (tThisExp texp cls scopes))
                                                                                       (tBlock [stm_then] False cls clsFields scopes))
                                                                                (tBlock [stm_else] False cls clsFields scopes)) : tStmts rest canReturn cls clsFields scopes
                       ABS.SAssert texp -> HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "assert") (tThisExp texp cls scopes)) : tStmts rest canReturn cls clsFields scopes
                       ABS.SWhile texp stm -> HS.Qualifier (HS.App
                                                                 (HS.App (HS.Var $ HS.UnQual $ HS.Ident "while")
                                                                  (tThisExp texp cls scopes))
                                                                 (tBlock [stm] False cls clsFields scopes)) : tStmts rest canReturn cls clsFields scopes
                       ABS.SDec typ ident -> tStmts rest canReturn cls clsFields (addToScope scopes ident typ)
                       -- ignore the dec TODO: don't ignore it, remove the ident from the class attributes to check

                       -- ABS.SDecAss typ ident@(ABS.Ident var) (ABS.ExpE eexp) ->  HS.Generator noLoc
                       --                                            (HS.PVar $ HS.Ident var) -- lhs
                       --                                            ((case (typ, eexp) of
                       --                                                (ABS.TypeVar (ABS.QualType qids), (ABS.New _ _)) -> HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "liftM") (HS.Var $ HS.UnQual $ HS.Ident $ (\ (ABS.QualTypeIdent (ABS.TypeIdent iid)) -> iid) (last qids)))
                       --                                                (ABS.TypeVar (ABS.QualType qids), (ABS.NewLocal _ _)) -> HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "liftM") (HS.Var $ HS.UnQual $ HS.Ident $ (\ (ABS.QualTypeIdent (ABS.TypeIdent iid)) -> iid) (last qids)))
                       --                                                _ -> id) (tRhs eexp cls scopes)) -- rhs
                       --                                            : tStmts rest canReturn cls clsFields (addToScope scopes ident typ) -- same as dec

                       -- otherwise, normalize it to Dec + Ass
                       ABS.SDecAss typ ident texp -> tStmts (ABS.SDec typ ident : ABS.SAss ident texp : rest) canReturn cls clsFields scopes

                       ABS.SAss ident@(ABS.Ident var) (ABS.ExpP texp) ->  (HS.Generator noLoc 
                                                                            (HS.PVar $ HS.Ident var) -- lhs
                                                                            ((case M.lookup ident (M.unions scopes) of -- add type signature from scope
                                                                                Just t ->  (flip $ HS.ExpTypeSig noLoc) (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual (HS.Ident "ABS"))) (HS.TyVar (HS.Ident cls))) (tType t))
                                                                                Nothing -> id) -- otherwise let it infer it
                                                                             (tThisExp texp cls scopes)))
                                                                     : tStmts rest canReturn cls clsFields scopes
                       ABS.SAss ident@(ABS.Ident var) (ABS.ExpE eexp)-> 
                           HS.Generator noLoc
                                 (HS.PVar $ HS.Ident var) -- lhs
                                 ((case M.lookup ident (M.unions scopes) of -- add type signature from scope
                                    Just t ->  (flip $ HS.ExpTypeSig noLoc) (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual (HS.Ident "ABS"))) (HS.TyVar (HS.Ident cls))) (tType t))
                                    Nothing -> id) -- otherwise let it infer it
                                 ((case eexp of
                                    ABS.New _ _ -> liftInterf ident scopes
                                    ABS.NewLocal _ _ -> liftInterf ident scopes
                                    _ -> id )
                                    (tRhs eexp cls scopes)))
                                 : tStmts rest canReturn cls clsFields scopes
                           
                       ABS.SFieldAss (ABS.Ident ident) (ABS.ExpP texp) ->  (HS.Qualifier (HS.Paren $ HS.InfixApp 
                                                                                               (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ ident)
                                                                                               (HS.QVarOp $ HS.UnQual $ HS.Symbol "=<<")
                                                                                         (HS.Paren (tThisExp texp cls scopes)))) -- paren are necessary here
                                                                          : tStmts rest canReturn cls clsFields scopes
                       ABS.SFieldAss ident@(ABS.Ident var) (ABS.ExpE eexp)-> 
                           (HS.Qualifier (HS.Paren $ HS.InfixApp 
                                          (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ var)
                                          (HS.QVarOp $ HS.UnQual $ HS.Symbol "=<<")
                                          ((case eexp of
                                              ABS.New _ _ -> liftInterf ident scopes
                                              ABS.NewLocal _ _ -> liftInterf ident scopes
                                              _ -> id )
                                          (tRhs eexp cls scopes))))
                                 : tStmts rest canReturn cls clsFields scopes
                           
                       ABS.SAwait g -> (HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "await") (tAwaitGuard g cls clsFields))) :
                                                                    (tStmts rest canReturn cls clsFields scopes)

    liftInterf ident@(ABS.Ident var) scopes = case M.lookup ident (M.unions scopes) of
                                Nothing -> error $ "Identifier " ++ var ++ " cannot be resolved from scope"
                                Just (ABS.TypeVar (ABS.QualType qids)) -> HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "liftM") (HS.Var $ HS.UnQual $ HS.Ident $ (\ (ABS.QualTypeIdent (ABS.TypeIdent iid)) -> iid) (last qids)))
                                Just _ -> error $ var ++ " not of interface type"

    tRhs eexp cls scopes = (let argsExps = case eexp of
                                                   ABS.Get pexp -> [pexp]
                                                   ABS.New _ pexps  -> pexps
                                                   ABS.NewLocal _ pexps -> pexps
                                                   ABS.SyncCall pexp1 _ pexps2 -> pexp1:pexps2
                                                   ABS.ThisSyncCall _ pexps -> pexps
                                                   ABS.AsyncCall pexp1 _ pexps2 -> pexp1:pexps2
                                                   ABS.ThisAsyncCall _ pexps -> pexps
                                thisTerms = concatMap collect argsExps
                                  in
                                 (if null thisTerms
                                  then tEffExp eexp cls scopes
                                  else -- readObject this >>= \ Class1 { record bindings   } ->
                                      HS.Paren $ HS.InfixApp 
                                                 (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                                                (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
                                                (HS.Lambda noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                                                   map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                                                (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisTerms))
                                                                 ]
                                                 (tEffExp eexp cls scopes))))

    tAwaitGuard :: ABS.Guard -> String -> [String] -> HS.Exp
    tAwaitGuard (ABS.VarGuard (ABS.Ident ident)) cls clsFields = HS.App
                                               (HS.Con $ HS.UnQual $ HS.Ident "FutureGuard")
                                               (HS.Var $ HS.UnQual $ HS.Ident ident)
    tAwaitGuard (ABS.ExpGuard pexp) cls clsFields = let awaitFields = collect pexp
                                                in
                                                  (HS.App (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ThisGuard") 
                                                                 (HS.List (map (HS.Lit . HS.Int . toInteger) (findIndices (`elem` awaitFields) clsFields))))
                                                    (tPureExp pexp M.empty)) -- TODO: might also require fscope here

    tAwaitGuard (ABS.FieldGuard (ABS.Ident ident)) cls clsFields = error "Not implemented yet, take Cosimo's consideration into account"
    tAwaitGuard (ABS.AndGuard gl gr) cls clsFields = HS.Paren $ HS.InfixApp 
                                   (tAwaitGuard gl cls clsFields)
                                   (HS.QVarOp $ HS.UnQual  $ HS.Symbol ":&:")
                                   (tAwaitGuard gr cls clsFields)


    tEffExp :: ABS.EffExp -> String -> [Scope] -> HS.Exp
    tEffExp (ABS.New (ABS.TypeVar (ABS.QualType qtids)) pexps) _cls scopes = (HS.App
                                                                       (HS.Var $ HS.UnQual $ HS.Ident "new")
                                                                       (foldl
                                                                        (\ acc pexp -> HS.App acc (tPureExp pexp fscope))
                                                                        (HS.Var  
                                                                               ((let mids = init qtids
                                                                                in
                                                                                  if null mids
                                                                                  then HS.UnQual
                                                                                  else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                                                                               (HS.Ident $ "__" ++ headToLower ( (\ (ABS.QualTypeIdent (ABS.TypeIdent cid)) -> cid) (last qtids)))))
                                                                        pexps))
        where fscope = M.unions scopes
    tEffExp (ABS.New _ _) _ _ = error "Not valid class name"

    tEffExp (ABS.NewLocal (ABS.TypeVar (ABS.QualType qtids)) pexps) _cls scopes = (HS.App
                                                                       (HS.Var $ HS.UnQual $ HS.Ident "new_local")
                                                                       (foldl
                                                                        (\ acc pexp -> HS.App acc (tPureExp pexp fscope))
                                                                        (HS.Var  
                                                                               ((let mids = init qtids
                                                                                in
                                                                                  if null mids
                                                                                  then HS.UnQual
                                                                                  else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                                                                               (HS.Ident $ "__" ++ headToLower ((\ (ABS.QualTypeIdent (ABS.TypeIdent cid)) -> cid) (last qtids)))))
                                                                        pexps))
        where fscope = M.unions scopes

    tEffExp (ABS.NewLocal _ _) _ _ = error "Not valid class name"


    tEffExp (ABS.SyncCall texp (ABS.Ident method) args) _cls scopes = HS.Paren $ HS.App 
                                                               (foldl
                                                                (\ acc arg -> HS.App acc (tPureExp arg fscope))
                                                                (HS.Var $ HS.UnQual $ HS.Ident $ method ++ "_sync")
                                                                args)
       (tPureExp texp fscope)
        where fscope = M.unions scopes
    -- normalize
    tEffExp (ABS.ThisSyncCall method args) cls scopes = tEffExp (ABS.SyncCall (ABS.ELit $ ABS.LThis) method args) cls scopes

    tEffExp (ABS.AsyncCall texp (ABS.Ident method) args) _cls scopes = HS.Paren $ HS.App 
                                                               (foldl
                                                                (\ acc arg -> HS.App acc (tPureExp arg fscope))
                                                                (HS.Var $ HS.UnQual $ HS.Ident $ method ++ "_async")
                                                                args)
       (tPureExp texp fscope)
        where fscope = M.unions scopes
    -- normalize
    tEffExp (ABS.ThisAsyncCall method args) cls scopes = tEffExp (ABS.AsyncCall (ABS.ELit $ ABS.LThis) method args) cls scopes

    tEffExp (ABS.Get texp) _cls scopes = HS.App (HS.Var $ HS.UnQual $ HS.Ident "get") (tPureExp texp fscope)
        where fscope = M.unions scopes

    tModuleName :: ABS.QualType -> HS.ModuleName
    tModuleName (ABS.QualType qtis) = HS.ModuleName $ joinQualTypeIds qtis

    eReturnUnit :: HS.Exp
    eReturnUnit = (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                         (HS.Con $ HS.Special $ HS.UnitCon)) -- return ()

    in mapM_ (prettyProgram . tProgram) asts


-- Utils

joinQualTypeIds :: [ABS.QualTypeIdent] -> String
joinQualTypeIds qtids = concat $ intersperse "." $ map (\ (ABS.QualTypeIdent (ABS.TypeIdent str)) -> str) qtids

typOfConstrType :: ABS.ConstrType -> ABS.Type
typOfConstrType (ABS.EmptyConstrType typ) = typ
typOfConstrType (ABS.RecordConstrType typ _) = typ

headToLower :: String -> String
headToLower (x:xs) = toLower x : xs

collect                              :: ABS.PureExp -> [String]
collect (ABS.Let _ pexp1 pexp2)      = collect pexp1 ++ collect pexp2
collect (ABS.If pexp1 pexp2 pexp3)   = collect pexp1 ++ collect pexp2 ++ collect pexp3
collect (ABS.Case pexp cbranches)    = collect pexp ++ concatMap (\ (ABS.CBranch _ pexp) -> collect pexp) cbranches
collect (ABS.EOr pexp1 pexp2)        = collect pexp1 ++ collect pexp2
collect (ABS.EAnd pexp1 pexp2)       = collect pexp1 ++ collect pexp2
collect (ABS.EEq pexp1 pexp2)        = collect pexp1 ++ collect pexp2
collect (ABS.ENeq pexp1 pexp2)       = collect pexp1 ++ collect pexp2
collect (ABS.ELt pexp1 pexp2)        = collect pexp1 ++ collect pexp2
collect (ABS.ELe pexp1 pexp2)        = collect pexp1 ++ collect pexp2
collect (ABS.EGt pexp1 pexp2)        = collect pexp1 ++ collect pexp2
collect (ABS.EGe pexp1 pexp2)        = collect pexp1 ++ collect pexp2
collect (ABS.EAdd pexp1 pexp2)       = collect pexp1 ++ collect pexp2
collect (ABS.ESub pexp1 pexp2)       = collect pexp1 ++ collect pexp2
collect (ABS.EMul pexp1 pexp2)       = collect pexp1 ++ collect pexp2
collect (ABS.EDiv pexp1 pexp2)       = collect pexp1 ++ collect pexp2
collect (ABS.EMod pexp1 pexp2)       = collect pexp1 ++ collect pexp2
collect (ABS.ELogNeg pexp)           = collect pexp
collect (ABS.EIntNeg pexp)           = collect pexp
collect (ABS.ECall _ pexps)          = concatMap collect pexps
collect (ABS.ENaryCall _ pexps)      = concatMap collect pexps
collect (ABS.EMultConstr _ pexps)    = concatMap collect pexps
collect (ABS.EThis (ABS.Ident attr)) = [attr]
collect _ = []

type Scope = M.Map ABS.Ident (ABS.Type)

addToScope :: [Scope] -> ABS.Ident -> ABS.Type -> [Scope]
addToScope (topscope:restscopes) var@(ABS.Ident pid) typ = M.insertWith (const $ const $ error $ pid ++ " already defined in this scope") var typ topscope  : restscopes
