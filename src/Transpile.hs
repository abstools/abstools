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
import Data.List (intersperse, nub, union, findIndices)
import Data.Char (toLower)

import qualified Data.Map as M

data ModuleST = ModuleST {
      filePath :: FilePath,
      moduleName :: ABS.QualType,
      st :: M.Map ABS.TypeIdent [ABS.Ident]
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
                                                                        st = foldl insertInterfs M.empty decls
                                                                      }
                where 
                  insertInterfs :: M.Map ABS.TypeIdent [ABS.Ident] -> ABS.Decl -> M.Map ABS.TypeIdent [ABS.Ident]
                  insertInterfs acc (ABS.InterfDecl tident msigs) = insertInterfs acc (ABS.ExtendsDecl tident [] msigs)  -- normalization
                  insertInterfs acc (ABS.ExtendsDecl tident _ msigs) = 
                      {- TODO it could generate a compilation error because of duplicate method declaration -}
                      {- TODO it could generate a compilation error because of duplicate instance declaration, by using insertWithKey -}
                      M.insertWith union tident (collectMethods msigs) acc
                  insertInterfs acc _ = acc
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
                                               HS.Ident "ImpredicativeTypes",
                                               HS.Ident "LiberalTypeSynonyms"
                     ]] 
                     Nothing 
                     Nothing 
                     [
                      HS.ImportDecl {HS.importLoc = noLoc, HS.importModule = HS.ModuleName "Control.Monad.Trans.RWS", HS.importQualified = True, HS.importSrc = False, HS.importPkg = Nothing, HS.importAs = Just (HS.ModuleName "RWS"), HS.importSpecs = Nothing},
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
                 ABS.MultConstr (ABS.TypeIdent cid) args -> HS.QualConDecl noLoc [] [] (HS.ConDecl (HS.Ident cid) (map (HS.BangedTy . tTypeOrTyVar tyvars . typOfConstrType) args))) constrs)
           []
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
    tDecl (ABS.InterfDecl tid ms) = tDecl (ABS.ExtendsDecl tid [ABS.QualType $ [ABS.QualTypeIdent $ ABS.TypeIdent "Object"]]  ms) 

    tDecl (ABS.ExtendsDecl (ABS.TypeIdent tname) extends ms) = HS.ClassDecl 
                                                              noLoc 
                                                              (map (\ (ABS.QualType e) -> HS.ClassA (HS.UnQual $ HS.Ident $ joinQualTypeIds e ++ "_") [HS.TyVar (HS.Ident "a")]) extends)
                                                              (HS.Ident $ tname ++ "_") 
                                                              [HS.UnkindedVar (HS.Ident "a")]
                                                              [] -- no fundeps
                                                              (map tMethodSig ms)
       : [
        HS.TypeDecl noLoc (HS.Ident tname) [] (HS.TyForall (Just [HS.UnkindedVar $ HS.Ident "a"]) [HS.ClassA (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyVar (HS.Ident "a")]] (HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyVar $ HS.Ident "a")))
                                                               -- type synonym for Objects typed by the interface
                                                               -- type Interf1 = forall a. Interf1_ a => ObjectRef a
                                                              
       ]

    tDecl (ABS.Fun fReturnTyp fid params body) = tDecl (ABS.ParFun fReturnTyp fid [] params body) -- no type variables

    tDecl (ABS.ParFun fReturnTyp (ABS.Ident fid) tyvars params body) = 
       -- adds an explicit type signature
       [HS.TypeSig noLoc [HS.Ident fid] (foldr
                                      (\ tpar acc -> HS.TyFun (tTypeOrTyVar tyvars tpar) acc)
                                      (tTypeOrTyVar tyvars fReturnTyp)
                                      (map (\ (ABS.Par typ _) -> typ) params))
       ,
        HS.FunBind [HS.Match noLoc (HS.Ident fid) (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) params)  Nothing (tBody body)  (HS.BDecls [])]
       ]

    -- normalizing class declarations
    tDecl (ABS.ClassDecl tident fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] [] fdecls maybeBlock mdecls)
    tDecl (ABS.ClassParamDecl tident params fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident params [] fdecls maybeBlock mdecls)
    tDecl (ABS.ClassImplements tident imps fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] imps fdecls maybeBlock mdecls)
    tDecl (ABS.ClassParamImplements (ABS.TypeIdent clsName) params imps fdecls maybeBlock mdecls) = -- TODO add check for imps, if a method is not implemented
       -- the record-ADT of the ABS class
        HS.DataDecl noLoc HS.DataType [] (HS.Ident clsName) [] 
              -- TODO check if its an interface so to generate an ObjectRef type and not an ADT
              [HS.QualConDecl noLoc [] [] $ HS.RecDecl (HS.Ident clsName) (([HS.Ident $ headToLower clsName ++ "_loc"],
                                                                            -- maybe it should be banged for the fields of the class
                                                                           HS.UnBangedTy (HS.TyForall Nothing [HS.ClassA (HS.UnQual (HS.Ident "Object_")) [HS.TyVar (HS.Ident "o")]] (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual (HS.Ident "ABS"))) (HS.TyVar (HS.Ident "o")))  (HS.TyCon (HS.UnQual (HS.Ident "COG")))))
                                                                           ):map (\ (t,(ABS.Ident i)) -> ([HS.Ident $ headToLower clsName ++ "_" ++ i], HS.UnBangedTy (tType t)))  allFields)]  []
        :

        -- the smart constructor
        HS.FunBind [HS.Match noLoc (HS.Ident $ headToLower clsName)
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
              (HS.UnQual $ HS.Ident "Object_") -- interface name
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
                                                                      (foldr (\ fdecl acc -> case fdecl of
                                                                                              ABS.FieldDeclAss _t (ABS.Ident fid) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
                                                                                              ABS.FieldDecl _ _ -> acc
                                                                                              ABS.MethDecl _ _ _ _ -> error "Second parsing error: Syntactic error, no method declaration accepted here"
                                                                             ) 
                                                                       -- class1_loc = (return (R __chan))
                                                                       [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                                                          (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                                                                             (HS.Var $ HS.UnQual $ HS.Ident "__chan"))]
                                                                       fdecls)) (HS.BDecls [])]
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

                                        -- __obj `async_call` __init
                                        , HS.Qualifier $ HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "__obj")
                                                           (HS.QVarOp $ HS.UnQual $ HS.Ident $ "async_call")
                                                           (HS.Var $ HS.UnQual $ HS.Ident "__init")
                                        -- __obj `async_call` run
                                        , HS.Qualifier $ HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "__obj")
                                                           (HS.QVarOp $ HS.UnQual $ HS.Ident $ "async_call")
                                                           (HS.Var $ HS.UnQual $ HS.Ident "run")
                                        
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
                                                                      (foldr (\ fdecl acc -> case fdecl of
                                                                                              ABS.FieldDeclAss _t (ABS.Ident fid) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
                                                                                              ABS.FieldDecl _ _ -> acc
                                                                                              ABS.MethDecl _ _ _ _ -> error "Second parsing error: Syntactic error, no method declaration accepted here"
                                                                             ) 
                                                                       -- class1_loc = thisCOG)
                                                                       [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                                                          (HS.Var $ HS.UnQual $ HS.Ident "thisCOG")
                                                                             ]
                                                                       fdecls)) (HS.BDecls [])]                                          
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

                                         -- __obj `async_call` __init
                                         , HS.Qualifier $ HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "__obj")
                                                           (HS.QVarOp $ HS.UnQual $ HS.Ident $ "async_call")
                                                           (HS.Var $ HS.UnQual $ HS.Ident "__init")
                                         -- __obj `async_call` run
                                         , HS.Qualifier $ HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "__obj")
                                                           (HS.QVarOp $ HS.UnQual $ HS.Ident $ "async_call")
                                                           (HS.Var $ HS.UnQual $ HS.Ident "run")
                                        
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
                                     -- let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 0) om
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
         allFields = map (\ (ABS.Par t i) -> (t,i)) params ++ map (\case
                                                                     ABS.FieldDecl t i -> (t,i)
                                                                     ABS.FieldDeclAss t i _ -> (t,i)
                                                                     ABS.MethDecl _ _ _ _ -> error "Second parsing error: Syntactic error, no method declaration accepted here"
                                                                    ) fdecls
         fieldInits = foldr (\ fdecl acc -> case fdecl of
                                                ABS.FieldDeclAss _t (ABS.Ident fid) pexp -> 
                                                    (HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident $ "__" ++ fid) Nothing 
                                                                                   (HS.UnGuardedRhs $ tPureExp pexp) (HS.BDecls [])]) : acc
                                                ABS.FieldDecl _ _ -> acc
                                                ABS.MethDecl _ _ _ _ -> error "Second parsing error: Syntactic error, no method declaration accepted here"
                               ) [] fdecls

         tMethDecl (ABS.MethDecl _ (ABS.Ident mident) mparams (ABS.Block block)) = HS.InsDecl $ 
                      HS.FunBind [HS.Match noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) mparams ++ [HS.PVar $ HS.Ident "this"])
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName (map (\ (_, ABS.Ident fname) -> fname) allFields))  (HS.BDecls [])]
         tMethDecl _ = error "Second parsing error: Syntactic error, no field declaration accepted here"
         -- TODO, can be optimized
         scanInterfs :: M.Map ABS.TypeIdent [ABS.BodyDecl] -- assoc list of interfaces to methods
         scanInterfs = M.mapMaybe (\ methodNames -> let mdecls' = filter (\case
                                                                         ABS.MethDecl _ mname _ _  -> mname `elem` methodNames
                                                                         _ -> error "Second parsing error: Syntactic error, no field declaration accepted here"
                                                                        )  mdecls
                                                   in if null mdecls'
                                                      then Nothing
                                                      else Just mdecls') $ M.unions (map st symbolTable)

    tBody :: ABS.FunBody -> HS.Rhs
    tBody ABS.Builtin = HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident "undefined" -- builtin turned to undefined
    tBody (ABS.PureBody exp) = HS.UnGuardedRhs $ tPureExp exp

    tPureExp :: ABS.PureExp -> HS.Exp
    tPureExp (ABS.If predE thenE elseE) = HS.If (tPureExp predE) (tPureExp thenE) (tPureExp elseE)

    tPureExp (ABS.Let (ABS.Par _ (ABS.Ident pid)) eqE inE) = 
                                              (HS.App -- apply the created lamdba to the equality expr
                                                  (HS.Lambda noLoc
                                                   -- ignore the type of the param because ABS let is monomorphic anyway, it can infer it
                                                   [HS.PVar $ HS.Ident pid] -- bound variable
                                                   (tPureExp inE))
                                                  (tPureExp eqE)
                                              )

    tPureExp (ABS.Case matchE branches) = HS.Case (tPureExp matchE) (map 
                                                                 (\ (ABS.CBranch pat exp) -> HS.Alt noLoc (tFunPat pat) (HS.UnGuardedAlt (tPureExp exp)) (HS.BDecls []))
                                                                 branches)

    tPureExp (ABS.ECall (ABS.Ident cid) args) = foldl 
                                            (\ acc nextArg -> HS.App acc (tPureExp nextArg))
                                            (HS.Var $ HS.UnQual $ HS.Ident cid)
                                            args

    tPureExp (ABS.ENaryCall (ABS.Ident cid) args) = HS.App 
                                                (HS.Var $ HS.UnQual $ HS.Ident cid)
                                                (HS.List (map tPureExp args))

    tPureExp (ABS.EOr left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual $ HS.Symbol "||")  (tPureExp right)

    tPureExp (ABS.EAnd left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&")  (tPureExp right)

    tPureExp (ABS.EEq left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")  (tPureExp right)

    tPureExp (ABS.ENeq left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "/=")  (tPureExp right)

    tPureExp (ABS.ELt left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<")  (tPureExp right)

    tPureExp (ABS.ELe left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<=")  (tPureExp right)

    tPureExp (ABS.EGt left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">")  (tPureExp right)

    tPureExp (ABS.EGe left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">=")  (tPureExp right)

    tPureExp (ABS.EAdd left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "+")  (tPureExp right)

    tPureExp (ABS.ESub left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "-")  (tPureExp right)

    tPureExp (ABS.EMul left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "*")  (tPureExp right)

    tPureExp (ABS.EDiv left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "/")  (tPureExp right)

    tPureExp (ABS.EMod left right) = HS.InfixApp (tPureExp left) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "%")  (tPureExp right)

    tPureExp (ABS.ELogNeg e) = HS.App (HS.Var $ HS.UnQual $ HS.Ident "not") (tPureExp e)

    tPureExp (ABS.EIntNeg e) = HS.NegApp (tPureExp e)

    tPureExp (ABS.EUnaryConstr (ABS.QualType qids)) = let mids = init qids
                                                  in HS.Con $ (if null mids 
                                                               then HS.UnQual 
                                                               else HS.Qual (HS.ModuleName $ joinQualTypeIds mids)
                                                              ) $ HS.Ident $ (\ (ABS.QualTypeIdent (ABS.TypeIdent cid)) -> cid) (last qids)

    tPureExp (ABS.EMultConstr qids args) = foldl
                                       (\ acc nextArg -> HS.App acc (tPureExp nextArg))
                                       (tPureExp (ABS.EUnaryConstr qids) )
                                       args


    tPureExp (ABS.EVar (ABS.Ident pid)) = HS.Var $ HS.UnQual $ HS.Ident pid

    tPureExp (ABS.ELit lit) = HS.Lit $ tFunLit lit


    -- this is a trick for sync_call and async_call TODO: error "Cannot compile object accesses in mathematically pure expressions"
    tPureExp (ABS.EThis (ABS.Ident ident)) = HS.Var $ HS.UnQual $ HS.Ident ("__" ++ ident)


    tFunPat :: ABS.Pattern -> HS.Pat
    tFunPat (ABS.PIdent (ABS.Ident pid)) = HS.PVar $ HS.Ident $ pid
    tFunPat (ABS.PUnaryConstr (ABS.TypeIdent tid)) = HS.PApp (HS.UnQual $ HS.Ident tid) []
    tFunPat (ABS.PMultConstr (ABS.TypeIdent tid) subpats) = HS.PApp (HS.UnQual $ HS.Ident tid) (map tFunPat subpats)
    tFunPat ABS.PUnderscore = HS.PWildCard
    tFunPat (ABS.PLit lit) = HS.PLit $ tFunLit lit

    tFunLit (ABS.LStr str) = HS.String str
    tFunLit (ABS.LInt i) = HS.Int i
    tFunLit _ = error " this or null are not allowed inside the functional core of ABS"

    tMethodSig :: ABS.MethSig -> HS.ClassDecl
    tMethodSig (ABS.MethSig tReturn (ABS.Ident mname) pars)  = HS.ClsDecl $
       HS.TypeSig noLoc [HS.Ident mname] (foldr  -- function application is right-associative
                                     (\ tpar acc -> HS.TyFun (tType tpar) acc)
                                     (HS.TyFun 
                                      (HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef")
                                         (HS.TyVar $ HS.Ident "a"))
                                      (HS.TyApp ((HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ABS") 
                                                              (HS.TyVar $ HS.Ident "a")) )
                                                (tType tReturn))
                                     )
                                     (map (\ (ABS.Par typ _) -> typ) pars))


    tThisExp :: ABS.PureExp -> String -> HS.Exp
    tThisExp texp cls = let thisTerms = collect texp
                    in if null thisTerms
                       then (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (tPureExp texp)) --  rhs  
                       else
                           HS.InfixApp 
                                 (HS.App (HS.Var $ HS.UnQual $ HS.Ident "readObject") $ HS.Var $ HS.UnQual $ HS.Ident "this")
                                 (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
                                 (HS.Lambda noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                                    map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                                 (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisTerms))
                                                  ] (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (tPureExp texp)))



    -- tThisExp (ABS.EThis (ABS.Ident ident)) cls = HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "liftM")
    --                                                            (HS.Var $ HS.UnQual $ HS.Ident (headToLower cls ++ "_" ++ ident))) $ 
    --                                              HS.App (HS.Var $ HS.UnQual $ HS.Ident "readObject") (HS.Var $ HS.UnQual $ HS.Ident "this")




    tType :: ABS.Type -> HS.Type
    tType t = tTypeOrTyVar [] t     -- no type variables in scope

    tTypeOrTyVar :: [ABS.TypeIdent] -> ABS.Type -> HS.Type
    tTypeOrTyVar _ ABS.TyUnit  = HS.TyCon $ HS.Special $ HS.UnitCon
    tTypeOrTyVar _ ABS.TyInt  = HS.TyCon $ HS.UnQual $ HS.Ident "Int"
    tTypeOrTyVar _ ABS.TyRat  = HS.TyCon $ HS.UnQual $ HS.Ident "Rational"
    tTypeOrTyVar _ (ABS.TyFut par) = HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "Future") (tType par)
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
                                                                      (error "No context for this")
                                                                      (error "No context for this")
                                                               ) (HS.BDecls [])
                                      :
                                      [HS.PatBind noLoc (HS.PVar (HS.Ident "main")) Nothing 
                                             (HS.UnGuardedRhs (HS.App (HS.Var (HS.UnQual (HS.Ident "main_is")))
                                                                      (HS.Var (HS.UnQual (HS.Ident "mainABS"))))) (HS.BDecls [])]

    tBlockWithReturn :: [ABS.Stm] -> String -> [String] -> HS.Exp
    tBlockWithReturn stmts cls clsFields = tBlock stmts True cls clsFields

    tBlock :: [ABS.Stm] -> Bool -> String -> [String] -> HS.Exp
    tBlock [] _canReturn _ _ = eReturnUnit
    tBlock stmts canReturn cls clsFields = HS.Do $ tStmts stmts canReturn cls clsFields ++
                                 -- if the last stmt is an assignment, then add a return (R ())
                                 -- 
                                 (case last stmts of
                                    ABS.SAss _ _ -> [HS.Qualifier eReturnUnit]
                                    ABS.SFieldAss _ _ -> [HS.Qualifier eReturnUnit]
                                    ABS.SDecAss _ _ _ ->  [HS.Qualifier eReturnUnit]
                                    _ -> []
                                 )
                                            

                                          

    -- tail-recursive tStmts
    tStmts :: [ABS.Stm] -> Bool -> String -> [String] -> [HS.Stmt]
    tStmts [] _canReturn _ _ = []
    tStmts (stmt:rest) canReturn cls clsFields = case stmt of
                       ABS.SExp e -> HS.Qualifier (case e of
                                        ABS.ExpE eexp -> tEffExp eexp cls -- have to force to WHNF
                                        ABS.ExpP texp -> tThisExp texp cls -- have to force to WHNF
                                        -- error "Cannot run a pure expression as a standalone statement"                                   
                                      ) : tStmts rest canReturn cls clsFields
                       ABS.SSuspend -> HS.Qualifier (HS.Var $ HS.UnQual $ HS.Ident "suspend") : tStmts rest canReturn cls clsFields
                       ABS.SBlock stmts -> HS.Qualifier (tBlock stmts False cls clsFields) : tStmts rest canReturn cls clsFields
                       ABS.SSkip ->  HS.Qualifier (HS.Var (HS.UnQual $ HS.Ident "skip")) : tStmts rest canReturn cls clsFields
                       ABS.SReturn e -> if canReturn
                                       then if null rest
                                            then [HS.Qualifier $ case e of
                                                                   ABS.ExpE eexp -> tEffExp eexp cls
                                                                   ABS.ExpP texp -> tThisExp texp cls
                                                 ]
                                            else error "Return must be the last statement"
                                       else error "Return must be the last statement" -- maybe differentiate between these two errors
                       ABS.SIf texp stm -> HS.Qualifier (HS.App 
                                                              (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenM") (tThisExp texp cls))
                                                        (tBlock [stm] False cls clsFields)) : tStmts rest canReturn cls clsFields
                       ABS.SIfElse texp stm_then stm_else -> HS.Qualifier (HS.App
                                                                                (HS.App
                                                                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenelseM") (tThisExp texp cls))
                                                                                       (tBlock [stm_then] False cls clsFields))
                                                                                (tBlock [stm_else] False cls clsFields)) : tStmts rest canReturn cls clsFields
                       ABS.SAssert texp -> HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "assert") (tThisExp texp cls)) : tStmts rest canReturn cls clsFields
                       ABS.SWhile texp stm -> HS.Qualifier (HS.App
                                                                 (HS.App (HS.Var $ HS.UnQual $ HS.Ident "while")
                                                                  (tThisExp texp cls))
                                                                 (tBlock [stm] False cls clsFields)) : tStmts rest canReturn cls clsFields
                       ABS.SDec _typ _ident -> tStmts rest canReturn cls clsFields -- ignore the dec TODO: don't ignore it, remove the ident from the class attributes to check
                       ABS.SDecAss typ ident texp ->  tStmts (ABS.SDec typ ident : ABS.SAss ident texp : rest) canReturn cls clsFields -- normalize it to Dec + Ass
                       ABS.SAss (ABS.Ident ident) (ABS.ExpP texp) ->  (HS.Generator noLoc 
                                                                            (HS.PVar $ HS.Ident ident) -- lhs
                                                                            (tThisExp texp cls))
                                                                     : tStmts rest canReturn cls clsFields
                       ABS.SAss (ABS.Ident ident) (ABS.ExpE eexp)-> 
                           HS.Generator noLoc
                                 (HS.PVar $ HS.Ident ident) -- lhs
                                 (let argsExps = case eexp of
                                               ABS.Get _ -> [] -- tEffExp eexp cls
                                               ABS.ThisGet _ -> [] -- tEffExp eexp cls
                                               ABS.New _ pexps  -> pexps
                                               ABS.NewLocal _ pexps -> pexps
                                               ABS.SyncCall _ _ pexps -> pexps
                                               ABS.ThisSyncCall _ pexps -> pexps
                                               ABS.AsyncCall _ _ pexps -> pexps
                                               ABS.ThisAsyncCall _ pexps -> pexps
                                      thisTerms = concatMap collect argsExps
                                  in
                                 (if null thisTerms
                                  then tEffExp eexp cls
                                  else -- readObject this >>= \ Class1 { record bindings   } ->
                                      HS.InfixApp 
                                                 (HS.App (HS.Var $ HS.UnQual $ HS.Ident "readObject") $ HS.Var $ HS.UnQual $ HS.Ident "this")
                                                (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
                                                (HS.Lambda noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                                                   map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                                                (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisTerms))
                                                                 ]
                                                 (tEffExp eexp cls))
                                 )
                                 )
                                 : tStmts rest canReturn cls clsFields
                           
                       ABS.SFieldAss (ABS.Ident ident) (ABS.ExpP texp) ->  (HS.Qualifier (HS.InfixApp 
                                                                                               (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ ident)
                                                                                               (HS.QVarOp $ HS.UnQual $ HS.Symbol "=<<")
                                                                                         (HS.Paren (tThisExp texp cls)))) -- paren are necessary here
                                                                          : tStmts rest canReturn cls clsFields
                       ABS.SFieldAss (ABS.Ident ident) (ABS.ExpE eexp)-> 
                           (HS.Qualifier (HS.InfixApp 
                                          (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ ident)
                                          (HS.QVarOp $ HS.UnQual $ HS.Symbol "=<<")
                                          (HS.Paren (let -- paren are necessary here
                                                        argsExps = case eexp of
                                                                      ABS.Get _ -> [] -- tEffExp eexp cls
                                                                      ABS.ThisGet _ -> [] -- tEffExp eexp cls
                                                                      ABS.New _ pexps  -> pexps
                                                                      ABS.NewLocal _ pexps -> pexps
                                                                      ABS.SyncCall _ _ pexps -> pexps
                                                                      ABS.ThisSyncCall _ pexps -> pexps
                                                                      ABS.AsyncCall _ _ pexps -> pexps
                                                                      ABS.ThisAsyncCall _ pexps -> pexps
                                                        thisTerms = concatMap collect argsExps
                                                     in
                                 (if null thisTerms
                                  then tEffExp eexp cls
                                  else -- readObject this >>= \ Class1 { record bindings   } ->
                                      HS.InfixApp 
                                                 (HS.App (HS.Var $ HS.UnQual $ HS.Ident "readObject") $ HS.Var $ HS.UnQual $ HS.Ident "this")
                                                (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
                                                (HS.Lambda noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                                                   map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                                                (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisTerms))
                                                                 ]
                                                 (tEffExp eexp cls))
                                 )
                                 ))))
                                 : tStmts rest canReturn cls clsFields
                           
                       ABS.SAwait g -> (HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "await") (tAwaitGuard g cls clsFields))) :
                                                                    (tStmts rest canReturn cls clsFields)

    tAwaitGuard :: ABS.Guard -> String -> [String] -> HS.Exp
    tAwaitGuard (ABS.VarGuard (ABS.Ident ident)) cls clsFields = HS.App
                                               (HS.Con $ HS.UnQual $ HS.Ident "FutureGuard")
                                               (HS.Var $ HS.UnQual $ HS.Ident ident)
    tAwaitGuard (ABS.ExpGuard pexp) cls clsFields = let awaitFields = collect pexp
                                                in
                                                  (HS.App (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ThisGuard") 
                                                                 (HS.List (map (HS.Lit . HS.Int . toInteger) (findIndices (`elem` awaitFields) clsFields))))
                                                    (tPureExp pexp))

    tAwaitGuard (ABS.FieldGuard (ABS.Ident ident)) cls clsFields = error "Not implemented yet, take Cosimo's consideration into account"
    tAwaitGuard (ABS.AndGuard gl gr) cls clsFields = HS.InfixApp 
                                   (tAwaitGuard gl cls clsFields)
                                   (HS.QVarOp $ HS.UnQual  $ HS.Symbol ":&:")
                                   (tAwaitGuard gr cls clsFields)


    tEffExp :: ABS.EffExp -> String -> HS.Exp
    tEffExp (ABS.New (ABS.TypeVar (ABS.QualType qtids)) pexps) _cls = (HS.App
                                                                       (HS.Var $ HS.UnQual $ HS.Ident "new")
                                                                       (foldl
                                                                        (\ acc pexp -> HS.App acc (tPureExp pexp))
                                                                        (HS.Var  
                                                                               ((let mids = init qtids
                                                                                in
                                                                                  if null mids
                                                                                  then HS.UnQual
                                                                                  else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                                                                               (HS.Ident $ headToLower $ (\ (ABS.QualTypeIdent (ABS.TypeIdent cid)) -> cid) (last qtids))))
                                                                        pexps))
    tEffExp (ABS.New _ _) _ = error "Not valid class name"

    tEffExp (ABS.NewLocal (ABS.TypeVar (ABS.QualType qtids)) pexps) _cls = (HS.App
                                                                       (HS.Var $ HS.UnQual $ HS.Ident "new_local")
                                                                       (foldl
                                                                        (\ acc pexp -> HS.App acc (tPureExp pexp))
                                                                        (HS.Var  
                                                                               ((let mids = init qtids
                                                                                in
                                                                                  if null mids
                                                                                  then HS.UnQual
                                                                                  else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                                                                               (HS.Ident $ headToLower $ (\ (ABS.QualTypeIdent (ABS.TypeIdent cid)) -> cid) (last qtids))))
                                                                        pexps))

    tEffExp (ABS.NewLocal _ _) _ = error "Not valid class name"


    tEffExp (ABS.SyncCall (ABS.Ident obj) (ABS.Ident method) args) _cls = HS.InfixApp 
                                                                     (HS.Var $ HS.UnQual $ HS.Ident obj)
                                                                     (HS.QVarOp $ HS.UnQual $ HS.Ident "sync_call")
                                                                     (foldl
                                                                      (\ acc arg -> HS.App acc (tPureExp arg))
                                                                      (HS.Var $ HS.UnQual $ HS.Ident method)
                                                                      args)

    tEffExp (ABS.ThisSyncCall method args) cls = tEffExp (ABS.SyncCall (ABS.Ident "this") method args) cls

    tEffExp (ABS.AsyncCall (ABS.Ident obj) (ABS.Ident method) args) _cls = HS.InfixApp 
                                                                     (HS.Var $ HS.UnQual $ HS.Ident obj)
                                                                     (HS.QVarOp $ HS.UnQual $ HS.Ident "async_call")
                                                                     (foldl
                                                                      (\ acc arg -> HS.App acc (tPureExp arg))
                                                                      (HS.Var $ HS.UnQual $ HS.Ident method)
                                                                      args)

    tEffExp (ABS.ThisAsyncCall method args) cls = tEffExp (ABS.AsyncCall (ABS.Ident "this") method args) cls                                                                             
    tEffExp (ABS.Get ident) cls = HS.App (HS.Var $ HS.UnQual $ HS.Ident "get")
                              (tThisExp (ABS.EVar ident) cls)

    tEffExp (ABS.ThisGet attr) cls = HS.App (HS.Var $ HS.UnQual $ HS.Ident "get")
                                 (tThisExp (ABS.EThis attr) cls)



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

