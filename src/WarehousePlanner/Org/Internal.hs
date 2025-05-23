{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WarehousePlanner.Org.Internal
( scenarioKey
, scenarioLayoutSize
, warehouseScenarioKey
, executeStep
, contentPathM
, sSortedSteps
, readScenarioFromPath
, readScenarioFromPaths
, readScenario
, savePointScenario
, scenarioToTextWithHash
, parseScenarioFile
, fileValid
, writeHeader
, addTagsToHeader
, scenarioToFullText
, importDispatch, importDispatchDef
)
where 

import ClassyPrelude
import WarehousePlanner.Base
import WarehousePlanner.Csv
import WarehousePlanner.Org.Types
import WarehousePlanner.Selector (splitOnNonEscaped)
import Control.Monad (zipWithM)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Writer (tell, execWriter)
import Data.Text(strip,splitOn)
import Data.Text qualified as Text
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeExtension, takeBaseName, splitExtension)
import Data.List qualified as List
import GHC.Generics
import Crypto.Hash qualified as Crypto
import System.FilePath.Glob(globDir1, compile, match)
import WarehousePlanner.WPL.Exec
import WarehousePlanner.WPL.PrettyPrint

-- * Parsing 
-- | Read and cut a scenario file into different component
parseScenarioFile :: (Maybe FilePath) ->  Text -> Either Text [Section]
parseScenarioFile pathm text = do -- Either
  headerM <- case pathm of
                Nothing -> Right Nothing
                Just path -> case splitExtension path of
                      (_, ".org") -> Right Nothing
                      (csv, ".csv") -> let ftype = takeExtension csv
                                      in case parseDrawer (pack $ drop 1 ftype) of
                                         Right headertype -> Right $ Just headertype
                                         _ -> Left $ tshow ftype <> " is an invalid csv type in " <> tshow path
                      (_, ".wpl" ) -> Right $ Just $ WPLH []
                      (_, ftype) -> Left $ tshow ftype <> " is an invalid type in " <> tshow path
  lineTypes <- traverse parseLine (lines text) 
  sequence $ linesToSections $ maybe id (\h -> (HeaderL h "" :)) headerM $ lineTypes
  

-- | Transform a line of text to a typed line
parseLine :: Text -> Either Text TypedLine
parseLine l@(strip -> line) | "-- END " `isPrefixOf` line          = Right EndL
               | ":END:" `isPrefixOf` line            = Right EndSectionL
               | "-- " `isPrefixOf` line              = Right $ CommentL
               | Just drawer <- extractDrawer line      = drawer
               | Just sha <- stripPrefix "@" line     = Right $ HashL (DocumentHash $ strip sha)
               | Just headerType <- extractHeader line=  Right $ HeaderL headerType line
               | line                 == ""     = Right $ CommentL
               | otherwise                            = Right $ TextL l


-- | Regroup lines into section
linesToSections :: [TypedLine] -> [Either Text Section]
linesToSections lines_ = reverse $ go lines_ Nothing [] [] where
  go :: [TypedLine] --  ^ lines_ to parse
     -> Maybe (HeaderType, Text) --  ^ header for current body
     -> [Text] --  ^ line of the current body in reverse order
     -> [Either Text Section] --  ^ previously parsed in reverse order
     -> [Either Text Section]
  go ((HeaderL newHeader title):ls) header current sections = -- start new section
    go ls (Just (newHeader, title)) [] (merge header current sections)
  go [] Nothing __current sections = sections
  go [] header current sections = merge header current sections
  go ((CommentL):ls) header current sections = go ls header current sections
  go ((EndL):_) header current sections = go [] header current sections
  go ((EndSectionL):ls) header current sections = -- close section 
        go ls Nothing [] (merge header current sections)
  -- not in section, skip
  go ((TextL _):ls) Nothing current sections = go ls Nothing current sections
  go ((TextL txt):ls) header current sections = go ls header (txt:current) sections
  -- Only one hash can be in a section. Close the previous section and open a new one at the same time
  go ((HashL sha):ls) header@(Just (ht,title)) [] sections = go ls header [] (Right (Section ht (Left sha) title) :sections)
  go ls@((HashL _):_) header current sections = go ls header [] (go [] header current sections)
  -- 
  -- Close the current section and  merge similar section if possible
  merge :: Maybe (HeaderType, Text)  -> [Text] -> [Either Text Section] -> [Either Text Section]
  merge Nothing _ sections = sections
  merge (Just (t@(TitleH _), title)) [] sections = Right (Section t (Right []) title): sections
  merge __header [] sections = sections
  merge (Just (ht,title)) current sections = Right (Section ht (Right (reverse current)) title) : sections

-- | Header is like "*** [HEADER] title"
extractHeader :: Text -> Maybe HeaderType
extractHeader line = case words line of
  (stars:__section:_) | isStars stars -> Just $ TitleH (length stars)
  _ -> Nothing
  where isStars = all ((==) '*') 
  
extractDrawer :: Text -> Maybe (Either Text TypedLine)
extractDrawer s = do
  drawerE <- stripPrefix ":" s >>= stripSuffix ":" >>= Just . parseDrawer
  return $ fmap (\d -> HeaderL d "") drawerE

parseDrawer :: Text -> Either Text HeaderType
parseDrawer h = case splitOn "_" h of
  [] -> Left $ "'' not a valid drawer."
  x:xs -> let (header, tags) = pre (toLower x : xs )
        in parseHeader header tags
  where 
      pre :: [Text] -> (Text, [Text])
      pre  [] = ("", [])
      pre allx@(x:xs) = let options = asum [ (cname,) <$> List.stripPrefix as allx
                                           | (c, ass) <- aliases
                                           , let cname = toLower $ writeHeader c
                                           , as <- [cname]:ass
                                           ]
                        in fromMaybe (x, xs) options
      aliases = [(MovesAndTagsH [],   [ ["moves and tags"]
                                      , ["moves", "and", "tags"]
                                      , ["movesandtags"]
                                      , ["mats"]
                                      , ["mat"]
                                      , ["tags and moves"]
                                      , ["tagsandmoves"]
                                      , ["tam"]
                                      ])
                 ,(ShelfTagsH,        [ ["shelf tags"]
                                      , ["tag", "shelves"]
                                      , ["shelves", "tag"]
                                      , ["tag shelves"]
                                      ])
                 ,(ShelfSplitH,       [ ["shelf", "split"]
                                      , ["shelf split"]
                                      , ["split shelves"]
                                      , ["split", "shelves"]
                                      , ["splitshelves"]
                                      ])
                 ,(ShelfJoinH,        [ ["shelf", "join"]
                                      , ["shelf join"]
                                      , ["join shelves"]
                                      ])
                 ,(UpdateShelvesH,    [ ["update shelves"]])
                 ,(TransformTagsH [], [ ["transform tags"]
                                      , ["transform", "tags"]
                                      , ["transform"]
                                      ])
                 ,(ClonesH [],        [ ["clone"]])
                 ,(DeletesH,          [ ["delete"]])
                 ,(ColourMapH,        [ ["colors"]
                                      , ["colours"]
                                      ])
                 ,(RearrangeH [],     [ ["rar"]])
                 ,(CheckShelvesH, [ ["check", "shelves"]
                                  , ["check"]
                                  ])
                 ]
  
-- | remove the H and to lower
cleanCName :: Text -> Text
cleanCName = toLower . dropEnd 1



class GHeader f where
  gwriteHeader :: f a -> Text
  gaddTags :: [Text] -> f a -> f a
  gParse :: Text -> [Text] -> Either Text (f a)

instance GHeader U1 where -- Single constructor
  gwriteHeader _ = ""
  gaddTags _ = id
  gParse _ _ = Right U1

instance (GHeader a , GHeader b) => GHeader (a :+: b) where -- Sum
  gwriteHeader (L1 x) = gwriteHeader x
  gwriteHeader (R1 x) = gwriteHeader x
  gaddTags tags (L1 x) = L1 (gaddTags tags x)
  gaddTags tags (R1 x) = R1 (gaddTags tags x)
  gParse header tags = fmap L1 (gParse header tags) <> fmap R1 (gParse header tags)
instance GHeader (K1 i [Text] ) where -- Constructor with tags
  gwriteHeader (K1 []) = ""
  gwriteHeader (K1 xs) = "_" <> intercalate "_" xs
  gaddTags tags (K1 xs) = K1 (xs <> tags)
  gParse _header tags = Right $ K1 tags
instance GHeader (K1 i Int ) where -- Title
  gwriteHeader (K1 _i) = ""
  gaddTags _tags (K1 i) = K1 i
  gParse header _tags = Right $ K1 l where 
         l = length $ span (== '*') header
instance GHeader a => GHeader (D1 c a) where
  gwriteHeader (M1 x) = gwriteHeader x 
  gaddTags tags (M1 x) = M1 (gaddTags tags x)
  gParse header tags = M1 <$> gParse header tags
-- | get the constructor and remove the trailing H
instance forall a c. (Constructor c, GHeader a) => GHeader (C1 c a) where
  gwriteHeader m@(M1 x) = (Text.init . pack $ conName m) <>  gwriteHeader x
  gaddTags tags (M1 x) = M1 (gaddTags tags x)
  gParse header tags = if header == cleanCName do pack cname
                  then M1 <$> (gParse header tags)
                  else Left $ header <> " invalid header"
                  where cname = conName @c (error "dummy to get type constructor")
instance GHeader a => GHeader (S1 c a) where
  gwriteHeader (M1 x) = gwriteHeader x
  gaddTags tags (M1 x) = M1 (gaddTags tags x)
  gParse header tags = M1 <$> gParse header tags

    
writeHeader :: HeaderType -> Text
writeHeader (TitleH _) = ""
writeHeader header = gwriteHeader $ from header

addTagsToHeader :: [Text] -> HeaderType -> HeaderType
addTagsToHeader tags g = to . gaddTags tags $ from g

parseHeader :: Text -> [Text] -> Either Text HeaderType
parseHeader header tags = to <$> gParse header tags

-- | Read a scenario text file. Needs IO to cache each sections into
-- in tempory file. 
-- The reason for that is the we are only doing a wrapper over
-- the legacy planner which works by parsing files using cassava.
-- To work, we need to save each section to a temporary file which
-- can be read by the legacy planner.
-- To avoid creating the same temporary file over and over, which
-- just cache them once and use a SHA identify them.
readScenario :: MonadIO m
             => (Int -> Section -> m (Either Text [Section])) --  ^ section expander, mainly to import sections for URI
             -> (Maybe FilePath)
             -> Text
             -> m (Either Text Scenario)
readScenario expandSection pathm text = do
  runExceptT $ do
    sections <-   ExceptT . return $ (parseScenarioFile pathm text)
    ExceptT $ sectionsToScenario expandSection sections

sectionsToScenario :: MonadIO m => (Int -> Section -> m (Either Text [Section])) -> [Section] -> m (Either Text Scenario)
sectionsToScenario expandSection sections0 = do 
  runExceptT $ do
    let levels _ [] = []
        levels lastLevel (s:ss) = let level = case sectionType s of
                                                TitleH l -> l 
                                                _ -> lastLevel
                                  in level : levels level ss
                                    
    sections <- concat <$> ExceptT ( sequence <$> zipWithM expandSection (levels 0 sections0) sections0)
    steps' <- mapM (\s -> ExceptT $ cacheSection s) sections
    ExceptT . return $ makeScenario steps' 

  
-- | Read one scenario file
readScenarioFromPath :: MonadIO io
                     => Bool -> (Int -> Section -> io (Either Text [Section]))
                     -- \^ section expander, mainly to import sections for URI
                     -> FilePath -> io (Either Text Scenario)
readScenarioFromPath withHistory expandSection path = do
  let useStdin = takeBaseName path == "-"
      addNewFileEvent = if withHistory 
                        then \sc -> sc { sSteps = NewFile path : sSteps sc }
                        else id
  exists <-  liftIO $ doesFileExist path
  contentE <- liftIO $
           case (exists, useStdin) of
                (_, True) -> Right . repack <$> getContents -- use stdin
                (True,_) -> Right . decodeUtf8 <$> readFile path
                (False, False) -> return $ Left $ "File " <> tshow path <> "doesn't exist."
  
  case contentE of 
    Left e -> return $ Left e
    Right content -> fmap (fmap (addNewFileEvent)) $ readScenario expandSection (Just path) content


readScenarioFromPaths :: MonadIO io
                      => Bool -> (Int -> Section -> io (Either Text [Section]))
                      -> Maybe FilePath
                      -> [FilePath] -> io (Either Text Scenario)
readScenarioFromPaths withHistory expandSection currentDir paths = do
   finalPaths <- mapM finalPath paths
   scenarios <- forM finalPaths $ readScenarioFromPath withHistory expandSection 
   return $ fmap mconcat $ sequence scenarios
   where finalPath path = do
           let bare = case currentDir of
                       Nothing -> path
                       Just pwd -> pwd </> path
           exists <- liftIO $ doesFileExist bare
           return if exists
           then bare
           else bare <.> "org"
                   

  
                        

fileValid :: FilePath -> Bool
fileValid = (`elem` [".org", ".csv", ".wpl"]) . takeExtension

savePointScenario :: Scenario
savePointScenario = Scenario Nothing [SavingPoint] Nothing mempty

-- | Save a content to a temporary file if needed
cacheContent :: MonadIO m => Content -> m (Either Text DocumentHash)
cacheContent (Left sha) = do
  contentPath <- contentPathM
  -- check it exists
  exist <- liftIO $ doesFileExist (contentPath sha)
  return $ if exist
           then Right sha
           else Left $ "No file found for SHA: " <> (unDocumentHash sha)

cacheContent (Right texts) = do
  contentPath <- contentPathM
  let bs = encodeUtf8 $ Text.unlines texts
      key = computeDocumentKey bs
  
  writeFile (contentPath key) bs

  return (Right key)

contentPathM :: MonadIO m => m (DocumentHash -> FilePath)
contentPathM = do
  tmp <- liftIO $ defaultPathMaker
  let f (DocumentHash file) = tmp (DocumentHash "planner-") <> unpack file
  return f


-- | Saves files and replace them by their hash
cacheSection :: MonadIO m => Section -> m (Either Text (HeaderType, (DocumentHash, Text)))
cacheSection (Section InitialH content _) = do
  case content of
    Left sha -> do
      -- TODO: 
      -- wh <- cacheWarehouseOut sha
      -- return $ case wh of
        -- Nothing -> Left "Initial state doesn't exits anymore"
        return $ Right (InitialH, (sha, "* Initial"))
    Right  _ -> return $ Left "Initial section needs a SHA (@...)"
cacheSection Section{..} = runExceptT $ do
  sha <- ExceptT $ cacheContent $ fmap stripContent sectionContent
  return $ (sectionType, (sha, sectionTitle))
  where stripContent = case sectionType of
                         WPLH{} -> id
                         _ -> map strip
  --    ^^^^^^^^^^^^ 
  --       remove spaces left and right unless it is a WPL which is indentation sensitive
  
-- | Load files and replace them by their text content
unCacheSection :: MonadIO m => Section -> m Section
unCacheSection section = do
  content <- case sectionContent section of
                Right text -> return $ Right text
                Left sha -> do
                  contentM <- retrieveContent sha
                  return $ maybe (Left sha) (Right . lines) contentM
  return $ section{sectionContent=content}

retrieveContent :: MonadIO m => DocumentHash -> m (Maybe Text)
retrieveContent key = do
  contentPath <- contentPathM
  let path = contentPath key
  exist <- liftIO $ doesFileExist  path
  if exist
    then (Just . decodeUtf8) <$> readFile path
    else return Nothing

makeScenario :: [(HeaderType, (DocumentHash, Text))]  -> Either Text Scenario
makeScenario sections0 = do -- Either
  let (initials, sections1) = partition (( InitialH==). fst) sections0
      (layouts, sections2) = partition (( LayoutH==). fst) sections1
      (colourMaps, sections3) = partition (( ColourMapH==) . fst) sections2
      firstOrNone _ [] = Right Nothing
      firstOrNone _ [x] = Right (Just $ snd x)
      firstOrNone err _ = Left err

  initial <- firstOrNone "Too many INITIAL sections" initials
  layout <- firstOrNone "Too many LAYOUT sections" (take 1 $ reverse layouts) -- take last one

  let steps = concatMap (\(header , (sha, title)) ->
                           (case header of
                            TitleH _ | "!" `isSuffixOf` title -> [SavingPoint]
                            _ -> []
                            ) ++ [Step header sha title]) sections3

  Right $ Scenario (map fst initial) steps (map fst layout) (map (fst . snd) colourMaps)

-- * Pretty Printing 
scenarioToTextWithHash :: Scenario -> Text
scenarioToTextWithHash scenario = sectionsToText $ scenarioToSections scenario

sectionsToText :: [Section] -> Text
sectionsToText sections = unlines $ concatMap sectionToText sections

emptyHash :: DocumentHash
emptyHash = computeDocumentKey ""
sectionToText :: Section -> [Text]
sectionToText Section{..} = execWriter $ do
    tell [sectionTitle]
    hasHeader <- do
              case writeHeader sectionType of
                "" -> return False
                header -> tell [":" <> header <> ":"] >> return True
    case sectionContent of
       Left k@(DocumentHash key) -> if k /= emptyHash then tell ["@" <> key] else return ()
       Right texts -> tell texts 
    when hasHeader $ do
         tell [":END:"]
    return ()

scenarioToFullText :: MonadIO m => Scenario -> m Text
scenarioToFullText scenario =  do
  let sections = scenarioToSections scenario
  expanded <- mapM unCacheSection sections
  return $ sectionsToText expanded


scenarioToSections :: Scenario -> [Section]
scenarioToSections Scenario{..} = execWriter $ do  -- []
  _ <- forM sInitialState (\state -> tell [Section InitialH (Left state) "* INITIAL"])
  _ <- forM sLayout (\layout -> tell [Section LayoutH (Left layout) "* LAYOUT"])
  _ <- forM sColourMap (\cmap -> tell [Section ColourMapH (Left cmap) "* COLOUR MAP"])
  forM sSteps (\s -> case s of
                  Step header sha title -> tell [Section header (Left sha) title]
                  SavingPoint -> return ()
                  NewFile _ -> return ()
              )

-- | Key identifying the scenario. Takes all document and has them.
scenarioKey :: Scenario -> DocumentHash
-- if a scenario is an empty initial state we use the same key as the original warehouse/scenario
scenarioKey (Scenario (Just key) [] Nothing _) = key
scenarioKey sc = computeDocumentKey .  encodeUtf8 $ scenarioToTextWithHash  sc

-- | Key indentifying the warehouse scenario, i.e. not taking the layout into account
warehouseScenarioKey ::  Scenario -> DocumentHash
warehouseScenarioKey sc = scenarioKey (sc {sLayout = Nothing, sSteps = sSortedSteps sc} )

-- some steps need to be done before other to make sense
-- shelves first, then orientations rules, then in inital order
sSortedSteps :: Scenario -> [Step]
sSortedSteps Scenario{..} = let
  steps = zipWith key sSteps  [1..]
  key step i = ((priority step, i), step)
  priority step = case step of
                    Step ShelvesH _ _  -> 1
                    Step OrientationsH _ _  -> 2
                    _ -> 3
  sorted = sortOn fst steps
  in  map snd sorted

-- * Rendering 

executeStep :: Step -> IO (WH () s)
executeStep SavingPoint = return (return ())
executeStep (NewFile path) = return $ newWHEvent 0 $ "FILE: " <> pack path
executeStep (Step header sha txt) = do
  contentPath <- contentPathM
  let path = contentPath sha
      defaultOrientations = [tiltedForward, tiltedFR]
      splitStyle s = let (style, colour) = splitAt 8 s
                       in (style, drop 1 colour)
      execute step = do
        s <- step
        return (s >> clearCache)
  wh <- case header of
          LayoutH -> return $ return ()
          ShelvesH -> execute $ readShelves BoxOrientations path
          InitialH -> return $ return ()
          StocktakeH tags_ -> execute $ do
                             let (throwErrors, tags) = partition (== "@throwerror") tags_
                             wh <- readStockTake tags defaultOrientations splitStyle path
                             let throwError = not (null throwErrors)
                             return $ do wh >>= \case
                                                  (_,[]) -> return ()
                                                  _ | not throwError -> return ()
                                                  (_,errors) -> error . unpack $ unlines errors
          BoxesH tags -> execute $ readBoxes tags defaultOrientations splitStyle path
          MovesH tags -> execute $ readMoves tags path
          TagsH tags -> execute $ readTags tags path
          MovesAndTagsH tags -> execute $ readMovesAndTags tags path
          ShelfTagsH -> execute $ readShelfTags path
          ShelfSplitH -> execute $ readShelfSplit path
          ShelfJoinH -> execute $ readShelfJoin path
          UpdateShelvesH -> execute $ readUpdateShelves path
          OrientationsH -> execute $ setOrientationRules defaultOrientations path
          TransformTagsH tags -> execute $ readTransformTags path tags
          ClonesH tags -> execute $ readClones (tags) path
          DeletesH -> execute $ readDeletes path
          TitleH _level -> return $ return () -- $ newWHEvent level txt
          ImportH -> return $ return ()
          ColourMapH -> return $ return ()
          RearrangeH tags -> execute $ readRearrangeBoxes tags path
          FreezeOrderH tags -> execute $ readFreezeOrder tags path
          WPLH tags -> do
             wpl <- readWPL path
             when ("@pretty" `elem` tags)  do
                  putStrLn $ prettyWPLs wpl
             execute $ fmap runWPL $ return wpl
          CheckShelvesH -> execute $ readCheckShelves path
  return do
     case header of
       TitleH level -> newWHEvent level txt
       _ -> newWHEvent 99 (writeHeader header)
     wh

-- | Retrieve the number of line in the layout file
scenarioLayoutSize :: MonadIO m => Scenario -> m Int
scenarioLayoutSize Scenario{..} = 
  case sLayout of
    Nothing -> return 0
    Just layout -> do
      l <- retrieveContent layout
      return (maybe 0 (length . lines) l)


-- | Default expansion
{- rST::import
Allows to import whole planner files either from existing files or
generated on the fly from an import dispatch provider (such as ``Fames``). Each line correspond to an
import and will be replaced with the result of the import. Some
imports accepts tags. Tags are given by "tagging" the import line
using ``thing_to_import#tag1#tag#...``


-  ``files/``\ pattern[``#``\ exclusive-pattern] Import all files
   matching the glob pattern. Files are local to the planner template
   directory.Tags can be used to filter out some file matched by the
   original pattern. Example

   ::

      files/Base.org --  Base.org file
      files/Base/* -- all file present in the Base folder
      files/Base/*#moves.org -- all file present in the Base folder except Base/moves.org
      files/Base/moves.org -- Base/moves.org only

-  ``file/``\ pattern[``#``\ tags] Import one file matching the glob
   pattern. Files are local to the planner template directory.Tags
   are added to each sections of the corresponding file Example

   ::

      file/Base/container.org#C1 -- Call container and #C1 to all sections within it.

::rST -}
importDispatch :: (Monad io, MonadIO io) => FilePath -> Int -> (Text -> [Text] -> io (Either Text [Section])) -> Section -> io (Either Text [Section])
importDispatch plannerDir nestedLevel dispatch (Section ImportH (Right content) _) = runExceptT $ do
  sectionss <- forM content $ \uri ->  do
    let (main:tags) = splitOnNonEscaped "#" $ strip uri
        pieces = splitOnNonEscaped "/" main
    ss <- case map unpack pieces of
               ("file": paths@(_:_) ) ->  do
                 sections <- ExceptT $ readLocalFile (intercalate "/" (plannerDir : paths)) tags
                 ss <- mapM (ExceptT . importDispatch plannerDir (3) dispatch) sections
                 return $ concat ss
               ("files": paths@(_:_)) -> do
                 sections <- ExceptT $ readLocalFiles plannerDir (intercalate "/" paths) tags
                 sss <- mapM (ExceptT . importDispatch plannerDir (3) dispatch) sections
                 return $ concat sss
               _ -> ExceptT $ dispatch main tags
    return $ Section (TitleH 0)
                     (Right [])
                     (" " <> uri)
           : ss
  return $ Section (TitleH (nestedLevel + 1))
                   (Right [])
                   (replicate (nestedLevel + 1) '*' <> " Import ")
         : concatMap (map $ raiseLevel $ nestedLevel + 2) sectionss
importDispatch _ _ _ section = return $ Right [section]

raiseLevel :: Int -> Section -> Section 
raiseLevel nestedLevel section = 
            case sectionType section of
                 TitleH level -> section { sectionType = TitleH (level + nestedLevel)
                                         , sectionTitle = replicate nestedLevel '*'  <> (sectionTitle section)
                                         }
                 _ -> section
               
                                 


importDispatchDef :: (MonadIO io) => FilePath -> Int -> Section -> io (Either Text [Section])
importDispatchDef plannerDir nestedLevel = importDispatch plannerDir nestedLevel err where
  err main tags = return $ Left $ intercalate "#" (main:tags) <> " is not a valid import"
-- | Read local files using glob pattern(s)
-- Uses the same directory as the planner
readLocalFiles :: (Monad io, MonadIO io) => FilePath -> String -> [Text] -> io (Either Text [Section])
readLocalFiles plannerDir pat excluded = do
  files <- liftIO $ globDir1 (compile pat) plannerDir
  let orgs = filter valid $ sort files
      exPats = map (compile . unpack . ("/**/" <>) ) excluded
      valid f =  all ($ f) $ fileValid : map (\p -> not . match p) exPats
  contents <- mapM readFile orgs
  let sectionss = traverse (\(c,fp) -> parseScenarioFile (Just fp) $ decodeUtf8 $ c)  $ zip contents orgs
  return $ fmap concat sectionss

readLocalFile :: (Monad io, MonadIO io) => FilePath -> [Text] -> io (Either Text [Section])
readLocalFile path tags = do
  content <- readFile path
  let sections = parseScenarioFile (Just path). decodeUtf8 $ content
      addTags section = section {sectionType = addTagsToHeader tags (sectionType section) }
  return $ fmap (map addTags) sections
-- * 
computeDocumentKey :: ByteString -> DocumentHash
computeDocumentKey bs = let
  digest = Crypto.hash bs :: Crypto.Digest Crypto.SHA256
  in DocumentHash (tshow digest)

defaultPathMaker :: IO (DocumentHash -> FilePath)
defaultPathMaker = do
  let tmp = "/tmp/DocumentCache" 
      f (DocumentHash key) = tmp </> (unpack key)
  createDirectoryIfMissing True tmp
  return f
  

