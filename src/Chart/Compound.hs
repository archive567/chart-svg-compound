{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Chart.Compound
where

import Prelude
import Chart
import Data.ByteString.Char8 qualified as C
import Data.List qualified as List
import Optics.Core
import Data.Maybe
import Data.Text (pack)
import Control.Monad.Trans.State.Lazy
import MarkupParse
import Control.Monad

-- * compounding

runHudCompound ::
  -- | initial canvas
  CanvasBox ->
  -- | huds-chart tuples representing independent chart trees occupying the same canvas space
  [([Hud], ChartTree)] ->
  -- | integrated chart tree
  ChartTree
runHudCompound cb ts = mconcat $ zipWith (\i ct -> group (Just ("compound" <> pack (show i))) [ct]) [(0 :: Int) ..] $ runHudCompoundWith cb ts'
  where
    ts' = zipWith (\db (hs, ct) -> (db, hs, ct)) dbs ts
    dbs = singletonGuard . boxes . foldOf charts' . snd <$> ts

-- | Combine a collection of chart trees that share a canvas box.
runHudCompoundWith ::
  -- | initial canvas
  CanvasBox ->
  -- | databox-huds-chart tuples representing independent chart trees occupying the same canvas space
  [(DataBox, [Hud], ChartTree)] ->
  -- | integrated chart trees
  [ChartTree]
runHudCompoundWith cb ts = zipWith mkTree [(0 :: Int) ..] $ (\x -> x s) <$> zipWith (\cs db -> flip execState (HudChart (cs & over chart' (projectWith cb db)) mempty db)) (snd <$> css) (snd <$> dbs)
  where
    s =
      hss
        & List.sortOn (view #priority . snd)
        & List.groupBy (\a b -> view #priority (snd a) == view #priority (snd b))
        & fmap (closes . fmap (view #hud . snd))
        & sequence
    dbs = zip [(0 :: Int) ..] $ fmap (\(x, _, _) -> x) ts
    hss = mconcat $ fmap (\(i, xs) -> fmap (i,) xs) $ zip [(0 :: Int) ..] (fmap (\(_, x, _) -> x) ts)
    css = zip [(0 :: Int) ..] (fmap (\(_, _, x) -> x) ts)
    mkTree i hc = group (Just ("chart" <> pack (show i))) [view #chart hc] <> group (Just ("hud" <> pack (show i))) [view #hud hc]


-- | Decorate a ChartTree with HudOptions
addHudCompound :: [(HudOptions, ChartTree)] -> [ChartTree]
addHudCompound [] = []
addHudCompound ts@((ho0, cs0) : _) =
  runHudCompoundWith
    (initialCanvas (view #chartAspect ho0) cs0)
    (zip3 dbs hss css)
  where
    hss = fst <$> huds
    dbs = snd <$> huds
    css = (snd <$> ts) -- <> (blank <$> dbs)
    huds = (\(ho, cs) -> toHuds ho (singletonGuard $ view box' cs)) <$> ts

addHudCompound' :: [(HudOptions, ChartTree)] -> [ChartTree]
addHudCompound' [] = []
addHudCompound' ts@((ho0, cs0) : _) =
  runHudCompoundWith
    (initialCanvas (view #chartAspect ho0) cs0)
    (zip3 dbs hss css)
  where
    hss = fst <$> huds
    dbs = snd <$> huds
    css = (snd <$> ts) <> (blank <$> dbs)
    huds = (\(ho, cs) -> toHuds ho (singletonGuard $ view box' cs)) <$> ts

collapseCompound :: [ChartOptions] -> ChartOptions
collapseCompound [] = mempty
collapseCompound cs@(c0 : _) =
  ChartOptions
    (view #markupOptions c0)
    (mempty & set #chartAspect (view (#hudOptions % #chartAspect) c0))
    (group (Just "compound") $ addHudCompound (zip (view #hudOptions <$> cs) (view #charts <$> cs)))

markupCompoundChartOptions :: [ChartOptions] -> Markup
markupCompoundChartOptions [] = mempty
markupCompoundChartOptions cs@(co0 : _) =
    header
      (view (#markupOptions % #markupHeight) co0)
      viewbox
      ( markupCssOptions (view (#markupOptions % #cssOptions) co0)
          <> mconcat (markupChartTree <$> csAndHuds)
      )
  where
    viewbox = singletonGuard (foldRect $ mconcat $ maybeToList . view styleBox' <$> csAndHuds)
    csAndHuds = addHudCompound (zip (view #hudOptions <$> cs) (view #charts <$> cs))

encodeCompoundChartOptions :: [ChartOptions] -> C.ByteString
encodeCompoundChartOptions [] = mempty
encodeCompoundChartOptions cs@(c0:_) =
  markdown_ (view (#markupOptions % #renderStyle) c0) Xml  (markupCompoundChartOptions cs)

writeCompoundChartOptions :: FilePath -> [ChartOptions] -> IO ()
writeCompoundChartOptions fp cs = C.writeFile fp (encodeCompoundChartOptions cs)

compoundClose :: [(Int,Hud)] -> State HudChart ()
compoundClose hss =
      hss
        & List.sortOn (view #priority . snd)
        & List.groupBy (\a b -> view #priority (snd a) == view #priority (snd b))
        & fmap (closes . fmap (view #hud . snd))
        & sequence
        & void

-- compoundCloses :: [(Int,Hud)] -> [(Int, State HudChart ())]
compoundCloses :: [(Int, Hud)] -> [[(Int, Hud)]]
compoundCloses hss =
      hss
        & List.sortOn (view #priority . snd)
        & List.groupBy (\a b -> view #priority (snd a) == view #priority (snd b))

closes' :: Int -> [(Int, Hud)] -> State HudChart ()
closes' x xs = do
  xs' <- sequence (view #hud . snd <$> xs)
  modify (appendHud $ mconcat $ fmap snd $ filter ((==x) . fst) $ zip (fst <$> xs) xs')

closes'' :: Int -> [(Int, Hud)] -> [State HudChart ()]
closes'' n xs = (\x -> closes' x xs) <$> [0..(n-1)]

closes''' :: Int -> [[(Int, Hud)]] -> [State HudChart ()]
closes''' n xss = fmap sequence_ $ List.transpose $ fmap (closes'' n) xss

appendHud :: ChartTree -> HudChart -> HudChart
appendHud cs x =
  x & over #hud (<> cs)

prioritizeHuds :: [Hud] -> [[Hud]]
prioritizeHuds hss =
      hss
        & List.sortOn (view #priority)
        & List.groupBy (\a b -> view #priority a == view #priority b)


-- | Combine a collection of chart trees that share a canvas box.
runHudCompoundWithCandidate ::
  -- | initial canvas
  CanvasBox ->
  -- | databox-huds-chart tuples representing independent chart trees occupying the same canvas space
  [(DataBox, [Hud], ChartTree)] ->
  -- | integrated chart tree
  ChartTree
runHudCompoundWithCandidate cb ts = hss
  where
    hss =
      ts &
      fmap (\(db,hs,_) -> fmap (over #hud (withStateT (#dataBox .~ db))) hs) &
      mconcat &
      prioritizeHuds &
      mapM_ (closes . fmap (view #hud)) &
      flip execState (HudChart css mempty undefined) &
      (\x -> group (Just "chart") [view #chart x] <> group (Just "hud") [view #hud x])

    css =
      ts &
      fmap (\(db,_,ct) -> over chart' (projectWith cb db) ct) &
      mconcat

runHudCompoundCandidate ::
  -- | initial canvas
  CanvasBox ->
  -- | huds-chart tuples representing independent chart trees occupying the same canvas space
  [([Hud], ChartTree)] ->
  -- | integrated chart tree
  ChartTree
runHudCompoundCandidate cb ts = runHudCompoundWithCandidate cb ts'
  where
    ts' = zipWith (\db (hs, ct) -> (db, hs, ct)) dbs ts
    dbs = singletonGuard . boxes . foldOf charts' . snd <$> ts

collapseCompoundCandidate :: [ChartOptions] -> ChartOptions
collapseCompoundCandidate [] = mempty
collapseCompoundCandidate cs@(c0 : _) =
  ChartOptions
    (view #markupOptions c0)
    (mempty & set #chartAspect (view (#hudOptions % #chartAspect) c0))
    (addHudCompoundCandidate (zip (view #hudOptions <$> cs) (view #charts <$> cs)))

-- | Decorate a ChartTree with HudOptions
addHudCompoundCandidate :: [(HudOptions, ChartTree)] -> ChartTree
addHudCompoundCandidate [] = mempty
addHudCompoundCandidate ts@((ho0, cs0) : _) =
  runHudCompoundWithCandidate
    (initialCanvas (view #chartAspect ho0) cs0)
    (zip3 dbs hss css)
  where
    hss = zipWith (\i hs -> fmap (over #priority (+(Priority (i*0.1)))) hs) [0..] (fst <$> huds)
    dbs = snd <$> huds
    css = (snd <$> ts) -- <> (blank <$> dbs)
    huds = (\(ho, cs) -> toHuds ho (singletonGuard $ view box' cs)) <$> ts
