{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | ウィキペディアの常用漢字一覧を甲州記法へ変換します。

module Main where

import qualified Data.Char                as Char
import qualified Data.Text                as Tx
import qualified Numeric                  as Num
import qualified Text.XML.Cursor          as C
import qualified Text.HTML.DOM            as H
import qualified Koshucode.Baala.Base     as Kb
import qualified Koshucode.Baala.Data     as Kd


-- --------------------------------------------  main

-- | エントリ・ポイント
main :: IO ()
main = do ks <- fromHtml htmlFile
          mapM_ putStrLn comment
          mapM_ (Kd.putJudge . kanjiJudge) ks

-- | 見出し
comment :: [String]
comment = [ "** -*- koshu -*-"
          , "**"
          , "**  常用漢字の一覧"
          , "**"
          , "**  このデータはウィキペディアの常用漢字一覧の本表から変換されました。"
          , "**  <https://ja.wikipedia.org/wiki/常用漢字一覧>"
          , "**  ウィキペディアの一覧は、平成 22 年内閣告示第 2 号の常用漢字表にもとづきます。"
          , "**"
          , "**  常用漢字"
          , "**    /通用字体 であらわされる漢字は、常用漢字として日本政府が選定した。"
          , "**    この漢字は /追加年 に常用漢字として追加された、"
          , "**    あるいは /削除年 に常用漢字から削除された。"
          , "**    この漢字に対応する /旧字体 がある。"
          , "**    この漢字は /部首 をもち、/総画数 で書ける。"
          , "**    学年別漢字配当表により、小学校の /学年 (1 から 6 まで) か、"
          , "**    /学年 が S のときは中学校以降で習う。"
          , "**    常用漢字内の音読み (カタカナ)、訓読み (ひらがな-送りがな) として"
          , "**    /読み に含まれる読み方がある。"
          , "**"
          , "" ]


-- --------------------------------------------  ウィキペディア

-- | <https://ja.wikipedia.org/wiki/常用漢字一覧> を保存したファイルの名前
htmlFile :: FilePath
htmlFile = "joyo-kanji.html"

-- | 常用漢字の一覧を HTML ファイルから抽出します。
fromHtml :: FilePath -> IO [JoyoKanji]
fromHtml path =
    do doc <- H.readFile path
       let cur = C.fromDocument doc
           ks  = cur C.$// kanjiList
       return ks

-- | 'C.check' の一般形.
check :: (C.Boolean b) => (a -> b) -> a -> [a]
check p c | C.bool $ p c  = [c]
          | otherwise     = []

-- | 常用漢字の本表を XML カーソルから抽出します。
kanjiList :: C.Cursor -> [JoyoKanji]
kanjiList = mainTable C.&/ C.element "tr" C.>=> kanji

-- | 本表の @\<table\>@
mainTable :: C.Axis
mainTable = C.check old where
    old = C.element "table"
          C.&/ C.element "tr"
          C.&/ C.element "th"
          C.&/ C.content
          C.>=> check (== "旧字")

-- | カーソルを一文字の漢字データに変換します。
kanji :: C.Cursor -> [JoyoKanji]
kanji cur =
    case cur C.$/ C.element "td" of
      [no, joyo, old, rad, nos, learn, add, del, re]
          -> [JoyoKanji { joyoNo      = num no
                        , joyoKanji   = text joyo
                        , joyoOld     = text old
                        , joyoRadical = text rad
                        , joyoNos     = num nos
                        , joyoLearn   = text learn
                        , joyoAdd     = num add
                        , joyoDel     = num del
                        , joyoRead    = text re
                        }]
      [] -> []
      xs -> error $ unlines $ "Not kanji data" : map show xs
    where
      -- 文字項目
      text :: C.Cursor -> String
      text c = 
          let direct = c C.$/ C.content
              anchor = c C.$/ C.element "a" C.&/ C.content
              string = Tx.unpack $ Tx.concat (direct ++ anchor)
          in filter (not . Char.isSpace) string
                   
      -- 数値項目
      num :: C.Cursor -> Maybe Int
      num c = maybeInt $ text c

-- | 文字列を整数として読み込みます。
maybeInt :: (Eq n, Num n) => String -> Maybe n
maybeInt s = case Num.readDec s of
               [(n, s')] | Kb.trimLeft s' == "" -> Just n
               _                                -> Nothing


-- --------------------------------------------  常用漢字

-- | 常用漢字
data JoyoKanji = JoyoKanji
    { joyoNo       :: Maybe Int    -- ^ 番号
    , joyoKanji    :: String       -- ^ 通用字体
    , joyoOld      :: String       -- ^ 旧字体
    , joyoRadical  :: String       -- ^ 部首
    , joyoNos      :: Maybe Int    -- ^ 総画数 (number of strokes)
    , joyoLearn    :: String       -- ^ 学年
    , joyoAdd      :: Maybe Int    -- ^ 追加年
    , joyoDel      :: Maybe Int    -- ^ 削除年
    , joyoRead     :: String       -- ^ 読み
    } deriving (Show, Eq, Ord)

-- | 常用漢字の肯定判断を作成します。
kanjiJudge :: JoyoKanji -> Kd.Judge Kd.BaalaC
kanjiJudge JoyoKanji {..} = j where
    j  = Kd.JudgeAffirm "常用漢字" $ Kd.omitEmpty ts
    ts = [ "番号"     ! dec    joyoNo
         , "通用字体" ! text   joyoKanji
         , "旧字体"   ! text   joyoOld
         , "部首"     ! text   joyoRadical
         , "総画数"   ! dec    joyoNos
         , "学年"     ! learn  joyoLearn
         , "追加年"   ! dec    joyoAdd
         , "削除年"   ! dec    joyoDel
         , "読み"     ! Kd.pTextSet (Kb.wordsBy (== '、') joyoRead)
         ]

    n ! c   = (n, c)

    dec     = Kd.maybeEmpty Kd.pDecFromInt

    text "" = Kd.empty
    text s  = Kd.pText s

    learn s = case maybeInt s of
                Just n  -> Kd.pDecFromInt n
                Nothing -> Kd.pCode s

