{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | hashable hashes a Text or a ByteString with XXH3, through C that the
-- JavaScript backend does not compile; @core/jsbits/polyfills.js@ ports it
-- (see POLYFILLS.md). These tests pin that port to the C: every value below
-- is what hashable 1.5.1.0 computes natively, where its C is compiled, so the
-- same assertions on the JavaScript backend check the port against it, for
-- inputs in every one of XXH3's length classes (0, 1–3, 4–8, 9–16, 17–128,
-- 129–240, and longer, past one 1024-byte block and a partial stripe).
--
-- On the JavaScript backend an 'Int' is 32 bits, so a 'hash' there is the low
-- half of the 64-bit value; @fromIntegral@ of the golden value compares
-- exactly that there and all of it elsewhere. The last group calls the
-- polyfill's functions directly on the JavaScript backend and compares all 64
-- bits.
--
-- The golden values were printed by a small program compiled with the native
-- GHC against this project's hashable (run under @cabal exec@ in
-- @dist-newstyle/native@), from these definitions: 'bytes' as below; for each
-- length @n@, @hash (B.pack (bytes n))@; @hashByteArrayWithSalt@ of a
-- 'SBS.ShortByteString' of 'bytes' with each salt; @hashWithSalt s@ of a lazy
-- ByteString of 'bytes' in 'chunks', next to @hashByteArrayWithSalt s@ of
-- 'bytes' followed by the length as 8 little-endian bytes (what the stream
-- hashes, which makes the second column the full 64-bit XXH3 of it); for
-- each text @t@, @hash t@, @hash (T.drop 1 t)@ and @hashWithSalt 7@ of it as
-- a lazy Text in chunks of 5; for each word, @hashByteArrayWithSalt@ of its
-- 8 and 4 little-endian bytes.
module Test.Hashable (spec) where

import Control.Monad (forM_)
import Data.Bits (shiftR)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (hash, hashByteArrayWithSalt, hashWithSalt)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word (Word64, Word8)
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)
#if defined(javascript_HOST_ARCH)
import Data.Word (Word32)
import Foreign.C.Types (CSize (..))
import GHC.Exts (ByteArray#)

-- The polyfill's functions, called as hashable calls them.
foreign import ccall unsafe "hs_XXH3_64bits_withSeed_offset"
  xxh3Offset :: ByteArray# -> CSize -> CSize -> Word64 -> Word64

foreign import ccall unsafe "hs_XXH3_64bits_withSeed_u64"
  xxh3Word64 :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "hs_XXH3_64bits_withSeed_u32"
  xxh3Word32 :: Word32 -> Word64 -> Word64
#endif

-- | @n@ bytes of a pattern that leaves no two neighbours alike.
bytes :: Int -> [Word8]
bytes n = [fromIntegral (i * 167 + 13) | i <- [0 .. n - 1]]

-- | Chunks of growing size, so that a lazy value's stream is split unevenly.
chunks :: [a] -> [[a]]
chunks = go 1
  where
    go _ [] = []
    go k xs = let (a, b) = splitAt k xs in a : go (k * 3 + 1) b

littleEndian :: Int -> Word64 -> [Word8]
littleEndian k x = [fromIntegral (x `shiftR` (8 * i)) | i <- [0 .. k - 1]]

-- | Equal as 'Int's: all 64 bits natively and on wasm, the low 32 on the
-- JavaScript backend.
sameHash :: String -> Word64 -> Int -> IO ()
sameHash what golden = assertEqual what (fromIntegral golden)

spec :: Spec
spec = describe "hashable's XXH3" $ do
  it "hashes a strict ByteString of every length class" $
    forM_ strictHashes $ \(n, golden) ->
      sameHash ("ByteString of " <> show n) golden (hash (B.pack (bytes n)))

  it "hashes a byte array with a salt" $
    forM_ saltedHashes $ \(n, salt, golden) ->
      let !(SBS array) = SBS.pack (bytes n)
       in sameHash ("bytes " <> show n <> ", salt " <> show salt) golden (hashByteArrayWithSalt array 0 n salt)

  it "hashes a lazy ByteString as a stream" $
    forM_ streamedHashes $ \(n, salt, golden, _) ->
      sameHash ("lazy " <> show n <> ", salt " <> show salt) golden (hashWithSalt salt (BL.fromChunks (map B.pack (chunks (bytes n)))))

  it "hashes Text, a slice of one, and lazy Text" $
    forM_ textHashes $ \(t, whole, sliced, lazy) -> do
      sameHash ("text " <> show t) whole (hash t)
      sameHash ("text " <> show t <> " less its first character") sliced (hash (T.drop 1 t))
      sameHash ("lazy text " <> show t) lazy (hashWithSalt 7 (TL.fromChunks (T.chunksOf 5 t)))

  it "keeps a HashMap keyed by Text" $ do
    let keys = map (T.pack . show) [0 .. 500 :: Int] <> [t | (t, _, _, _) <- textHashes]
        table = HM.fromList (zip keys [0 :: Int ..])
    forM_ (zip keys [0 ..]) $ \(k, v) -> assertEqual ("lookup " <> show k) (Just v) (HM.lookup k table)

#if defined(javascript_HOST_ARCH)
  describe "on the JavaScript backend, all 64 bits" $ do
    it "of a byte array with a seed" $
      forM_ (saltedHashes <> [(n, 0, golden) | (n, golden) <- strictHashes]) $ \(n, salt, golden) ->
        let !(SBS array) = SBS.pack (bytes n)
         in assertEqual ("bytes " <> show n <> ", salt " <> show salt) golden (xxh3Offset array 0 (fromIntegral n) (fromIntegral salt))
    it "of what a stream hashes" $
      forM_ streamedHashes $ \(n, salt, _, golden) ->
        let !(SBS array) = SBS.pack (bytes n <> littleEndian 8 (fromIntegral n))
         in assertEqual ("stream " <> show n <> ", salt " <> show salt) golden (xxh3Offset array 0 (fromIntegral (n + 8)) (fromIntegral salt))
    it "of a word, as its bytes" $
      forM_ wordHashes $ \(x, salt, golden64, golden32) -> do
        assertEqual ("Word64 " <> show x) golden64 (xxh3Word64 x (fromIntegral salt))
        assertEqual ("Word32 " <> show x) golden32 (xxh3Word32 (fromIntegral x) (fromIntegral salt))
#else
  -- The same table read the other way: the words' golden values are hashes
  -- of their bytes, which the C computes everywhere but the JavaScript
  -- backend.
  it "hashes a word's bytes" $
    forM_ wordHashes $ \(x, salt, golden64, golden32) -> do
      let !(SBS eight) = SBS.pack (littleEndian 8 x)
          !(SBS four) = SBS.pack (littleEndian 4 x)
      sameHash ("Word64 " <> show x) golden64 (hashByteArrayWithSalt eight 0 8 salt)
      sameHash ("Word32 " <> show x) golden32 (hashByteArrayWithSalt four 0 4 salt)
#endif

-- The golden values.

strictHashes :: [(Int, Word64)]
strictHashes =
  [ (0, 3244421341483603138)
  , (1, 9953473643821969856)
  , (2, 11538408164903770248)
  , (3, 6854380581500377659)
  , (4, 12153447570723842627)
  , (5, 3395239738952025794)
  , (7, 1899929699632077069)
  , (8, 7473994587629095078)
  , (9, 9075147396419384944)
  , (15, 2001505174323493201)
  , (16, 13877419963702728759)
  , (17, 13560467639765998995)
  , (32, 18436302365004978924)
  , (33, 2861215511267736877)
  , (64, 3201508657082647536)
  , (65, 3591861603652762972)
  , (96, 2214281319955948975)
  , (97, 5344927117677944380)
  , (128, 11841678383244871894)
  , (129, 5522292113263139390)
  , (200, 10862687670879500168)
  , (240, 2889038531686385639)
  , (241, 213983505349086952)
  , (500, 5304919224561636447)
  , (1024, 5678441650331022609)
  , (1025, 11568382488138126718)
  , (2049, 17459738292381544444)
  , (4100, 8288978747430207488)
  ]

saltedHashes :: [(Int, Int, Word64)]
saltedHashes =
  [ (0, 1, 5604079703740606211)
  , (0, -1, 5478965906984379733)
  , (0, -1975321933, 7945537971744315322)
  , (1, 1, 620451599715999349)
  , (1, -1, 6202824297588074982)
  , (1, -1975321933, 12656406648225375867)
  , (2, 1, 12292334856942099410)
  , (2, -1, 4477510236547290417)
  , (2, -1975321933, 2902623169647115380)
  , (3, 1, 16603120100261730016)
  , (3, -1, 1354861817997695313)
  , (3, -1975321933, 721364645174394938)
  , (4, 1, 14260666254824321880)
  , (4, -1, 11429226228728878454)
  , (4, -1975321933, 16985327162781367875)
  , (5, 1, 8791568787844826341)
  , (5, -1, 5530581260241357745)
  , (5, -1975321933, 3495873312551220181)
  , (7, 1, 5413873776596761492)
  , (7, -1, 6246729677674640592)
  , (7, -1975321933, 6270986846811008315)
  , (8, 1, 18028330578982313052)
  , (8, -1, 183159375591067147)
  , (8, -1975321933, 2440440824345821151)
  , (9, 1, 2765755017150847136)
  , (9, -1, 13323396594358482227)
  , (9, -1975321933, 5013759017102182127)
  , (15, 1, 9006105209421728375)
  , (15, -1, 10265941630696069790)
  , (15, -1975321933, 4808469271382664753)
  , (16, 1, 15009801194993183743)
  , (16, -1, 5366065163824442316)
  , (16, -1975321933, 10300929691505875997)
  , (17, 1, 13572141796604348872)
  , (17, -1, 16970989870592180207)
  , (17, -1975321933, 4112577284569014381)
  , (32, 1, 2032660921473018146)
  , (32, -1, 5039320821111297598)
  , (32, -1975321933, 5965816592367688711)
  , (33, 1, 1531610047254578058)
  , (33, -1, 4107385510519813004)
  , (33, -1975321933, 10862091313873709080)
  , (64, 1, 9025103267374691744)
  , (64, -1, 16264618459812471133)
  , (64, -1975321933, 15824366157123231135)
  , (65, 1, 8824120846568752348)
  , (65, -1, 6050642085171480274)
  , (65, -1975321933, 2338919194933207862)
  , (96, 1, 8767102289223837982)
  , (96, -1, 18030817643322932072)
  , (96, -1975321933, 4189985633641229388)
  , (97, 1, 2527190457253147688)
  , (97, -1, 13742563340613720612)
  , (97, -1975321933, 1480433451195874819)
  , (128, 1, 331519392656242068)
  , (128, -1, 5667826411672408200)
  , (128, -1975321933, 2415857731238043379)
  , (129, 1, 453309587916105570)
  , (129, -1, 4109856138642599305)
  , (129, -1975321933, 1287223038183982767)
  , (200, 1, 7537953097379429138)
  , (200, -1, 10472767651565070741)
  , (200, -1975321933, 18144299193340384384)
  , (240, 1, 14494450549313447364)
  , (240, -1, 9846422352385000867)
  , (240, -1975321933, 6203600517574219333)
  , (241, 1, 14593819655575255035)
  , (241, -1, 17588747408250261187)
  , (241, -1975321933, 17071794465238289861)
  , (500, 1, 16624587938511292689)
  , (500, -1, 10894723100506410005)
  , (500, -1975321933, 12473026683416103720)
  , (1024, 1, 6408010732372468991)
  , (1024, -1, 10436450357262486482)
  , (1024, -1975321933, 11616009711276207868)
  , (1025, 1, 28894826719850699)
  , (1025, -1, 14882445140865926008)
  , (1025, -1975321933, 643664654573511860)
  , (2049, 1, 5642191593259011723)
  , (2049, -1, 15978071289163760601)
  , (2049, -1975321933, 4906679870210788380)
  , (4100, 1, 11668153169390112515)
  , (4100, -1, 11039475803782954398)
  , (4100, -1975321933, 15308205643073476282)
  ]

streamedHashes :: [(Int, Int, Word64, Word64)]
streamedHashes =
  [ (0, 1, 11408090015561836892, 11408090015561836892)
  , (0, -1, 2850470691755143884, 2850470691755143884)
  , (0, -1975321933, 6556819671860081995, 6556819671860081995)
  , (1, 1, 11648718248610840199, 11648718248610840199)
  , (1, -1, 18150152510837721586, 18150152510837721586)
  , (1, -1975321933, 7753665561237223363, 7753665561237223363)
  , (2, 1, 13178306891320286650, 13178306891320286650)
  , (2, -1, 13058548013057319461, 13058548013057319461)
  , (2, -1975321933, 3057935289469769161, 3057935289469769161)
  , (3, 1, 1630506472465396319, 1630506472465396319)
  , (3, -1, 1179377521384580423, 1179377521384580423)
  , (3, -1975321933, 1943119399637882146, 1943119399637882146)
  , (4, 1, 8702889839928853731, 8702889839928853731)
  , (4, -1, 9622375002765921936, 9622375002765921936)
  , (4, -1975321933, 14234769218290359163, 14234769218290359163)
  , (5, 1, 356590276325727671, 356590276325727671)
  , (5, -1, 11383988141337412347, 11383988141337412347)
  , (5, -1975321933, 13695432978510430728, 13695432978510430728)
  , (7, 1, 17766220647958156270, 17766220647958156270)
  , (7, -1, 7853487695145174799, 7853487695145174799)
  , (7, -1975321933, 15537729831476179893, 15537729831476179893)
  , (8, 1, 5392233744477922985, 5392233744477922985)
  , (8, -1, 14902912782805807694, 14902912782805807694)
  , (8, -1975321933, 17894943253901628780, 17894943253901628780)
  , (9, 1, 10056980422668530895, 10056980422668530895)
  , (9, -1, 12768064991438508163, 12768064991438508163)
  , (9, -1975321933, 12001994631902403023, 12001994631902403023)
  , (15, 1, 9067130675013003360, 9067130675013003360)
  , (15, -1, 12935726632616587576, 12935726632616587576)
  , (15, -1975321933, 6442415909436849332, 6442415909436849332)
  , (16, 1, 4452629990885913441, 4452629990885913441)
  , (16, -1, 4222375081877081145, 4222375081877081145)
  , (16, -1975321933, 14017110306095975650, 14017110306095975650)
  , (17, 1, 8389667880646271273, 8389667880646271273)
  , (17, -1, 9150616380547615753, 9150616380547615753)
  , (17, -1975321933, 5088802287854065284, 5088802287854065284)
  , (32, 1, 9934750631831503664, 9934750631831503664)
  , (32, -1, 14640088715334197093, 14640088715334197093)
  , (32, -1975321933, 5621071288185605441, 5621071288185605441)
  , (33, 1, 9496810723267326457, 9496810723267326457)
  , (33, -1, 16761797041046627314, 16761797041046627314)
  , (33, -1975321933, 11670729844025898365, 11670729844025898365)
  , (64, 1, 4968197406753875914, 4968197406753875914)
  , (64, -1, 9685822055361727151, 9685822055361727151)
  , (64, -1975321933, 13555381650316515870, 13555381650316515870)
  , (65, 1, 8213080095477524949, 8213080095477524949)
  , (65, -1, 14410414562998878899, 14410414562998878899)
  , (65, -1975321933, 709788437649739990, 709788437649739990)
  , (96, 1, 1537187243734841110, 1537187243734841110)
  , (96, -1, 14462749959405680016, 14462749959405680016)
  , (96, -1975321933, 4902608695791877047, 4902608695791877047)
  , (97, 1, 9749011903872629673, 9749011903872629673)
  , (97, -1, 2682003119443222766, 2682003119443222766)
  , (97, -1975321933, 4600332929615567982, 4600332929615567982)
  , (128, 1, 11915672279210221873, 11915672279210221873)
  , (128, -1, 12963427971328339594, 12963427971328339594)
  , (128, -1975321933, 14434466746814973396, 14434466746814973396)
  , (129, 1, 5649052176324900924, 5649052176324900924)
  , (129, -1, 8251857178197095031, 8251857178197095031)
  , (129, -1975321933, 8762849572255228993, 8762849572255228993)
  , (200, 1, 5786631235262735523, 5786631235262735523)
  , (200, -1, 11004319332320390530, 11004319332320390530)
  , (200, -1975321933, 2868371108416241228, 2868371108416241228)
  , (240, 1, 9013006966089287440, 9013006966089287440)
  , (240, -1, 14952026416201331793, 14952026416201331793)
  , (240, -1975321933, 8903657820447863303, 8903657820447863303)
  , (241, 1, 15123946983258976788, 15123946983258976788)
  , (241, -1, 6508960893505694050, 6508960893505694050)
  , (241, -1975321933, 732841641666601828, 732841641666601828)
  , (500, 1, 12410773171865192868, 12410773171865192868)
  , (500, -1, 15320778668783907523, 15320778668783907523)
  , (500, -1975321933, 5400891894682236378, 5400891894682236378)
  , (1024, 1, 10923914483667181456, 10923914483667181456)
  , (1024, -1, 2852799983644951159, 2852799983644951159)
  , (1024, -1975321933, 6170364807978936123, 6170364807978936123)
  , (1025, 1, 9986667334111681755, 9986667334111681755)
  , (1025, -1, 659227151046340911, 659227151046340911)
  , (1025, -1975321933, 12165504080106489836, 12165504080106489836)
  , (2049, 1, 8585679352354085434, 8585679352354085434)
  , (2049, -1, 14773364684048826342, 14773364684048826342)
  , (2049, -1975321933, 14696462234894661557, 14696462234894661557)
  , (4100, 1, 14110497749195843949, 14110497749195843949)
  , (4100, -1, 3391451906132668, 3391451906132668)
  , (4100, -1975321933, 1034143305437541897, 1034143305437541897)
  ]

textHashes :: [(Text, Word64, Word64, Word64)]
textHashes =
  [ ("", 3244421341483603138, 3244421341483603138, 5133589608643455977)
  , ("a", 16629034431890738719, 3244421341483603138, 16404057421767308032)
  , ("abc", 8696274497037089104, 2483557330662156479, 14444917244859402470)
  , ("route", 8686557153332962867, 6936481337958806865, 16047437715111950726)
  , ("/vanilla/", 9613742359629870650, 15858182194341394878, 16882537524350024845)
  , ("Halogen", 4077052854781588546, 10745810893312544836, 16542748209693153551)
  , ("za\380\243\322\263 g\281\347l\261 ja\378\324", 4834509733166038713, 17534304236472338437, 10643601913732078375)
  , ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\252", 15645650322186774484, 4828050703365227964, 15761191487625008864)
  , ("Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; Haskell, Halogen; ", 12197493564686484501, 14611311468998955576, 16556271931733448857)
  ]

wordHashes :: [(Word64, Int, Word64, Word64)]
wordHashes =
  [ (0, 0, 14374147212387527897, 5238470482016868669)
  , (0, -1, 2850470691755143884, 11986147470418792584)
  , (1, 0, 3439722301264460078, 15781232456890734344)
  , (1, -1, 8730478022209596988, 17064378983524203240)
  , (3735928559, 0, 5662645884973118465, 5305376981424706319)
  , (3735928559, -1, 79206626409971249, 1793825508934490114)
  , (81985529216486895, 0, 13226496048068982694, 7073500297415602454)
  , (81985529216486895, -1, 5862644779652640949, 14843529884161775523)
  , (18446744073709551615, 0, 5841669975847748627, 14801956013896328748)
  , (18446744073709551615, -1, 15748937971782621674, 6437870620736156622)
  ]
