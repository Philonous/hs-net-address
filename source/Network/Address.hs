{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Address
  (  -- * Reading / Showing
    readIPv6
  , readIPv4
  , readIP
  , showHostAddress6
  , showHostAddress
  , showIP
    -- * Octets
  , Octets
  , octToHostAddress
  , octToSockAddr
  , hostAddressToOct
    -- * Groups
  , Groups
  , groupsToHostAddress6
  , hostAddress6ToGroups
    -- * Parsers
  , Port
  , ipv4BareP
  , ipv4P
  , ipv6BareP
  , ipv6P
  ) where

import Network.Socket
import Text.Parsec
import Control.Applicative ((<$>))
import Control.Monad
import Data.Word
import Data.Function
import Numeric
import Data.Bits
import Data.List

type Octets = (Word8, Word8, Word8, Word8)
type Groups = (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
type Port = Word16
-- | Convert octetes to HostAddress
--
-- example: @octToHostAddress (127,0,0,1) -- localhost@
octToHostAddress :: Octets -> HostAddress
octToHostAddress (i1, i2, i3, i4) =
    fromIntegral i4 `shiftL` (3*8) .|.
    fromIntegral i3 `shiftL` (2*8) .|.
    fromIntegral i2 `shiftL` (1*8) .|.
    fromIntegral i1 `shiftL` (0*8)

-- | Construct IPv4 SockAddr from Octets
octToSockAddr :: Octets -> SockAddr
octToSockAddr = SockAddrInet 0 . octToHostAddress

-- | Deconstruct HostAddress into Octets
hostAddressToOct :: HostAddress -> Octets
hostAddressToOct h = ( fromIntegral $ h `shiftR` (0*8) .&. 0xff
                     , fromIntegral $ h `shiftR` (1*8) .&. 0xff
                     , fromIntegral $ h `shiftR` (2*8) .&. 0xff
                     , fromIntegral $ h `shiftR` (3*8) .&. 0xff
                     )

-- | Show IPv4 address in dotted decimal representation
showHostAddress :: HostAddress -> String
showHostAddress ha = let (o1, o2, o3, o4) = hostAddressToOct ha in
    intercalate "." $ map show [o1, o2, o3, o4]

-- | Print SockAddrInet or SockAddrInet6 address. Port and scope id are left out
-- if set to 0.
--
-- Same as 'show' for all other address types (e.g. IPC)
showIP :: SockAddr -> String
showIP (SockAddrInet p ha) = let addr = showHostAddress ha in
    if (p == 0)
    then addr
    else addr ++ ":" ++ show p
showIP (SockAddrInet6 p _ ha sid) = let
        addr = showHostAddress6 ha
        addrSid = if sid == 0
                  then addr
                  else addr ++ "%" ++ show sid
    in if (p == 0)
       then addrSid
       else concat [ "[", addrSid, "]:" ++ show p]
showIP x = show x

-- | Show canonic representation of IPv6 address
-- (longest, most leftmost streak of more than one zeroes replaced by "::")
showHostAddress6 :: HostAddress6 -> String
showHostAddress6 ha = let
                  (g1, g2, g3, g4, g5, g6, g7, g8) = hostAddress6ToGroups ha
                  gs = map fromIntegral [g1, g2, g3, g4, g5, g6, g7, g8]
                  -- Find length m of longest consecutive 0-streak:
                  m = maximum . map (either (const 0) id) $ compressRuns 0 0 gs
                  -- Compress first streak that has length of at least m
                  compressed = compressRuns m 0 gs
              in  showRun compressed
  where
    -- compress runs if run length is greater than m. If we find such a run with
    -- length n we increase the minimum run length for the remaining list to
    -- (n+1) Used twice: once to find the greatest run length and once to
    -- compress it
    compressRuns :: Int -> Int -> [Int] -> [Either Int Int]
    compressRuns _ 0 [] = []
    compressRuns m 0 (0 : 0 : xs) = compressRuns m 2 (xs)
    compressRuns m 0 (0 : xs) = Left 0 : compressRuns m 0 (xs)
    compressRuns m n (0 : xs) = compressRuns m (n+1) (xs)
    compressRuns m 0 (x:xs) = Left x : compressRuns m 0 xs
    compressRuns m n xs = if m <= n
                            then Right n : compressRuns (n+1) 0 xs
                            else ((replicate n (Left 0))) ++ compressRuns m 0 xs
    showRun [] = error "wrong number of groups"
    showRun (Right _ : xs) = "::" ++ go xs
    showRun xs = go xs
    go [] = ""
    go (Right _ : xs)  = ":" ++ go xs
    go [Left n] = showHex n ""
    go (Left n : xs) = showHex n ""++ ":" ++ go xs

-- | Read IPv4 address in the dotted decimal format
--
-- Port is set to 0 if not given
--
-- example: @readIPv4 \"127.0.0.1:80\"@
readIPv4 :: String -> Maybe SockAddr
readIPv4 x = case parse ipv4P "" x of
    Left _ -> Nothing
    Right ((i1, i2, i3, i4), port ) -> Just $
                              SockAddrInet (maybe 0 fromIntegral port)
                                            (octToHostAddress (i1, i2, i3, i4))

-- | Read IPv6 addres according to RFC 4291
--
-- Scope IDs are accepted only in numerical format
--
-- Port and scope id are set to 0 if not given, flow info is always set to 0
--
-- Examples
--
-- * compressed format with port:
--
-- @readIPv6 \"[::1]:8080\"@
--
-- * mixed address format:
--
-- @readIPv6 \"0:0:0:0:0:FFFF:129.144.52.38\"@
--
-- * with scope id:
--
-- @readIPv6 \"FF01::101%3\"@
readIPv6 :: String -> Maybe SockAddr
readIPv6 x = case parse ipv6P "" x of
    Left _ -> Nothing
    Right (groups, port, scopeID) -> Just $
                                     SockAddrInet6
                                       (maybe 0 fromIntegral port)
                                       0
                                       (groupsToHostAddress6 groups)
                                       (maybe 0 id scopeID)

-- | Read an IP addres. Accepts IPv4 addresses in dotted decimal format and
-- IPv6 addresses according to RFC 4291, either with or without port or scope id
-- (set to 0 if not given)
--
-- See also 'readIPv6' and 'readIPv4'
readIP :: String -> Maybe SockAddr
readIP x = case parse par "" x of
    Left _ -> Nothing
    Right r -> Just r
  where
     par = ((\(hs, p) -> SockAddrInet (maybe 0 fromIntegral p)
                           (octToHostAddress hs )) <$> ipv4P)
                       <|> ((\(hs, p, s ) -> SockAddrInet6
                                               (maybe 0 fromIntegral p)
                                               0
                                               (groupsToHostAddress6 hs)
                                               (maybe 0 id s)) <$> ipv6P)

-- | Convert hexadecimal groups to HostAddress6
groupsToHostAddress6 :: Groups -> HostAddress6
groupsToHostAddress6 (g1, g2, g3, g4 , g5, g6, g7, g8) =
    ( fromIntegral g1 `shiftL` 16 .|. fromIntegral g2
    , fromIntegral g3 `shiftL` 16 .|. fromIntegral g4
    , fromIntegral g5 `shiftL` 16 .|. fromIntegral g6
    , fromIntegral g7 `shiftL` 16 .|. fromIntegral g8
    )

hostAddress6ToGroups :: HostAddress6 -> Groups
hostAddress6ToGroups (h1, h2, h3, h4) =
    ( fromIntegral $ h1 `shiftR` 16
    , fromIntegral $ h1 .&. 0xffff
    , fromIntegral $ h2 `shiftR` 16
    , fromIntegral $ h2 .&. 0xffff
    , fromIntegral $ h3 `shiftR` 16
    , fromIntegral $ h3 .&. 0xffff
    , fromIntegral $ h4 `shiftR` 16
    , fromIntegral $ h4 .&. 0xffff
    )

number :: (Stream s m Char, Integral a, Bounded a) => ParsecT s u m a
number = fix $ \x -> do
    i <- read <$> many1 digit
    mb <- return maxBound `asTypeOf` x
    if i > fromIntegral mb
        then fail "Number too large"
        else return $ fromIntegral (i :: Word32)

ipv4BareP :: ( Stream s m Char) =>
             ParsecT s u m Octets
ipv4BareP  = do
    d1 <- number
    [d2, d3, d4] <- replicateM 3 $ (char '.') >> number
    return (d1, d2, d3, d4)

-- | Dotted decimal representation
ipv4P  :: Stream s m Char => ParsecT s u m (Octets, Maybe Port)
ipv4P = do
    bare <- ipv4BareP
    port <- (try $ do
        void $ char ':'
        Just <$> number)
             <|> return Nothing
    return (bare, port)

ipv6BareP :: Stream s m Char => ParsecT s u m Groups
ipv6BareP = do
    hs1 <- sepBy hex4 (try $ char ':' >> notFollowedBy (char ':'))
    mbhs2 <- optionMaybe $ (string "::" >> sepBy hex4 (char ':'))
    mbOctets <- optionMaybe $ ipv4BareP
    let (compressed, hs2) = case mbhs2 of
            Nothing -> (False, [])
            Just jhs2 -> (True, jhs2)
    let v4Part = case mbOctets of
            Nothing -> []
            Just (o1, o2, o3, o4) -> [ fromIntegral o1 `shiftL` 8
                                       .|. fromIntegral o2
                                     , fromIntegral o3 `shiftL` 8
                                       .|. fromIntegral o4
                                     ]
    let l = sum . map length $ [hs1, hs2, v4Part]
    if (l > 8 || (l < 8 && not compressed))
        then fail "wrong number of groups"
        else return . toGroups $ concat [ hs1
                                        , replicate (8-l) 0
                                        , hs2
                                        , v4Part
                                        ]
  where
    hex4 = try $ do
        h <- many1 hexDigit
        notFollowedBy $ char '.'
        when (length h > 4) $ fail "group has more than 4 characters"
        case readHex h of
            ((h', ""):_) -> return h'
            _ -> fail "not a hexadecimal"
    toGroups [i1, i2, i3, i4, i5, i6, i7, i8] = (i1, i2, i3, i4, i5, i6, i7, i8)
    toGroups _ = error "Wrong number of groups"

ipv6P :: Stream s m Char => ParsecT s u m (Groups, Maybe Port, Maybe ScopeID)
ipv6P = do
    bracket <- (char '[' >> return True) <|> return False
    ip <- ipv6BareP
    scopeID <- optionMaybe (char '%' >> number)
    port <- if bracket
      then  char ']' >> char ':' >> Just <$> number
      else return Nothing
    return (ip, port, scopeID)
