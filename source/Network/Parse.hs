{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Parse where

import Network.Socket
import Network.Socket.Internal
import Text.Parsec
import Control.Applicative ((<$>))
import Control.Monad
import Data.Word
import Data.Function
import Numeric
import Data.Bits

type Octets = (Word8, Word8, Word8, Word8)
type Groups = (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
-- | Convert octetes to HostAddress
--
-- example: @toHostAddress (127,0,0,1) -- localhost@
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
hostAddressToOct :: Word32 -> Octets
hostAddressToOct h = ( fromIntegral $ h `shiftR` (0*8) .&. 0xff
                     , fromIntegral $ h `shiftR` (1*8) .&. 0xff
                     , fromIntegral $ h `shiftR` (2*8) .&. 0xff
                     , fromIntegral $ h `shiftR` (3*8) .&. 0xff
                     )

-- | Read IPv4 address in the dotted decimal format
--
-- Port is set to 0 if not given
--
-- example: @readIPv4 "127.0.0.1:80"@
readIPv4 :: String -> Maybe SockAddr
readIPv4 x = case parse ipv4P "" x of
    Left _ -> Nothing
    Right ((i1, i2, i3, i4), port ) -> Just $
                              SockAddrInet (maybe 0 fromIntegral port)
                                            (octToHostAddress (i1, i2, i3, i4))

-- | Read IPv6 addres
--
-- example : @readIPv6 "[::1]:8080"@
readIPv6 x = case parse ipv6P "" x of
    Left _ -> Nothing
    Right (groups, port) -> Just $ SockAddrInet6 (maybe 0 fromIntegral port) 0
                                                 (groupsToHostAddress6 groups) 0

-- | Read an IP addres. It may be either an IPv4 address in the dotted decimal
-- format or an ipv6 address
readIP :: String -> Maybe SockAddr
readIP x = case parse p "" x of
    Left _ -> Nothing
    Right r -> Just r
  where
     p = ((\(hs, p) -> SockAddrInet (maybe 0 fromIntegral p)
                           (octToHostAddress hs )) <$> ipv4P)
                       <|> ((\(hs, p) -> SockAddrInet6 (maybe 0 fromIntegral p) 0
                           (groupsToHostAddress6 hs) 0 ) <$> ipv6P)

-- | Convert hexadecimal groups to HostAddress6
groupsToHostAddress6 :: Groups -> HostAddress6
groupsToHostAddress6 (g1, g2, g3, g4 , g5, g6, g7, g8) =
    ( fromIntegral g1 `shiftL` 16 .|. fromIntegral g2
    , fromIntegral g3 `shiftL` 16 .|. fromIntegral g4
    , fromIntegral g5 `shiftL` 16 .|. fromIntegral g6
    , fromIntegral g7 `shiftL` 16 .|. fromIntegral g8
    )

number :: (Stream s m Char, Integral a, Bounded a) => ParsecT s u m a
number = fix $ \x -> do
    i <- read <$> many1 digit
    mb <- return maxBound `asTypeOf` x
    if i > fromIntegral mb
        then fail "Number too large"
        else return $ fromIntegral i

-- | Dotted decimal representation
ipv4P  :: Stream s m Char => ParsecT s u m (Octets, Maybe Word16)
ipv4P = do
    d1 <- number
    [d2, d3, d4] <- replicateM 3 $ (char '.') >> number
    port <- (try $ do
        char ':'
        Just <$> number)
             <|> return Nothing
    return ((d1, d2, d3, d4), port)

ipv6bareP :: Stream s m Char => ParsecT s u m Groups
ipv6bareP = do
    hs1 <- sepBy hex4 (try $ char ':' >> notFollowedBy (char ':'))
    mbhs2 <- (string "::" >> Just <$> sepBy hex4 (char ':')) <|> (return Nothing)
    case mbhs2 of
        Nothing -> if length hs1 /= 8
                   then fail "wrong number of groups"
                   else return $ toGroups hs1
        Just hs2 -> let l = length hs1 + length hs2 in
            if l > 7
               then fail "wrong number of groups"
               else return . toGroups $ hs1 ++ replicate (8-l) 0 ++ hs2
  where
    hex4 = do
        h <- many1 hexDigit
        when (length h > 4) $ fail "group has more than 4 characters"
        case readHex h of
            ((h', ""):_) -> return h'
            _ -> fail "not a hexadecimal"
    toGroups [i1, i2, i3, i4, i5, i6, i7, i8] = (i1, i2, i3, i4, i5, i6, i7, i8)

ipv6P :: Stream s m Char => ParsecT s u m (Groups, Maybe Word16)
ipv6P = do
    char '['
    ip <- ipv6bareP
    char ']' >> char ':'
    port <- number
    return (ip, Just port :: Maybe Word16)
  <|> do
    ip <- ipv6bareP
    return (ip, Nothing)
