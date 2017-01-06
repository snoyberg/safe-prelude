{-# LANGUAGE CPP #-}
-- TODO Consider replacing all IO-specific functions being reexported
-- to MonadIO
module SafePrelude
    ( -- * Types
      Maybe (..)
    , Ordering (..)
    , Bool (..)
    , Char
    , IO
    , Either (..)
    , ByteString
    , Text
    , Map
    , HashMap
    , IntMap
    , Set
    , HashSet
    , IntSet
    , Seq
    , Identity (..)
    , SomeException (..)
    , SomeAsyncException (..)
    -- very grudgingly
    , String
    , IO.FilePath
      -- ** Numbers
    , Word
    , Word8
    , Word16
    , Word32
    , Word64
    , Int
    , Int8
    , Int16
    , Int32
    , Int64
    , Integer
    , Rational
    , Float
    , Double
    , Proxy (..)
      -- * Type classes
    , Ord (..)
    , Eq (..)
    , Bounded (..)
    , Show (..)
    , Read (..)
    , Functor (fmap, (<$))
    , Applicative (pure, (<*>), (*>), (<*))
    , Alternative (empty, (<|>), some, many)
    , Monad ((>>=), (>>), return, fail)
    , MonadIO (liftIO)
    , MonadTrans (lift)
    , MonadReader (ask, local, reader)
    , MonadThrow
    , Exception (toException, fromException)
    , MonadCatch
    , MonadMask
    , Foldable (fold, foldMap, foldr, foldr', foldl, foldl')
    -- separate from the type class in earlier bases
    , toList, null, length, elem
    , Traversable (traverse, sequenceA)
    , Typeable
    , IsString (..)
    , Hashable (..)
    , Semigroup (..)
    , Monoid (..)
      -- ** Numeric
    , Num (..)
    , Real (..)
    , Integral (..)
    , Fractional (..)
    , Floating (..)
    , RealFrac (..)
    , RealFloat (..)
      -- * Functions
    , (Prelude.$)
    , (&)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
    , (Prelude..)
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Prelude.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.odd
    , Prelude.even
    , Prelude.uncurry
    , Prelude.curry
    , Prelude.asTypeOf
    , Prelude.seq
    , fix
      -- ** Numeric
    , (^)
    , (^^)
    , subtract
    , fromIntegral
    , realToFrac
      -- ** Foldable
    , sum
    , product
    , foldrM
    , foldlM
    , traverse_
    , for_
    , sequenceA_
    , asum
    , SafePrelude.mapM_
    , SafePrelude.forM_
    , SafePrelude.sequence_
    , msum
    , concat
    , concatMap
    , and
    , or
    , any
    , all
    , notElem
    , find
      -- ** Traversable
    , SafePrelude.mapM
    , SafePrelude.sequence
    , for
    , SafePrelude.forM
    , mapAccumL
    , mapAccumR
      -- ** Functor
    , ($>)
    , (<$>)
    , void
      -- ** Applicative
    , liftA
    , liftA2
    , liftA3
      -- ** Alternative
    , optional
      -- ** Monad
    , (=<<)
    , (>=>)
    , (<=<)
    , forever
    , join
    , foldM
    , foldM_
    , replicateM_
    , guard
    , when
    , unless
    , liftM
    , ap
    , (<$!>)
      -- ** Concurrent
    , threadDelay
    , MVar
    , newEmptyMVar
    , newMVar
    , takeMVar
    , putMVar
    , readMVar
    , swapMVar
    , tryTakeMVar
    , tryPutMVar
    , isEmptyMVar
    , withMVar
    , withMVarMasked
    , modifyMVar_
    , modifyMVar
    , modifyMVarMasked_
    , modifyMVarMasked
    , tryReadMVar
    , mkWeakMVar
    , Chan
    , newChan
    , writeChan
    , readChan
    , dupChan
      -- ** Reader
    , asks
      -- ** Exceptions
    , throwIO
    , throwM
    , throwTo
    , catch
    , catchIO
    , catchAny
    , catchDeep
    , catchAnyDeep
    , handle
    , handleIO
    , handleAny
    , handleDeep
    , handleAnyDeep
    , try
    , tryIO
    , tryAny
    , tryDeep
    , tryAnyDeep
    , onException
    , bracket
    , bracket_
    , finally
    , withException
    , bracketOnError
    , bracketOnError_
    , displayException
      -- ** Arrow
    , (&&&)
    , (***)
      -- ** Maybe
    , mapMaybe
    , catMaybes
    , fromMaybe
    , isJust
    , isNothing
    , listToMaybe
      -- ** Either
    , partitionEithers
    , lefts
    , rights
      -- ** Ord
    , on
    , comparing
      -- ** Say
    , say
    , sayString
    , sayShow
    , sayErr
    , sayErrString
    , sayErrShow
    , hSay
    , hSayString
    , hSayShow
      -- ** IORef
    , IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef
    , modifyIORef'
    , atomicModifyIORef
    , atomicModifyIORef'
    , atomicWriteIORef
    , mkWeakIORef
      -- ** IO
    , IO.Handle
    , IO.IOMode (..)
    , IO.stdin
    , IO.stdout
    , IO.stderr
    , IO.hClose
    , IO.withBinaryFile
    , readFile
    , writeFile
    , readFileUtf8
    , writeFileUtf8
      -- ** Character encoding
    , encodeUtf8
    , decodeUtf8
      -- ** deepseq
    , NFData (rnf)
    , deepseq
    , ($!!)
    , force
      -- ** Monoids
    , (++)
      -- ** Read
    , readMaybe
    , readEither
    ) where

import Control.Exception.Safe
import Data.Maybe
import Data.Function
import Data.Ord
import Data.Either
import Control.Arrow
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Text (Text)
import Data.Foldable hiding (sum, product, mapM_, forM_, sequence_, msum)
import Data.Traversable hiding (mapM, sequence)
import Data.String
import Data.Int
import Data.Word
import Prelude (Maybe (..), Ordering (..), Bool (..), Char, IO, Either (..), Integer, Rational, Float, Double, Ord (..), Eq (..), Bounded (..), Show (..), Read (..), Num (..), Real (..), Integral (..), Fractional (..), Floating (..), RealFrac (..), RealFloat (..), (^), (^^), subtract, fromIntegral, realToFrac)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.HashSet (HashSet)
import Data.Sequence (Seq)
import Control.Applicative
import Control.Monad
import Data.Functor
import Control.Concurrent hiding (throwTo)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Hashable
import qualified Prelude
import Data.Monoid hiding ((<>))
import Say
import Data.Semigroup
import Text.Read
import Data.Typeable
import Data.IORef
import qualified System.IO as IO
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Control.DeepSeq

-- | Get the sum of the elements in a 'Foldable'.
--
-- This is not the same as the function from 'Data.Foldable'; instead,
-- this function uses a strict left fold.
--
-- @since 0.1.0.0
sum :: (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0

-- | Get the product of the elements in a 'Foldable'.
--
-- This is not the same as the function from 'Data.Foldable'; instead,
-- this function uses a strict left fold.
--
-- @since 0.1.0.0
product :: (Foldable f, Num a) => f a -> a
product = foldl' (*) 1

-- | Operator version of 'mappend'.
--
-- In base, this operator is known as '<>'. However, this is the name
-- of the operator for 'Semigroup' as well. Once 'Semigroup' is a
-- superclass of 'Monoid', this historical accident will be
-- unimportant. In the meanwhile, 'SafePrelude' deals with this
-- situation by making '<>' the 'Semigroup' operator, and '++' the
-- 'Monoid' operator.
--
-- @since 0.1.0.0
(++) :: Monoid m => m -> m -> m
(++) = mappend
{-# INLINE (++) #-}
infixr 5  ++

-- | A total function for decoding a 'ByteString' into 'Text' using a
-- UTF-8 character encoding. This uses 'lenientDecode' in the case of
-- any encoding errors.
--
-- @since 0.1.0.0
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

-- | Read a file assuming a UTF-8 character encoding.
--
-- This leverages 'decodeUtf8', so in the event of a character
-- encoding issue, replacement characters will be used.
--
-- @since 0.1.0.0
readFileUtf8 :: MonadIO m => IO.FilePath -> m Text
readFileUtf8 = liftIO . fmap decodeUtf8 . readFile

-- | Write a file using a UTF-8 character encoding.
--
-- @since 0.1.0.0
writeFileUtf8 :: MonadIO m => IO.FilePath -> Text -> m ()
writeFileUtf8 fp = liftIO . writeFile fp . encodeUtf8

-- | Synonym for 'traverse_'; different from base to generalize to
-- 'Applicative'.
--
-- @since 0.1.0.0
mapM_ :: (Applicative m, Foldable f) => (a -> m b) -> f a -> m ()
mapM_ = traverse_


-- | Flipped version of 'mapM_'.
--
-- @since 0.1.0.0
forM_ :: (Applicative m, Foldable f) => f a -> (a -> m b) -> m ()
forM_ = for_

-- | Synonym for 'sequence_'; different from base to generalize to
-- 'Applicative'.
--
-- @since 0.1.0.0
sequence_ :: (Applicative m, Foldable f) => f (m a) -> m ()
sequence_ = sequenceA_

-- | Synonym for 'traverse'; different from base to generalize to
-- 'Applicative'.
--
-- @since 0.1.0.0
mapM :: (Applicative m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapM = traverse

-- | Flipped version of 'mapM'.
--
-- @since 0.1.0.0
forM :: (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
forM = for

-- | Synonym for 'sequenceA'; different from base to generalize to
-- 'Applicative'.
--
-- @since 0.1.0.0
sequence :: (Applicative m, Traversable t) => t (m a) -> m (t a)
sequence = sequenceA

#if !MIN_VERSION_base(4, 8, 0)
-- Copied straight from base

-- | Test whether the structure is empty. The default implementation is
-- optimized for structures that are similar to cons-lists, because there
-- is no general way to do better.
null :: t a -> Bool
null = foldr (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.  The
-- default implementation is optimized for structures that are similar to
-- cons-lists, because there is no general way to do better.
length :: t a -> Int
length = foldl' (\c _ -> c+1) 0

-- | '&' is a reverse application operator.  This provides notational
-- convenience.  Its precedence is one higher than that of the forward
-- application operator '$', which allows '&' to be nested in '$'.
--
-- @since 4.8.0.0
(&) :: a -> (a -> b) -> b
x & f = f x


-- | Strict version of 'Data.Functor.<$>'.
--
-- @since 4.8.0.0
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
#endif
