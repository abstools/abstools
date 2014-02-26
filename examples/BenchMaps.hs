{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
 
class (Object_ a) => IArray_ a where
         
        sum :: ObjectRef a -> ABS a Int
 
type IArray = forall a . (IArray_ a) => ObjectRef a
 
data Array = Array{array_loc :: (Object_ o) => ABS o COG,
                   array_n :: Int, array_i :: Int, array_s :: Int,
                   array_m :: Map Int Int}
array n = Array{array_n = n}
 
instance Object_ Array where
        new __cont
          = do __chan <- lift (lift newChan)
               let __i = 0
               let __s = 0
               let __m = empty
               let __c
                     = __cont{array_i = __i, array_s = __s, array_m = __m,
                              array_loc = return __chan}
               __ioref <- lift (lift (newIORef __c))
               let __obj = ObjectRef __ioref 0
               lift (lift (spawnCOG __chan))
               __obj `async_call` __init
               __obj `async_call` run
               return __obj
        new_local __cont
          = do let __i = 0
               let __s = 0
               let __m = empty
               let __c
                     = __cont{array_i = __i, array_s = __s, array_m = __m,
                              array_loc = thisCOG}
               __ioref <- lift (lift (newIORef __c))
               __astate@(AState{aCounter = __counter}) <- lift RWS.get
               lift (RWS.put (__astate{aCounter = __counter + 1}))
               let __obj = ObjectRef __ioref __counter
               __obj `sync_call` __init
               __obj `sync_call` run
               return __obj
        whereis = array_loc
        __init this
          = do while
                 (readObject this >>=
                    \ Array{array_i = __i, array_n = __n} -> return (__i <= __n))
                 (do do a <- readObject this >>=
                               \ Array{array_i = __i, array_n = __n} -> return (__i * __n)
                        set_array_m =<<
                          (readObject this >>=
                             \ Array{array_m = __m, array_i = __i} -> return (put __m __i a))
                        set_array_i =<<
                          (readObject this >>= \ Array{array_i = __i} -> return (__i + 1))
                        return ())
 
set_array_n :: Int -> ABS Array ()
set_array_n v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{array_n = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 0) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_array_i :: Int -> ABS Array ()
set_array_i v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{array_i = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 1) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_array_s :: Int -> ABS Array ()
set_array_s v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{array_s = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 2) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_array_m :: Map Int Int -> ABS Array ()
set_array_m v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{array_m = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 3) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
instance IArray_ Array where
        sum this
          = do set_array_i =<< (return 0)
               while
                 (readObject this >>=
                    \ Array{array_i = __i, array_n = __n} -> return (__i <= __n))
                 (do do set_array_s =<<
                          (readObject this >>=
                             \ Array{array_s = __s, array_m = __m, array_i = __i} ->
                               return (__s + lookupUnsafe __m __i))
                        set_array_i =<<
                          (readObject this >>= \ Array{array_i = __i} -> return (__i + 1))
                        return ())
               readObject this >>= \ Array{array_s = __s} -> return __s
mainABS
  = do a1 <- new (array 1000)
       a2 <- new (array 1000)
       a3 <- new (array 1000)
       a4 <- new (array 1000)
       f1 <- a1 `async_call` sum
       f2 <- a2 `async_call` sum
       f3 <- a3 `async_call` sum
       f4 <- a4 `async_call` sum
       res1 <- get (return f1)
       res2 <- get (return f2)
       res3 <- get (return f3)
       res4 <- get (return f4)
       assert (return (res1 == 500500000))
       assert (return (res2 == 500500000))
       assert (return (res3 == 500500000))
       assert (return (res4 == 500500000))
main = main_is mainABS