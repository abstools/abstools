{-# LANGUAGE Rank2Types, NoImplicitPrelude,
  ExistentialQuantification, MultiParamTypeClasses #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import Prim
import ABSPrelude
 
class (Object__ a) => IArray_ a where
         
        sum :: IArray -> ABS a Int
 
data IArray = forall a . (IArray_ a) => IArray (ObjectRef a)
 
instance Sub IArray IArray where
        up x = x
 
instance Sub IArray AnyObject where
        up (IArray a) = AnyObject a
sum_sync (__wrapped@(IArray __obj@(ObjectRef __ioref _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- whereis __obj1
       when (__hereCOG /= otherCOG)
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (sum __wrapped)
sum_async (__wrapped@(IArray __obj@(ObjectRef __ioref _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       __loc <- whereis __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __tid cog __counter
       lift (lift (writeChan __loc (RunJob __obj __f (sum __wrapped))))
       return __f
 
data Array = Array{array_loc :: (Object__ o) => ABS o COG,
                   array_n :: Int, array_x :: Int, array_i :: Int, array_s :: Int,
                   array_m :: Map Int Int}
__array n = Array{array_n = n}
 
instance Object__ Array where
        new __cont
          = do __chan <- lift (lift newChan)
               let __x = 10
               let __i = 0
               let __s = 0
               let __m = empty
               let __c
                     = __cont{array_x = __x, array_i = __i, array_s = __s,
                              array_m = __m, array_loc = return __chan}
               __ioref <- lift (lift (newIORef __c))
               let __obj = ObjectRef __ioref 0
               lift (lift (spawnCOG __chan))
               do __mvar <- lift (lift newEmptyMVar)
                  AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
                  astate@(AState{aCounter = __counter}) <- lift RWS.get
                  lift (RWS.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __tid cog __counter
                  lift
                    (lift
                       (writeChan __chan (RunJob __obj __f (__init (AnyObject __obj)))))
               do __mvar <- lift (lift newEmptyMVar)
                  AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
                  astate@(AState{aCounter = __counter}) <- lift RWS.get
                  lift (RWS.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __tid cog __counter
                  lift
                    (lift
                       (writeChan __chan (RunJob __obj __f (__run (AnyObject __obj)))))
               return __obj
        new_local __cont
          = do let __x = 10
               let __i = 0
               let __s = 0
               let __m = empty
               let __c
                     = __cont{array_x = __x, array_i = __i, array_s = __s,
                              array_m = __m, array_loc = thisCOG}
               __ioref <- lift (lift (newIORef __c))
               __astate@(AState{aCounter = __counter}) <- lift RWS.get
               lift (RWS.put (__astate{aCounter = __counter + 1}))
               let __obj = ObjectRef __ioref __counter
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__init (AnyObject __obj))
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__run (AnyObject __obj))
               return __obj
        whereis = array_loc
        __init this
          = do while
                 (readThis >>=
                    \ Array{array_i = __i, array_x = __x} -> return (__i < __x))
                 (do do a <- (readThis >>=
                                \ Array{array_i = __i, array_n = __n} -> return (__i * __n))
                               :: ABS Array Int
                        (set_array_m =<<
                           ((readThis >>=
                               \ Array{array_m = __m, array_i = __i} ->
                                 return (put __m __i (a)))))
                        (set_array_i =<<
                           ((readThis >>= \ Array{array_i = __i} -> return (__i + 1))))
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
 
set_array_x :: Int -> ABS Array ()
set_array_x v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{array_x = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 1) om
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
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 2) om
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
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 3) om
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
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 4) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
instance IArray_ Array where
        sum this
          = do (set_array_i =<< (return 0))
               while
                 (readThis >>=
                    \ Array{array_i = __i, array_x = __x} -> return (__i < __x))
                 (do do (set_array_s =<<
                           ((readThis >>=
                               \ Array{array_s = __s, array_m = __m, array_i = __i} ->
                                 return (__s + lookupUnsafe __m __i))))
                        (set_array_i =<<
                           ((readThis >>= \ Array{array_i = __i} -> return (__i + 1))))
                        return ())
               (readThis >>= \ Array{array_s = __s} -> return __s)
mainABS
  = do a3 <- liftM IArray (new_local (__array 100)) :: ABS Top IArray
       x <- (sum_sync (up a3)) :: ABS Top Int
       assert (return ((x) == 4500))
main = main_is mainABS