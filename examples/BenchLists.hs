{-# LANGUAGE Rank2Types, NoImplicitPrelude,
  ExistentialQuantification, MultiParamTypeClasses #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import Prim
import ABSPrelude
 
data Map_ a b = EmptyMap_
              | InsertAssoc_ (Pair a b) (Map_ a b)
              deriving Eq
put_ ms k v
  = case ms of
        EmptyMap_ -> InsertAssoc_ (k, v) EmptyMap_
        InsertAssoc_ (k_, v_) ts -> if (k == k_) then
                                      InsertAssoc_ (k_, v) ts else
                                      InsertAssoc_ (k_, v_) (put_ ts k v)
lookupUnsafe_ ms k = fromJust (lookup_ ms k)
lookup_ ms k
  = case ms of
        InsertAssoc_ (k_, y) tm -> if (k == k_) then Just y else
                                     lookup_ tm k
        EmptyMap_ -> Nothing
 
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
                   array_n :: Int, array_i :: Int, array_s :: Int,
                   array_m :: Map_ Int Int}
__array n = Array{array_n = n}
 
instance Object__ Array where
        new __cont
          = do __chan <- lift (lift newChan)
               let __i = 0
               let __s = 0
               let __m = EmptyMap_
               let __c
                     = __cont{array_i = __i, array_s = __s, array_m = __m,
                              array_loc = return __chan}
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
          = do let __i = 0
               let __s = 0
               let __m = EmptyMap_
               let __c
                     = __cont{array_i = __i, array_s = __s, array_m = __m,
                              array_loc = thisCOG}
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
                    \ Array{array_i = __i, array_n = __n} -> return (__i <= __n))
                 (do do a <- (readThis >>=
                                \ Array{array_i = __i, array_n = __n} -> return (__i * __n))
                               :: ABS Array Int
                        (set_array_m =<<
                           ((readThis >>=
                               \ Array{array_m = __m, array_i = __i} ->
                                 return (put_ __m __i (a)))))
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
 
set_array_m :: Map_ Int Int -> ABS Array ()
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
          = do (set_array_i =<< (return 0))
               while
                 (readThis >>=
                    \ Array{array_i = __i, array_n = __n} -> return (__i <= __n))
                 (do do (set_array_s =<<
                           ((readThis >>=
                               \ Array{array_s = __s, array_m = __m, array_i = __i} ->
                                 return (__s + lookupUnsafe_ __m __i))))
                        (set_array_i =<<
                           ((readThis >>= \ Array{array_i = __i} -> return (__i + 1))))
                        return ())
               (readThis >>= \ Array{array_s = __s} -> return __s)
mainABS
  = do a1 <- liftM IArray (new (__array 1000)) :: ABS Top IArray
       a2 <- liftM IArray (new (__array 1000)) :: ABS Top IArray
       a3 <- liftM IArray (new (__array 1000)) :: ABS Top IArray
       a4 <- liftM IArray (new (__array 1000)) :: ABS Top IArray
       f1 <- (sum_async (up a1)) :: ABS Top (Fut Int)
       f2 <- (sum_async (up a2)) :: ABS Top (Fut Int)
       f3 <- (sum_async (up a3)) :: ABS Top (Fut Int)
       f4 <- (sum_async (up a4)) :: ABS Top (Fut Int)
       res1 <- get (f1) :: ABS Top Int
       res2 <- get (f2) :: ABS Top Int
       res3 <- get (f3) :: ABS Top Int
       res4 <- get (f4) :: ABS Top Int
       assert (return ((res1) == 500500000))
       assert (return ((res2) == 500500000))
       assert (return ((res3) == 500500000))
       assert (return ((res4) == 500500000))
main = main_is mainABS