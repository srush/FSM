{-# LANGUAGE TypeFamilies, FlexibleContexts #-} 
module FSM where 
import Dist
import SemiRing
import qualified Data.Map as M 
import Data.Map ((!))
import Data.Monoid.Multiplicative 

class (Show (State a),
       Eq (State a),
       Ord (State a),
       Show (Symbol a),
       SemiRing (FSMSemiRing a)) => 
      WFSA a where 
    type State a
    type Symbol a
    type FSMSemiRing a  
    initialState :: a -> State a 
    isFinal :: a -> State a -> Bool
    transition ::  a -> State a -> Symbol a -> 
                   Maybe (State a, FSMSemiRing a) 


type SimpleState = Int 
data SimpleFSA sym = SimpleFSA {
      fsaInitial  :: SimpleState,
      fsaFinal :: SimpleState,
      fsaTransitions :: M.Map SimpleState (M.Map sym SimpleState)
}

data WeightedFSA sym semi = WeightedFSA {       
    fsaTransitionWeights :: CondDist SimpleState sym, 
    fsaBase :: SimpleFSA sym
    }

simpleFromAssoc start end assoc =
    SimpleFSA {
  fsaInitial = start,
  fsaFinal = end,
  fsaTransitions = M.fromList $ map (\(st, trans) -> (st, M.fromList trans)) assoc
}
    

instance (Show b, Ord b) =>  WFSA (SimpleFSA b)  where
      type State (SimpleFSA b) = SimpleState 
      type Symbol (SimpleFSA b) = b 
      type FSMSemiRing (SimpleFSA b) = BoolRing 
      initialState = fsaInitial
      isFinal fsa = (== fsaFinal fsa)
      transition fsa fromState symbol = 
        fmap (\r -> (r,one)) (M.lookup symbol (trans ! fromState) ) 
          where 
            trans = fsaTransitions fsa

instance (Ord sym, Show sym, ProbSemiRing semi) =>  WFSA (WeightedFSA sym semi)  where
      type State (WeightedFSA sym semi) = SimpleState 
      type Symbol (WeightedFSA sym semi) = sym 
      type FSMSemiRing (WeightedFSA sym semi) = semi 
      initialState = fsaInitial . fsaBase
      isFinal fsa = (== (fsaFinal $ fsaBase fsa))
      transition wfsa fromState symbol = do
        nextState <- M.lookup symbol (trans ! fromState)  
        return (nextState, fromDouble $ condProb dist fromState symbol ) 
          where 
            fsa = fsaBase wfsa
            dist = fsaTransitionWeights wfsa 
            trans = fsaTransitions fsa

