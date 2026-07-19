module Games.Blackjack.Play where

import Games.Blackjack.Monads
import Control.Monad.State (execStateT, StateT)


playBlackjack :: BlackjackState -> IO ()
playBlackjack initialState = do
    putStrLn "Starting Blackjack"
    finalState <- execStateT gameLoop initialState
    putStrLn "Game over!"


-- TODO
gameLoop :: StateT BlackjackState IO ()
gameLoop = return ()