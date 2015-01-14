{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Display where

import LSystem
import Graphics.Frame

import Graphics.Rendering.OpenGL

import Control.Applicative
import Control.Monad.Reader

data RenderState = RenderState { grammar :: Grammar
                               , currentFrame :: Frame
                               , symbols :: [Symbol]
                               , varFuncs :: [(Symbol, Frame -> IO Frame)]
                               , constantFuncs :: [(Symbol, Frame -> IO Frame)]
                               }

newtype Renderer a = Renderer { unRenderer :: ReaderT RenderState IO a }
    deriving (Functor, Applicative, Monad,
              MonadReader RenderState, MonadIO)

runRenderer :: Renderer a -> RenderState -> IO a
runRenderer r state = runReaderT (unRenderer r) state

renderGrammar :: Renderer ()
renderGrammar = do
    rule <- asks symbols
    case rule of
        [] -> return ()
        (s:ss) -> do
            g <- asks grammar
            fr <- asks currentFrame
            vf <- asks varFuncs
            cf <- asks constantFuncs
            case lookup s (if isVariable s g then vf else cf) of
                Nothing -> return ()
                Just f -> do
                    newFrame <- liftIO $ f fr
                    rs <- ask
                    let newRenderState = rs { currentFrame = newFrame
                                            , symbols = ss
                                            }
                    local (const newRenderState) renderGrammar


color3f :: Color3 GLfloat -> IO ()
color3f = color

vertex3f :: Vertex3 GLfloat -> IO ()
vertex3f = vertex

