-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Run
-- Copyright   :  (c) Don Stewart 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  currently non-portable (Control.Concurrent)
--
-- Convenient interface to external processes 
--

{-
   LICENSE


Copyright :  (c) Galois, Inc. 2007

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.



-}

module Run (

        -- * Running processes
        readProcess

    ) where

import System.Process
import System.Exit
import System.IO

import Control.Monad
import Control.Concurrent
import qualified Control.Exception as C

--
-- | readProcess forks an external process, reads its standard output,
-- waits for the process to terminate, and returns either the output
-- string, or an exitcode.
--
readProcess :: FilePath                     -- ^ command to run
            -> [String]                     -- ^ any arguments
            -> String                       -- ^ standard input
            -> IO (Either ExitCode String)  -- ^ either the stdout, or an exitcode

readProcess cmd args input = C.handle (return . handler) $ do

    (inh,outh,errh,pid) <- runInteractiveProcess cmd args Nothing Nothing

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ hPutStr inh input
    hClose inh          -- done with stdin
    hClose errh         -- ignore stderr

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- C.catch (waitForProcess pid) (\_ -> return ExitSuccess)

    return $ case ex of
        ExitSuccess   -> Right output
        ExitFailure _ -> Left ex

  where
    handler (C.ExitException e) = Left e
    handler e                   = Left (ExitFailure 1)

