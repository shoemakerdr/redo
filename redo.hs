import Control.Monad (filterM)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)



main :: IO ()
main = do
    args <- getArgs 
    mapM_ redo args


redo :: String -> IO ()
redo target = do
    path <- redoPath target
    case path of
        Nothing ->
            error $ "No .do file found for target `" ++ target ++ "`"

        Just path -> do
            let tmp = target ++ "---redoing"
                script = "sh " ++ path ++  " - - " ++ tmp ++ " > " ++ tmp
            print script
            (_, _, _, ph) <- createProcess $ 
                shell $ script
            exit <- waitForProcess ph
            case exit of
                ExitSuccess -> do
                    renameFile tmp target

                ExitFailure code -> do
                    hPutStrLn stderr $
                        "Redo script exited with non-zero exit code: " ++ show code
                    removeFile tmp


redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = do
    existingCandidates <- filterM doesFileExist candidates
    return $ safeHead existingCandidates
        where 
            candidates =
                [ target ++ ".do"] ++ 
                if hasExtension target then
                    [replaceBaseName target "default" ++ ".do" ]
                else
                    []
            safeHead [] = Nothing
            safeHead (x:_) = Just x
