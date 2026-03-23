import System.IO
import Data.Time

loadUniversity :: IO [University]
loadUniversity = do
    content <- readFile "University.txt"
    let uni = map read (lines content)
    length uni `seq` return uni

data Student = Student String String
    deriving (Show, Read)

data Record = Record String Int Int
    deriving (Show, Read)

data University = UStudent Student | URecord Record
    deriving (Show, Read)

getStudents :: [University] -> [Student]
getStudents uni = [s | UStudent s <- uni]

getRecords :: [University] -> [Record]
getRecords uni = [r | URecord r <- uni]

saveUniversity :: [University] -> IO ()
saveUniversity uni =
    writeFile "University.txt" (unlines (map show uni))

getCurrentMinutes :: IO Int
getCurrentMinutes = do
    utc <- getCurrentTime
    tz <- getCurrentTimeZone
    let local = utcToLocalTime tz utc
    let hourOfDay = localTimeOfDay local
    let h = todHour hourOfDay
    let m = todMin hourOfDay
    return (h * 60 + m)

formatHours :: Int -> String
formatHours mins = 
    let h = mins `div` 60
        m = mins `mod` 60
        hh = if h < 10 then "0" ++ show h else show h
        mm = if m < 10 then "0" ++ show m else show m
    in hh ++ ":" ++ mm

findName :: String -> [Student] -> String
findName sid students =
    case filter (\(Student id _) -> id == sid) students of
        (Student _ name):_ -> name
        _ -> "Desconocido"

calculateTime :: Record -> Int
calculateTime (Record _ entry exit) = exit - entry

showStudent :: Student -> String
showStudent (Student sid name) =
    "ID: " ++ sid ++ " | Nombre: " ++ name

showRecord :: [Student] -> Record -> String
showRecord students (Record sid entry exit)
    | exit == 0 =
        "Nombre: " ++ findName sid students ++
        " | ID: " ++ sid ++
        " | Entrada: " ++ formatHours entry ++
        " | Aún dentro de la universidad"
    | otherwise =   
        "Nombre: " ++ findName sid students ++
        " | ID: " ++ sid ++
        " | Entrada: " ++ formatHours entry ++
        " | Salida: " ++ formatHours exit ++
        " | Tiempo: " ++ show (calculateTime(Record sid entry exit)) ++ " minuto/s"

menu :: [University] -> IO ()
menu uni = do
    let students = getStudents uni
    let records = getRecords uni
    putStrLn "\nSistema de Registro de Estudiantes Universitarios"
    putStrLn "1. Registrar estudiante"
    putStrLn "2. Check In"
    putStrLn "3. Check Out"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Listar registro de todos los estudiantes"
    putStrLn "6. Buscar por ID de estudiante"
    putStrLn "7. Salir"
    putStrLn "\n"

    opcion <- getLine

    case opcion of

        "1" -> do
            putStrLn "ID: "
            sid <- getLine

            putStrLn "Nombre:"
            name <- getLine

            let existe = any (\(Student id _) -> id == sid) students

            if existe
            then do
                putStrLn "El estudiante ya existe"
                menu uni
            else do
                let newUni = UStudent (Student sid name) : uni
                saveUniversity newUni
                putStrLn "Estudiante registrado"
                menu newUni

        "2" -> do
            putStrLn "ID: "
            sid <- getLine

            let existe = any (\(Student id _) -> id == sid) students
            let active = any (\(Record id _ exit) -> id == sid && exit == 0) records

            if not existe 
            then do 
                putStrLn "Estudiante no existe"
                menu uni
            else if active
            then do
                putStrLn "El estudiante ya está dentro de la universidad"
                menu uni
            else do
                time <- getCurrentMinutes
                putStrLn ("Hora de entrada: " ++ formatHours time)

                let newUni = URecord (Record sid time 0) : uni 
                saveUniversity newUni
                putStrLn "Entrada registrada"
                menu newUni

        "3" -> do
            putStrLn "ID:"
            sid <- getLine

            time <- getCurrentMinutes
            putStrLn ("Hora de salida: " ++ formatHours time)

            let updateRecord x =
                    case x of
                        URecord (Record id entry exit) ->
                            if id == sid && exit == 0
                                then URecord (Record id entry time)
                                else x
                        _ -> x

            let updated = map updateRecord uni

            saveUniversity updated
            putStrLn "Salida registrada"
            menu updated
        
        "4" -> do
            putStrLn "\n--- Estudiantes ---"
            mapM_ (putStrLn . showStudent) students
            menu uni
        
        "5" -> do
            putStrLn "\n--- Registros ---"
            mapM_ (putStrLn . showRecord students) records
            menu uni
            
        "6" -> do
            putStrLn "Ingrese ID del estudiante a buscar:"
            sid <- getLine

            let student = filter (\(Student id _) -> id == sid) students
            let studentRecords = filter (\(Record id _ exit) -> id == sid) records

            if null student
                then putStrLn "No existe estudiante con ese ID"
                else if null studentRecords
                    then do 
                    putStrLn "Estudiante encontrado:"
                    mapM_ (putStrLn . showStudent) student
                    putStrLn "El o la estudiante no está dentro de la universidad"
                else do 
                putStrLn "\nRegistro:"
                let lastRecord = head studentRecords
                putStrLn(showRecord students lastRecord)
            menu uni
        
        "7" -> putStrLn "Saliendo del sistema"

        _ -> do
            putStrLn "Opción inválida"
            menu uni

main :: IO ()
main = do
    putStrLn "Cargando sistema..."
    uni <- loadUniversity
    menu uni