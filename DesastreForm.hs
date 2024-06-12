import Data.List (find)
import Data.Char (isDigit)

# As descrições das funções estão todas na documentação do código

data DisasterPerson = DisasterPerson {
    fullName :: String,
    age :: Int,
    gender :: String,
    address :: Address,
    disasterType :: String,
    disasterDate :: String,
    impactDescription :: String,
    immediateNeeds :: [String],
    insuranceDetails :: Maybe InsuranceDetails
} deriving (Show)

data Address = Address {
    street :: String,
    city :: String,
    state :: String,
    country :: String
} deriving (Show)

data InsuranceDetails = InsuranceDetails {
    insuranceType :: String,
    policyNumber :: String
} deriving (Show)

disasterTypes :: [String]
disasterTypes = ["Terremoto", "Inundação", "Furacão", "Incêndio", "Tornado", "Tsunami"]

genderOptions :: [String]
genderOptions = ["Masculino", "Feminino", "Outro"]

captureData :: IO DisasterPerson
captureData = do
    putStrLn "Digite o nome completo:"
    name <- getLine
    putStrLn "Digite a idade:"
    age <- readAge
    putStrLn "Selecione o gênero:"
    mapM_ putStrLn (zipWith (\i t -> show i ++ ". " ++ t) [1..] genderOptions)
    genderIndex <- readGenderIndex
    let gender = genderOptions !! (genderIndex - 1)
    address <- captureAddress
    putStrLn "Selecione o tipo de desastre natural:"
    mapM_ putStrLn (zipWith (\i t -> show i ++ ". " ++ t) [1..] disasterTypes)
    disasterTypeIndex <- readDisasterTypeIndex
    let disasterType = disasterTypes !! (disasterTypeIndex - 1)
    putStrLn "Digite a data do desastre (formato: DD/MM/AAAA):"
    disasterDate <- readDate
    putStrLn "Descreva o impacto pessoal do desastre:"
    impactDescription <- getLine
    putStrLn "Digite as necessidades imediatas (separadas por vírgula):"
    needs <- getLine
    putStrLn "Possui detalhes do seguro? (S/N)"
    hasInsurance <- getLine
    insurance <- if hasInsurance == "S" || hasInsurance == "s"
                    then Just <$> captureInsuranceDetails
                    else return Nothing
    return DisasterPerson {
        fullName = name,
        age = age,
        gender = gender,
        address = address,
        disasterType = disasterType,
        disasterDate = disasterDate,
        impactDescription = impactDescription,
        immediateNeeds = words needs,
        insuranceDetails = insurance
    }

readAge :: IO Int
readAge = do
    ageStr <- getLine
    case reads ageStr of
        [(age, "")] | age >= 0 -> return age
        _ -> do
            putStrLn "Idade inválida! Digite novamente:"
            readAge

readGenderIndex :: IO Int
readGenderIndex = do
    indexStr <- getLine
    case reads indexStr of
        [(index, "")] | index >= 1 && index <= length genderOptions -> return index
        _ -> do
            putStrLn "Opção inválida! Digite novamente:"
            readGenderIndex

readDisasterTypeIndex :: IO Int
readDisasterTypeIndex = do
    indexStr <- getLine
    case reads indexStr of
        [(index, "")] | index >= 1 && index <= length disasterTypes -> return index
        _ -> do
            putStrLn "Opção inválida! Digite novamente:"
            readDisasterTypeIndex

captureAddress :: IO Address
captureAddress = do
    putStrLn "Digite o nome da rua:"
    street <- getLine
    putStrLn "Digite a cidade:"
    city <- getLine
    putStrLn "Digite o estado:"
    state <- getLine
    putStrLn "Digite o país:"
    country <- getLine
    return Address {
        street = street,
        city = city,
        state = state,
        country = country
    }

captureInsuranceDetails :: IO InsuranceDetails
captureInsuranceDetails = do
    putStrLn "Digite o tipo de seguro:"
    insuranceType <- getLine
    putStrLn "Digite o número da apólice:"
    policyNumber <- getLine
    return InsuranceDetails {
        insuranceType = insuranceType,
        policyNumber = policyNumber
    }

validateData :: DisasterPerson -> Bool
validateData person = not (null (fullName person))

readDate :: IO String
readDate = do
    dateStr <- getLine
    if isValidDate dateStr
        then return dateStr
        else do
            putStrLn "Formato de data inválido! Digite novamente (DD/MM/AAAA):"
            readDate

isValidDate :: String -> Bool
isValidDate dateStr = length dateStr == 10 && all isDigit (take 2 dateStr) && dateStr !! 2 == '/' && all isDigit (take 2 (drop 3 dateStr)) && dateStr !! 5 == '/' && all isDigit (drop 6 dateStr)

type Registry = [DisasterPerson]

addRecord :: Registry -> DisasterPerson -> Registry
addRecord registry person = person : registry

listRecords :: Registry -> IO ()
listRecords registry = mapM_ printPerson registry
    where printPerson person = do
            putStrLn "---------------------------"
            putStrLn $ "Nome Completo: " ++ fullName person
            putStrLn $ "Idade: " ++ show (age person)
            putStrLn $ "Gênero: " ++ gender person
            putStrLn "Endereço:"
            putStrLn $ "Rua: " ++ street (address person)
            putStrLn $ "Cidade: " ++ city (address person)
            putStrLn $ "Estado: " ++ state (address person)
            putStrLn $ "País: " ++ country (address person)
            putStrLn $ "Tipo de Desastre: " ++ disasterType person
            putStrLn $ "Data do Desastre: " ++ disasterDate person
            putStrLn $ "Descrição do Impacto Pessoal: " ++ impactDescription person
            putStrLn $ "Necessidades Imediatas: " ++ unwords (immediateNeeds person)
            case insuranceDetails person of
                Just details -> do
                    putStrLn "Detalhes do Seguro:"
                    putStrLn $ "Tipo de Seguro: " ++ insuranceType details
                    putStrLn $ "Número da Apólice: " ++ policyNumber details
                Nothing -> putStrLn "Detalhes do Seguro: Não informado"

filterByDisasterType :: String -> Registry -> [DisasterPerson]
filterByDisasterType dType = filter (\person -> disasterType person == dType)

filterByDate :: String -> Registry -> [DisasterPerson]
filterByDate dDate = filter (\person -> disasterDate person == dDate)

filterByName :: String -> Registry -> [DisasterPerson]
filterByName name = filter (\person -> fullName person == name)

main :: IO ()
main = do
    putStrLn "Bem-vindo ao formulário de registro de desastres naturais!"
    registry <- menu []
    putStrLn "Encerrando o programa..."

menu :: Registry -> IO Registry
menu registry = do
    putStrLn "\nEscolha uma opção:"
    putStrLn "1. Adicionar novo registro"
    putStrLn "2. Listar todos os registros"
    putStrLn "3. Filtrar por tipo de desastre"
    putStrLn "4. Filtrar por data"
    putStrLn "5. Filtrar por nome"
    putStrLn "6. Sair"
    option <- getLine
    case option of
        "1" -> do
            person <- captureData
            if validateData person
                then do
                    putStrLn "Registro adicionado com sucesso!"
                    menu (addRecord registry person)
                else do
                    putStrLn "Erro: Informações inválidas! O registro não foi adicionado."
                    menu registry
        "2" -> do
            putStrLn "Lista de todos os registros:"
            listRecords registry
            menu registry
        "3" -> do
            putStrLn "Digite o tipo de desastre:"
            dType <- getLine
            let filteredByType = filterByDisasterType dType registry
            putStrLn "Registros encontrados:"
            listRecords filteredByType
            menu registry
        "4" -> do
            putStrLn "Digite a data do desastre (formato: DD/MM/AAAA):"
            dDate <- readDate
            let filteredByDate = filterByDate dDate registry
            putStrLn "Registros encontrados:"
            listRecords filteredByDate
            menu registry
        "5" -> do
            putStrLn "Digite o nome:"
            name <- getLine
            let filteredByName = filterByName name registry
            putStrLn "Registros encontrados:"
            listRecords filteredByName
            menu registry
        "6" -> return registry
        _ -> do
            putStrLn "Opção inválida! Por favor, escolha uma opção válida."
            menu registry
