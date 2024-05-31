import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Char (toLower)

-- Definição do tipo para Gênero
data Gender = Male | Female | Other deriving (Show, Read, Enum, Bounded)

-- Definição do tipo para o Tipo de Desastre
data DisasterType = Earthquake | Flood | Hurricane | OtherDisaster deriving (Show, Read, Enum, Bounded)

-- Definição do tipo para Pessoa
data DisasterPerson = DisasterPerson
  { name :: String              -- Nome
  , age :: Int                  -- Idade
  , gender :: Gender            -- Gênero
  , address :: String           -- Endereço (Rua, Cidade, Estado, País)
  , disaster :: DisasterType    -- Tipo de Desastre
  , disasterDate :: String      -- Data do Desastre
  , impactDescription :: String -- Descrição do Impacto Pessoal
  , immediateNeeds :: [String]  -- Necessidades Imediatas
  , insuranceDetails :: String  -- Detalhes do Seguro
  } deriving (Show)

-- Definição do tipo para Abrigo
data Shelter = Shelter
  { nomeAbrigo :: String        -- Nome do Abrigo
  , cnpj :: String              -- CNPJ
  , contatoAbrigo :: String     -- Contato do Abrigo
  , qntAbrigados :: Int         -- Quantidade de abrigados
  , qntMax :: Int               -- Capacidade máxima do Abrigo
  , enderecoAbrigo :: String    -- Endereço do Abrigo
  , qntDoacao :: Double         -- Quantidade de doações recebidas
  , refeitorio :: Bool          -- Possui refeitório
  , metodoDoacao :: String      -- Método de doação
  , auxGovernoAbrigo :: Bool    -- Recebe algum auxílio do governo
  } deriving (Show)

-- Definição de um tipo para armazenar registros de Pessoa
type DisasterPersonRecord = [DisasterPerson]

-- Definição de um tipo para armazenar registros de Abrigo
type ShelterRecord = [Shelter]

-- Função principal
main :: IO ()
main = do
    putStrLn "Bem-vindo ao sistema de registro de desastres!"
    putStrLn "Por favor, insira os dados da pessoa afetada pelo desastre:"
    person <- capturePersonData
    putStrLn "Pessoa cadastrada com sucesso!"
    putStrLn "Agora, insira os dados do abrigo que está acolhendo vítimas do desastre:"
    putStrLn "Abrigo cadastrado com sucesso!"
    putStrLn "Obrigado por registrar as informações."

-- Função para capturar dados da Pessoa
capturePersonData :: IO DisasterPerson
capturePersonData = do
    putStrLn "Nome:"
    name <- getLine			-- Captura nome e armazena em "name"
    putStrLn "Idade:"
    ageStr <- getLine			-- Captura idade e armazena em "age"
    let age = read ageStr :: Int
    putStrLn "Gênero (Male, Female, Other):"
    genderStr <- getLine		-- Captura o genero e armazena em "gender"
    let gender = readGender genderStr
    putStrLn "Endereço (Rua, Cidade, Estado, País):"
    address <- getLine			-- Captura endereço e armazena em "address"
    putStrLn "Tipo de Desastre Natural (Earthquake, Flood, Hurricane, OtherDisaster):"
    disasterStr <- getLine		-- Captura qual o desastre e armazena em "disaster"
    let disaster = readDisasterType disasterStr
    putStrLn "Data do Desastre (AAAA-MM-DD):"
    disasterDate <- getLine		-- Captura qual a data do desastre e armazena em "disasterDate"
    putStrLn "Descrição do Impacto Pessoal:"
    impactDescription <- getLine	-- Captura qual o impacto pessoal e armazena em "impactDescription"
    putStrLn "Necessidades Imediatas (separadas por vírgula):"
    immediateNeedsStr <- getLine	-- Captura qual as necessidades imediatas e armazena em "immediateNeedsStr"
    let immediateNeeds = splitByComma immediateNeedsStr		-- Separando as palavras por vírgulas
    putStrLn "Detalhes do Seguro:"
    insuranceDetails <- getLine		-- Captura qual os detalhes do seguro e armazena em "insuranceDetails"
    return DisasterPerson { name, age, gender, address, disaster, disasterDate, impactDescription, immediateNeeds, insuranceDetails }	-- Retorna a função "DisasterPerson" com todos os parametros inseridos

-- Função para capturar dados do Abrigo
captureShelterData :: IO Shelter
captureShelterData = do
    putStrLn "Nome do Abrigo:"
    nomeAbrigo <- getLine
    putStrLn "CNPJ do Abrigo:"
    cnpj <- getLine
    putStrLn "Contato do Abrigo:"
    contatoAbrigo <- getLine
    putStrLn "Quantidade de abrigados:"
    qntAbrigadosStr <- getLine
    let qntAbrigados = read qntAbrigadosStr :: Int
    putStrLn "Capacidade máxima do Abrigo:"
    qntMaxStr <- getLine
    let qntMax = read qntMaxStr :: Int
    putStrLn "Endereço do Abrigo:"
    enderecoAbrigo <- getLine
    putStrLn "Quantidade de doações recebidas:"
    qntDoacaoStr <- getLine
    let qntDoacao = read qntDoacaoStr :: Double
    putStrLn "Possui refeitório? (True/False):"
    refeitorioStr <- getLine
    let refeitorio = read refeitorioStr :: Bool
    putStrLn "Método de doação:"
    metodoDoacao <- getLine
    putStrLn "Recebe algum auxílio do governo? (True/False):"
    auxGovernoAbrigoStr <- getLine
    let auxGovernoAbrigo = read auxGovernoAbrigoStr :: Bool
    return Shelter { nomeAbrigo, cnpj, contatoAbrigo, qntAbrigados, qntMax, enderecoAbrigo, qntDoacao, refeitorio, metodoDoacao, auxGovernoAbrigo }

-- Função para validar e retornar um gênero
readGender :: String -> Gender
readGender str = case map toLower str of
    "male" -> Male
    "female" -> Female
    "other" -> Other
    _ -> error "Gênero inválido! Por favor, insira Male, Female ou Other."

-- Função para validar e retornar um tipo de desastre
readDisasterType :: String -> DisasterType
readDisasterType str = case map toLower str of
    "earthquake" -> Earthquake
    "flood" -> Flood
    "hurricane" -> Hurricane
    "otherdisaster" -> OtherDisaster
    _ -> error "Tipo de desastre inválido! Por favor, insira Earthquake, Flood, Hurricane ou OtherDisaster."

-- Função para dividir uma string em uma lista de strings usando uma vírgula como delimitador
splitByComma :: String -> [String]
splitByComma = map (filter (/= ' ')) . split ','
  where split _ [] = []
        split delim str = let (before, remainder) = span (/= delim) str
                          in before : case remainder of
