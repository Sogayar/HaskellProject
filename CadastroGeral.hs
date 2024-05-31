import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.List (intercalate)

-- Definição do tipo para Gênero
data Gender = Male | Female | Other deriving (Show, Read, Enum, Bounded)

-- Definição do tipo para Pessoa
data Person = Person
  { name :: String              -- Nome
  , age :: Int                  -- Idade
  , gender :: Gender            -- Gênero
  , address :: String           -- Endereço (Rua, Cidade, Estado, CEP)
  , email :: String             -- Email
  , tel :: String               -- Telefone
  } deriving (Show)

-- Definição de um tipo para armazenar registros de Pessoa
type PersonRecord = [Person]

-- Função principal
main :: IO ()
main = do
    putStrLn "Bem-vindo ao sistema de cadastro!"
    putStrLn "Por favor, insira os dados para o cadastro:" 	-- Fazer menu
    person <- capturePersonData
    putStrLn "Cadastro realizado com sucesso!"
    putStrLn "Obrigado por se cadastrar."

-- Função para capturar dados da Pessoa
capturePersonData :: IO Person
capturePersonData = do
    putStrLn "Nome Completo:"
    name <- getLine 		-- Captura nome e armazena em "name"
    putStrLn "Idade:"
    ageStr <- getLine		-- Captura idade e armazena em "age"
    let age = readAge ageStr
    putStrLn "Gênero (Male, Female, Other):"
    genderStr <- getLine	-- Captura o genero e armazena em "gender"
    let gender = readGender genderStr
    putStrLn "Endereço (Rua, Cidade, Estado, País):"
    address <- getLine		-- Captura endereço e armazena em "address"
    email <- promptWithValidation "Email:" validateEmail
    putStrLn "Telefone:"	-- Captura telefone e armazena em "tel"
    tel <- getLine
    return Person { name, age, gender, address, email, tel } -- Retorna a função "Person" com todos os parametros inseridos

-- Função para validar e retornar uma idade (número inteiro)
readAge :: String -> Int
readAge str = case readMaybe str of		-- Função "if" 
    Just age -> age
    Nothing -> error "Idade inválida! Por favor, insira um número inteiro."	-- Como se fosse "else"

-- Função para validar e retornar um gênero
readGender :: String -> Gender
readGender str = case map toLower str of	-- Passa a aceitar respostas com começo minúsculo, pois sempre colocará a primeira letra em maiúsculo
    "male" -> Male
    "female" -> Female
    "other" -> Other
    _ -> error "Gênero inválido! Por favor, insira Male, Female ou Other."

-- Função para validar e retornar um email
validateEmail :: String -> Either String String
validateEmail email
  | isValidEmail email = Right email
  | otherwise = Left "Email inválido! Por favor, insira um email válido."		-- "Else"

-- Função para verificar se um email possui um formato básico válido
isValidEmail :: String -> Bool
isValidEmail email = '@' `elem` email && '.' `elem` (dropWhile (/= '@') email)

-- Função para solicitar entrada do usuário com validação
promptWithValidation :: String -> (String -> Either String a) -> IO a
promptWithValidation text validate = do
    putStr text
    input <- getLine
    case validate input of
        Left errorMsg -> do
            putStrLn errorMsg
            promptWithValidation text validate
        Right value -> return value
