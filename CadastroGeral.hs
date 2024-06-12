import Text.Read (readMaybe)
import Data.Char (toLower)
import Text.Regex.Posix ((=~))

# As explicações das funções estão todas na documentação do projeto

data Person = Person { nomeCompleto :: String
                     , idade :: Int
                     , genero :: String
                     , endereco :: String
                     , email :: String
                     , telefone :: String
                     } deriving (Show)

capturarIdade :: IO Int
capturarIdade = do
    putStrLn "Digite a idade:"
    input <- getLine
    case readMaybe input of
        Just age | age >= 0 -> return age
        _ -> do
            putStrLn "Idade inválida. Por favor, digite novamente."
            capturarIdade

capturarGenero :: IO String
capturarGenero = do
    putStrLn "Digite o gênero:"
    input <- getLine
    let generoLower = map toLower input
    if generoLower `elem` ["masculino", "feminino", "outro"]
        then return generoLower
        else do
            putStrLn "Gênero inválido. Por favor, digite novamente."
            capturarGenero

capturarInfo :: String -> (String -> Bool) -> IO String
capturarInfo prompt validator = do
    putStrLn prompt
    input <- getLine
    if validator input
        then return input
        else do
            putStrLn "Entrada inválida. Por favor, digite novamente."
            capturarInfo prompt validator

validarEmail :: String -> Bool
validarEmail email = email =~ ("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$" :: String)

validarTelefone :: String -> Bool
validarTelefone telefone = length telefone == 9 && all (\c -> c >= '0' && c <= '9') telefone

capturarPessoa :: IO Person
capturarPessoa = do
    nome <- capturarInfo "Digite o nome completo:" (\_ -> True)
    idade <- capturarIdade
    genero <- capturarGenero
    endereco <- capturarInfo "Digite o endereço (Rua, Cidade, Estado, País):" (\_ -> True)
    email <- capturarInfo "Digite o email:" validarEmail
    telefone <- capturarInfo "Digite o telefone:" validarTelefone
    return $ Person nome idade genero endereco email telefone

type Registro = [Person]

listarRegistros :: Registro -> IO ()
listarRegistros registros = mapM_ print registros

main :: IO ()
main = do
    putStrLn "Cadastro Geral"
    putStrLn "Digite 'sair' a qualquer momento para encerrar o programa."
    registros <- capturarRegistros []
    putStrLn "Deseja listar todos os registros cadastrados? (sim/não)"
    listarOpcao <- getLine
    if map toLower listarOpcao == "sim"
        then listarRegistros registros
        else putStrLn "Programa encerrado."

capturarRegistros :: Registro -> IO Registro
capturarRegistros registros = do
    cadastrar <- capturarPessoa
    putStrLn "Registro cadastrado com sucesso!"
    putStrLn "Deseja cadastrar outra pessoa? (sim/não)"
    opcao <- getLine
    if map toLower opcao == "sim"
        then capturarRegistros (cadastrar:registros)
        else return (cadastrar:registros)
