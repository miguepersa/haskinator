data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Tipo de datos para representar las respuestas
data Answer = Yes | No deriving (Show, Read, Eq)

-- Tipo de datos para las preguntas
type Question = String

-- Tipo de datos para un nodo en el árbol de preguntas
data QuestionNode = QuestionNode Question (Tree QuestionNode) (Tree QuestionNode) deriving (Show, Read, Eq)

-- Insertar una pregunta en el árbol
insertQuestion :: Question -> Tree QuestionNode -> Tree QuestionNode
insertQuestion q EmptyTree = Node (QuestionNode q EmptyTree EmptyTree) EmptyTree EmptyTree
insertQuestion q (Node node left right)
    | q == currentNode = Node node left right
    | q < currentNode  = Node node (insertQuestion q left) right
    | q > currentNode  = Node node left (insertQuestion q right)
    where currentNode = getQuestion node

-- Obtener la pregunta de un nodo
getQuestion :: QuestionNode -> Question
getQuestion (QuestionNode q _ _) = q

-- Responder a una pregunta y navegar por el árbol
answerQuestion :: Answer -> QuestionNode -> Tree QuestionNode
answerQuestion Yes (QuestionNode _ yes _) = yes
answerQuestion No (QuestionNode _ _ no) = no

