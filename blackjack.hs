--BlackJack game where you test your luck to obtain 21 with only two cards
import System.Random

--deck creation seperated into hand value and card face values
data value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
Jack | Queen | King deriving(Show, Eq, Enum)
data Card = (value)

type Deck = [value]
type Hand = [value]

--4 sets of suits so Ace-King repeats 4 times for full deck of cards
deckOfCards :: Deck
deckOfCards = [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]++
              [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]++
              [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]++
              [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]

cardNum :: value -> [Int]
cardNum Ace = [1, 11]
cardNum Two = [2]
cardNum Three = [3]
cardNum Four = [4]
cardNum Five = [5]
cardNum Six = [6]
cardNum Seven = [7]
cardNum Eight = [8]
cardNum Nine = [9]
cardNum Ten = [10]
cardNum Jack = [10]
cardNum Queen = [10]
cardNum King = [10]

--players receives 2 cards for hand and those cards are taken out of the deck
deal :: Int -> Hand -> deckOfCards
deal = (take 2 deckOfCards, drop 2 deckOfCards)

--checking if initial hand is black jack, (Ace + (10 | Jack | Queen | King))
blackJack :: Hand -> bool
blackJack [firstCard, secondCard] = ((firstCard == Ace) && (secondCard == 10 || Jack || Queen || King)) ||
                                    ((secondCard == Ace) && (firstCard == 10 || Jack || Queen || King))
blackJack _ = False

--check if deal granted BlackJack
winner :: Hand -> Int -> Str
winner hand
  | blackJack = "Winner you got BlackJack!"
  | otherwise = "Sorry Try again."

--Gameplay interface start
main :: IO
main = do
  putStrLn "Welcome to BlackJack, enter p to play"
  winner <- getLine >>= readIO
  putStrLn "Thanks for playing!"
