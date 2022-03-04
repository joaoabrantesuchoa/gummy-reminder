# :elephant: Gummy Reminder :elephant:
![Badge Concluído](http://img.shields.io/static/v1?label=STATUS&message=CONCLUÍDO&color=GREEN&style=for-the-badge)

## :1234: Índice

* [Descrição](#descrição)
* [Principais Funcionalidades](#principais-funcionalidades)
* [Requisitos e Execução](#requisitos)
* [Implementação](#implementação)
* [Equipe](#equipe)
## Descrição
Programa feito em Haskell para a disciplina de PLP.
Trata-se de um programa que usa repetição espaçada com flashcards para ajudar a memorizar conceitos.

## :hammer: Principais Funcionalidades
- `Criar decks`: cria um deck temático no qual serão inseridos cards com conteúdo relacionado ao tema.
- `Add cartas`: adiciona cards ao deck escolhido para o usuário revisar e testar o conteúdo memorizado.
- `Revisar`: o usuário inicia o estudo. Cartas são mostradas em ordem aleatória e cabe ao usuário responder corretamente o que está escrito nela.   

## :warning: Requisitos
Para rodar o programa, é necessário possuir o Cabal e o Git instalados em sua máquina. A partir deles, você poderá
seguir a lista de comandos listados na seção [Execução](Execução) listada a baixo.

### :hammer_and_wrench: Execução
Para executar o programa, siga a sequência de passos:

1º Clone o repositório Github que está localizado o projeto:

`git clone https://github.com/joaoabrantesuchoa/gummy-reminder/`

2º Configure o Cabal: 

`cabal update 
cabal install cabal-install 
cabal build` 

3º Rode o programa: 

`cabal run`

## Implementação
A implementação foi realizada em módulos, divindo os grandes blocos de funcionalidades em Controllers, de modo a facilitar o entendimento e manutenção
do código, e as informações guardados em um arquivo de texto, que serve de banco de dados. Segue os principais módulos:
* Card: representa uma carta, dividida em frente, em que o usuário visualiza o que deve ser respondido, e verso, que possui a resposta do que foi perguntado.
* Deck: o deck do tema escolhido, que possui um nome e cards relacionadas ao tema.
* DeckController: realiza as operações relacionadas ao módulo deck, como os métodos de adicionar, remover e editar.
* TxtController: comanda as operações relacionadas ao banco de dados, modificando o arquivo de texto de acordo com o que for feito.
* CLIController: adiciona funções extras para melhorar a experiência do usuário com a interface.

## :technologist: Equipe
* Guilherme Peixoto ([guilhermefpeixoto](https://github.com/guilhermefpeixoto))
* João Gabriel Abrantes ([joaoabrantesuchoa](https://github.com/joaoabrantesuchoa))
* José Rufino ([joseaorufino](https://github.com/joseaorufino))
* Pedro Casado ([pgscasado](https://github.com/pgscasado))
* Pedro Henrique Queiróz ([pedrohenrique-ql](https://github.com/pedrohenrique-ql))
