# :elephant: Gummy Reminder :elephant:

## Descrição
Programa feito em SWI-Prolog para a disciplina de PLP.
Trata-se de um programa que usa repetição espaçada com flashcards para ajudar a memorizar conceitos.

## :hammer: Principais Funcionalidades
- `Criar decks`: cria um deck temático no qual serão inseridos cards com conteúdo relacionado ao tema.
- `Add cartas`: adiciona cards ao deck escolhido para o usuário revisar e testar o conteúdo memorizado.
- `Revisar`: o usuário inicia o estudo. Cartas são mostradas em ordem aleatória e cabe ao usuário responder corretamente o que está escrito nela.   

## :warning: Requisitos
Para rodar o programa, é necessário possuir o [SWI-Prolog](https://www.haskell.org/cabal/), o [Git](https://git-scm.com) e o [Node.js](https://nodejs.org/en/) instalados em sua máquina. A partir deles, você poderá
seguir a lista de comandos na seção abaixo.

### :hammer_and_wrench: Execução
Para executar o programa, siga a sequência de passos:

1º Clone o repositório Github que está localizado o projeto:

```
$ git clone https://github.com/joaoabrantesuchoa/gummy-reminder/
```

2º Entre na pasta do projeto em Prolog:

```sh
$ cd gummy-reminder/prolog
$ pwd
<caminho até a pasta atual...>/gummy-reminder/prolog
```

3º Rode o programa: 

```
$ npm start
```

4º Para fechar o programa realize as seguintes operações:

```ps1
1 ?-
[apertar ctrl+C]
Action (h for help) ? Deseja finalizar o arquivo em lotes (Y/N)?
[Primeiro, enviar a confirmação para "Deseja finalizar o arquivo em lotes (Y/N)", que seria "Y"+Enter]
[Depois, enviar a confirmação de sair para "Action (h for help) ?", que é "e"+Enter]
exit (status 4)
```


## :computer: Implementação
A implementação foi realizada em módulos, divindo os grandes blocos de funcionalidades em Controllers, de modo a facilitar o entendimento e manutenção
do código, e as informações guardados em um arquivo JSON, que serve de banco de dados. Segue os principais módulos:
* deckController: realiza as operações relacionadas aos Decks, como os métodos de adicionar, remover e editar.
* cardController: realiza as operações relacionadas ao Cards, como os métodos de adicionar, remover.
* JsonFunctions: comanda as operações relacionadas ao banco de dados, modificando o arquivo JSON de acordo com o que for feito.

## :technologist: Equipe
* Guilherme Peixoto ([guilhermefpeixoto](https://github.com/guilhermefpeixoto))
* João Gabriel Abrantes ([joaoabrantesuchoa](https://github.com/joaoabrantesuchoa))
* José Rufino ([joseaorufino](https://github.com/joseaorufino))
* Pedro Casado ([pgscasado](https://github.com/pgscasado))
* Pedro Henrique ([pedrohenrique-ql](https://github.com/pedrohenrique-ql))
